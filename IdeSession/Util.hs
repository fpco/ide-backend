{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DeriveFunctor, DeriveGeneric, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IdeSession.Util (
    -- * Misc util
    showExWithClass
  , accessorName
  , lookup'
  , envWithPathOverride
  , writeFileAtomic
  , setupEnv
  , relInclToOpts
  , parseProgressMessage
    -- * Simple diffs
  , Diff(..)
  , applyMapDiff
  , suppressStdOutput
  , redirectStdOutput
  , restoreStdOutput
  , suppressStdError
  , redirectStdError
  , restoreStdError
  ) where

import Control.Monad (void, forM_, mplus)
import Data.Typeable (typeOf)
import qualified Control.Exception as Ex
import Data.Accessor (Accessor, accessor)
import qualified Data.ByteString      as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Tagged (Tagged, untag)
import Data.Digest.Pure.MD5 (MD5Digest, MD5Context)
import Data.Binary (Binary(..))
import qualified Data.Binary                  as Bin
import qualified Data.Binary.Get.Internal     as Bin (readNWith)
import qualified Data.Binary.Put              as Bin (putBuilder)
import qualified Data.Binary.Builder.Internal as Bin (writeN)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (castPtr)
import Control.Applicative ((<$>))
import Crypto.Types (BitLength)
import Crypto.Classes (blockLength, initialCtx, updateCtx, finalize)
import System.FilePath (splitFileName, (<.>), (</>))
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.IO (Handle, hClose, openBinaryTempFile, hFlush, stdout, stderr)
import Data.Char (isSpace)
import qualified Data.Attoparsec.Text as Att
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import System.Environment (getEnvironment)
import System.FilePath (splitSearchPath, searchPathSeparator)
import System.Posix (Fd)
import System.Posix.IO.ByteString
import System.Posix.Types (CPid(..))
import qualified System.Posix.Files as Files
import qualified Data.ByteString.Char8 as BSSC (pack)
import System.Posix.Env (setEnv, unsetEnv)
import GHC.Generics (Generic)
import Text.Show.Pretty

import IdeSession.Strict.Container
import qualified IdeSession.Strict.Map as StrictMap

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

-- | Show an exception together with its most precise type tag.
showExWithClass :: Ex.SomeException -> String
showExWithClass (Ex.SomeException ex) = show (typeOf ex) ++ ": " ++ show ex

-- | Translate record field '_name' to the accessor 'name'
accessorName :: String -> Maybe String
accessorName ('_' : str) = Just str
accessorName _           = Nothing

-- | Prelude.lookup as an accessor
lookup' :: Eq a => a -> Accessor [(a, b)] (Maybe b)
lookup' key =
    accessor (lookup key) $ \mVal list ->
      case mVal of
        Nothing  -> delete key list
        Just val -> override key val list
  where
    override :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
    override a b [] = [(a, b)]
    override a b ((a', b') : xs)
      | a == a'   = (a, b) : xs
      | otherwise = (a', b') : override a b xs

    delete :: Eq a => a -> [(a, b)] -> [(a, b)]
    delete _ [] = []
    delete a ((a', b') : xs)
      | a == a'   = xs
      | otherwise = (a', b') : delete a xs

envWithPathOverride :: [FilePath] -> IO (Maybe [(String, String)])
envWithPathOverride []            = return Nothing
envWithPathOverride extraPathDirs = do
    env <- getEnvironment
    let path  = fromMaybe "" (lookup "PATH" env)
        path' = intercalate [searchPathSeparator]
                  (extraPathDirs ++ splitSearchPath path)
        env'  = ("PATH", path') : filter (\(var, _) -> var /= "PATH") env
    return (Just env')

-- | Writes a file atomically.
--
-- The file is either written successfully or an IO exception is raised and
-- the original file is left unchanged.
--
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
-- Returns the hash of the file; we are careful not to force the entire input
-- bytestring into memory (we compute the hash as we write the file).
writeFileAtomic :: FilePath -> BSL.ByteString -> IO MD5Digest
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  createDirectoryIfMissing True targetDir
  Ex.bracketOnError
    (openBinaryTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        let bits :: Tagged MD5Digest BitLength ; bits = blockLength
        hash <- go handle initialCtx $ makeBlocks (untag bits `div` 8) content
        hClose handle
        renameFile tmpPath targetPath
        return hash)
  where
    go :: Handle -> MD5Context -> [BSS.ByteString] -> IO MD5Digest
    go _ _   []       = error "Bug in makeBlocks"
    go h ctx [bs]     = BSS.hPut h bs >> return (finalize ctx bs)
    go h ctx (bs:bss) = BSS.hPut h bs >> go h (updateCtx ctx bs) bss

-- | @makeBlocks n@ splits a bytestring into blocks with a size that is a
-- multiple of 'n', with one left-over smaller bytestring at the end.
--
-- Based from the (unexported) 'makeBlocks' in the crypto-api package, but
-- we are careful to be as lazy as possible (the first -- block can be returned
-- before the entire input bytestring is forced)
makeBlocks :: Int -> BSL.ByteString -> [BSS.ByteString]
makeBlocks n = go . BSL.toChunks
  where
    go [] = [BSS.empty]
    go (bs:bss)
      | BSS.length bs >= n =
          let l = BSS.length bs - (BSS.length bs `rem` n)
              (bsInit, bsTail) = BSS.splitAt l bs
          in bsInit : go (bsTail : bss)
      | otherwise =
          case bss of
            []         -> [bs]
            (bs':bss') -> go (BSS.append bs bs' : bss')

-- | First restore the environment to the specified initial environment, then
-- apply the given overrides
setupEnv :: [(String, String)] -> [(String, Maybe String)] -> IO ()
setupEnv initEnv overrides = do
  -- Delete everything in the current environment
  curEnv <- getEnvironment
  forM_ curEnv $ \(var, _val) -> unsetEnv var

  -- Restore initial environment
  forM_ initEnv $ \(var, val) -> setEnv var val True

  -- Apply overrides
  forM_ overrides $ \(var, mVal) ->
    case mVal of
      Just val -> setEnv var val True
      Nothing  -> unsetEnv var

relInclToOpts :: FilePath -> [FilePath] -> [String]
relInclToOpts sourcesDir relIncl =
   ["-i"]  -- reset to empty
   ++ map (\path -> "-i" ++ sourcesDir </> path) relIncl

parseProgressMessage :: Text -> Either String (Int, Int, Text)
parseProgressMessage = Att.parseOnly parser
  where
    parser :: Att.Parser (Int, Int, Text)
    parser = do
      _    <- Att.char '['                ; Att.skipSpace
      step <- Att.decimal                 ; Att.skipSpace
      _    <- Att.string (Text.pack "of") ; Att.skipSpace
      numS <- Att.decimal                 ; Att.skipSpace
      _    <- Att.char ']'                ; Att.skipSpace
      rest <- parseCompiling `mplus` Att.takeText
      return (step, numS, rest)

    parseCompiling :: Att.Parser Text
    parseCompiling = do
      compiling <- Att.string (Text.pack "Compiling") ; Att.skipSpace
      _         <- parseTH                            ; Att.skipSpace
      modName   <- Att.takeTill isSpace
      return $ Text.concat [compiling, Text.pack " ", modName]

    parseTH :: Att.Parser ()
    parseTH = Att.option () $ void $ Att.string (Text.pack "[TH]")

{------------------------------------------------------------------------------
  Simple diffs
------------------------------------------------------------------------------}

data Diff a = Keep | Remove | Insert a
  deriving (Show, Functor, Generic)

instance Binary a => Binary (Diff a) where
  put Keep       = Bin.putWord8 0
  put Remove     = Bin.putWord8 1
  put (Insert a) = Bin.putWord8 2 >> Bin.put a

  get = do
    header <- Bin.getWord8
    case header of
      0 -> return Keep
      1 -> return Remove
      2 -> Insert <$> Bin.get
      _ -> fail "Diff.get: invalid header"

instance PrettyVal a => PrettyVal (Diff a) -- relies on Generics

applyMapDiff :: forall k v. Ord k
             => Strict (Map k) (Diff v)
             -> Strict (Map k) v -> Strict (Map k) v
applyMapDiff diff = foldr (.) id (map aux $ StrictMap.toList diff)
  where
    aux :: (k, Diff v) -> Strict (Map k) v -> Strict (Map k) v
    aux (_, Keep)     = id
    aux (k, Remove)   = StrictMap.delete k
    aux (k, Insert x) = StrictMap.insert k x

-- Manipulations with stdout and stderr.

type FdBackup = Fd

suppressStdOutput :: IO FdBackup
suppressStdOutput = suppressHandle stdout stdOutput

redirectStdOutput :: FilePath -> IO FdBackup
redirectStdOutput = redirectHandle stdout stdOutput

restoreStdOutput :: FdBackup -> IO ()
restoreStdOutput = restoreHandle stdout stdOutput

suppressStdError :: IO FdBackup
suppressStdError = suppressHandle stderr stdError

redirectStdError :: FilePath -> IO FdBackup
redirectStdError = redirectHandle stderr stdError

restoreStdError :: FdBackup -> IO ()
restoreStdError = restoreHandle stderr stdError

suppressHandle :: Handle -> Fd -> IO FdBackup
suppressHandle h fd = do
  hFlush h
  fdBackup <- dup fd
  closeFd fd
  -- Will use next available file descriptor: that of h, e.g., stdout.
  _ <- openFd (BSSC.pack "/dev/null") WriteOnly Nothing defaultFileFlags
  return fdBackup

-- | Redirects a handle (e.g., stdout) to a file, Creates the file, if needed,
-- just truncates, if not.
redirectHandle :: Handle -> Fd -> FilePath -> IO FdBackup
redirectHandle h fd file = do
  let mode = Files.unionFileModes Files.ownerReadMode Files.ownerWriteMode
      fileBS = BSSC.pack file
  -- The file can't be created down there in openFd, because then
  -- a wrong fd gets captured.
  void $ createFile fileBS mode
  hFlush h
  fdBackup <- dup fd
  closeFd fd
  -- Will use next available file descriptor: that of h, e.g., stdout.
  _ <- openFd fileBS WriteOnly Nothing defaultFileFlags
  return fdBackup

restoreHandle :: Handle -> Fd -> FdBackup -> IO ()
restoreHandle h fd fdBackup = do
  hFlush h
  closeFd fd
  _ <- dup fdBackup
  closeFd fdBackup

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Binary Text where
  get   = do units <- Bin.get
             Bin.readNWith (units * 2) $ \ptr ->
               Text.fromPtr (castPtr ptr) (fromIntegral units)

  put t = do put (Text.lengthWord16 t)
             Bin.putBuilder $
               Bin.writeN (Text.lengthWord16 t * 2)
                          (\p -> Text.unsafeCopyToPtr t (castPtr p))

deriving instance Binary CPid
