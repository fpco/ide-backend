{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IdeSession.Util (
    -- * Misc util
    showExWithClass
  , accessorName
  , lookup'
  , writeFileAtomic
    -- * Simple diffs
  , Diff(..)
  , applyMapDiff
  , suppressStdOutput
  , redirectStdOutput
  , restoreStdOutput
  ) where

import Data.Typeable (typeOf)
import qualified Control.Exception as Ex
import Data.Accessor (Accessor, accessor)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Tagged (Tagged, untag)
import Data.Digest.Pure.MD5 (MD5Digest, MD5Context)
import Data.Binary (Binary(..), getWord8, putWord8)
import Control.Applicative ((<$>))
import Crypto.Types (BitLength)
import Crypto.Classes (blockLength, initialCtx, updateCtx, finalize)
import System.FilePath (splitFileName, (<.>))
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.IO (Handle, hClose, openBinaryTempFile, hFlush, stdout)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import System.Posix (Fd)
import System.Posix.IO.ByteString
import qualified System.Posix.Files as Files
import qualified Data.ByteString.Char8 as BSSC (pack)

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

instance Binary Text where
  put = put . Text.encodeUtf8
  get = Text.decodeUtf8 <$> get

{------------------------------------------------------------------------------
  Simple diffs
------------------------------------------------------------------------------}

data Diff a = Keep | Remove | Insert a
  deriving (Show, Functor)

instance Binary a => Binary (Diff a) where
  put Keep       = putWord8 0
  put Remove     = putWord8 1
  put (Insert a) = putWord8 2 >> put a

  get = do
    header <- getWord8
    case header of
      0 -> return Keep
      1 -> return Remove
      2 -> Insert <$> get
      _ -> fail "Diff.get: invalid header"

applyMapDiff :: forall k v. Ord k
             => Strict (Map k) (Diff v)
             -> Strict (Map k) v -> Strict (Map k) v
applyMapDiff diff = foldr (.) id (map aux $ StrictMap.toList diff)
  where
    aux :: (k, Diff v) -> Strict (Map k) v -> Strict (Map k) v
    aux (_, Keep)     = id
    aux (k, Remove)   = StrictMap.delete k
    aux (k, Insert x) = StrictMap.insert k x

type StdOutputBackup = Fd

suppressStdOutput :: IO StdOutputBackup
suppressStdOutput = do
  hFlush stdout
  stdOutputBackup <- dup stdOutput
  closeFd stdOutput
  -- Will use next available file descriptor: that is, stdout.
  _ <- openFd (BSSC.pack "/dev/null") WriteOnly Nothing defaultFileFlags
  return stdOutputBackup

-- | Redirects stdout to a file, Creates the file, if needed.
redirectStdOutput :: FilePath -> IO StdOutputBackup
redirectStdOutput file = do
  hFlush stdout
  stdOutputBackup <- dup stdOutput
  closeFd stdOutput
  let mode = Files.unionFileModes Files.ownerReadMode Files.ownerWriteMode
  -- Will use next available file descriptor: that is, stdout.
  _ <- openFd (BSSC.pack file) WriteOnly (Just mode) defaultFileFlags
  return stdOutputBackup

restoreStdOutput :: StdOutputBackup -> IO ()
restoreStdOutput stdOutputBackup = do
  hFlush stdout
  closeFd stdOutput
  dup stdOutputBackup
  closeFd stdOutputBackup
