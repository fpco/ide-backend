module Main (main) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 (pack)
import Data.Time
import Data.IORef
import System.Random
import System.IO
import System.IO.Unsafe

import IdeSession

type Identifier = String
type Type       = String

newtype Program      = Program [BindingGroup]
newtype BindingGroup = BindingGroup [(Identifier, Maybe Type, Expression)]

data Expression = Print String
                | Seq Expression Expression
                | Var Identifier

instance Show Program where
  show (Program bgs) = unlines (map show bgs)

instance Show BindingGroup where
  show (BindingGroup bindings) = unlines (map showBinding bindings)
    where
      showBinding (id, mtyp, expr) = showTypeAnn (id, mtyp)
                                  ++ id ++ " = " ++ show expr

      showTypeAnn (id, Nothing)  = ""
      showTypeAnn (id, Just typ) = id ++ " :: " ++ typ ++ "\n"

instance Show Expression where
  show (Print str) = "putStr " ++ str
  show (Seq e1 e2) = show e1 ++ " >> " ++ show e2
  show (Var id)    = id

nextId :: IORef Int
{-# NOINLINE nextId #-}
nextId = unsafePerformIO $ newIORef 0

mkId :: IO Identifier
mkId = do
  cnt <- readIORef nextId
  writeIORef nextId (cnt + 1)
  return $ "id" ++ show cnt

randomProgram :: IO String
randomProgram = show <$> mkProg
  where
    mkProg :: IO Program
    mkProg = do
      numBGs <- randomRIO (1, 100)
      Program <$> replicateM numBGs mkBG

    mkBG :: IO BindingGroup
    mkBG = do
      numBindings <- randomRIO (1, 3)
      ids <- replicateM numBindings mkId
      typ <- replicateM numBindings randomIO
      BindingGroup <$> mapM (mkBinding ids) (zip ids typ)

    mkBinding :: [Identifier] -> (Identifier, Bool) -> IO (Identifier, Maybe Type, Expression)
    mkBinding [_] (id, showTyp) =
      return (id, mkTyp showTyp "IO ()", Print "hi")
    mkBinding is (id, showTyp) = do
      id <- (is !!) <$> randomRIO (0, length is - 1)
      return (id, mkTyp showTyp "IO ()", Var id `Seq` Print "hi")

    mkTyp :: Bool -> Type -> Maybe Type
    mkTyp False _  = Nothing
    mkTyp True typ = Just typ



main :: IO ()
main = do
    session <- initSession defaultSessionConfig

    updateSession session (updateCodeGeneration True) ignoreProgress

    replicateM 1000 $ do
      prog <- randomProgram
      before <- getCurrentTime
      updateSession session (updateSourceFile "Main.hs" (pack prog)) ignoreProgress
      after <- getCurrentTime
      let latency = (1e6 :: Double) * realToFrac (diffUTCTime after before)
      hPutStrLn stderr $ show (length prog) ++ " " ++ show latency

    shutdownSession session
  where
    ignoreProgress :: Progress -> IO ()
    ignoreProgress _ = return ()


