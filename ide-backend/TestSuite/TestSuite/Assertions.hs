{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
-- | Convenience assertions
module TestSuite.Assertions (
    -- * General assertions
    collectErrors
  , assertSameSet
  , assertRaises
    -- * Assertions about session state
  , assertLoadedModules
    -- * Assertions about source code errors
  , assertNoErrors
  , assertOneError
  , assertSomeErrors
  , assertMoreErrors
  , assertSourceErrors
  , assertSourceErrors'
  , assertErrorOneOf
  , show3errors
    -- * Assertions about type information
  , assertIdInfo
  , assertIdInfo'
  , assertExpTypes
  , ignoreVersions
  , allVersions
  , from78
  , from710
  , assertUseSites
  , assertAlphaEquiv
    -- * Known problems
  , fixme
    -- * Auxiliary
  , isAsyncException
  , mkSpan
  ) where

import Prelude hiding (mod, span)
import Control.Monad
import Data.Char
import Data.Either
import Data.Function (on)
import Data.List hiding (span)
import Test.HUnit
import Test.HUnit.Lang
import Text.Regex (mkRegex, subRegex)
import qualified Control.Exception         as Ex
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.ByteString.UTF8      as S
import qualified Data.Text                 as T

import IdeSession
import TestTools

{-------------------------------------------------------------------------------
  General assertions
-------------------------------------------------------------------------------}

-- | Don't fail on the first assertion, but run all and collect all errors
collectErrors :: [Assertion] -> Assertion
collectErrors as = do
    es <- lefts `liftM` (sequence $ map Ex.try as)
    if null es
      then return ()
      else Ex.throwIO (concatFailures es)
  where
    concatFailures :: [HUnitFailure] -> HUnitFailure
    concatFailures = go []
      where
        go acc []                    = HUnitFailure (unlines . reverse $ acc)
        go acc (HUnitFailure e : es) = go (e : acc) es

-- | Compare two sets. The expected set is the _second_ set
-- (inconsistent with the rest of the assertions but more convenient)
assertSameSet :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertSameSet header = aux `on` sort
  where
    aux :: (Ord a, Show a) => [a] -> [a] -> Assertion
    aux actual expected =
      case diff expected actual of
        ([], []) ->
          return ()
        (missing, unexpected) ->
          assertFailure $ header
                       ++ "\nMissing: " ++ show missing
                       ++ "\nUnexpected: " ++ show unexpected

{-------------------------------------------------------------------------------
  Assertions about session state
-------------------------------------------------------------------------------}

assertLoadedModules :: IdeSession -> String -> [String] -> Assertion
assertLoadedModules session header goodMods = do
  loadedMods <- getLoadedModules session
  assertSameSet header (map T.pack goodMods) loadedMods

{-------------------------------------------------------------------------------
  Assertions about source code errors
-------------------------------------------------------------------------------}

assertNoErrors :: IdeSession -> Assertion
assertNoErrors session = do
  errs <- getSourceErrors session
  assertBool ("Unexpected errors: " ++ show3errors errs) $ null errs

assertOneError :: IdeSession -> Assertion
assertOneError session = do
  assertSomeErrors session
  msgs <- getSourceErrors session
  assertBool ("Too many type errors: " ++ show3errors msgs)
    $ length msgs <= 1

assertSomeErrors :: IdeSession -> Assertion
assertSomeErrors session = do
  msgs <- getSourceErrors session
  assertBool "An error was expected, but not found" $ length msgs >= 1

assertMoreErrors :: IdeSession -> Assertion
assertMoreErrors session = do
  msgs <- getSourceErrors session
  assertBool ("Too few type errors: " ++ show3errors msgs)
    $ length msgs >= 2

show3errors :: [SourceError] -> String
show3errors errs =
  let shown = intercalate "\n" (map show $ take 3 $ errs)
      more | length errs > 3 = "\n... and more ..."
           | otherwise       = ""
  in shown ++ more

-- @assertSourceErrors session [[a,b,c],[d,e,f],..] checks that there are
-- exactly as many errors as elements in the outer list, and each of those
-- errors must match one of the errors inside the inner lists
assertSourceErrors :: IdeSession -> [[(Maybe FilePath, String)]] -> Assertion
assertSourceErrors session expected = do
  errs <- getSourceErrors session
  if length errs /= length expected
    then assertFailure $ "Unexpected source errors: " ++ show3errors errs
    else forM_ (zip expected errs) $ \(potentialExpected, actualErr) ->
           assertErrorOneOf actualErr potentialExpected

assertSourceErrors' :: IdeSession -> [String] -> Assertion
assertSourceErrors' session = assertSourceErrors session . map
  (\err -> [(Nothing, err)])

assertErrorOneOf :: SourceError -> [(Maybe FilePath, String)] -> Assertion
assertErrorOneOf (SourceError _ loc actual) potentialExpected =
    case foldr1 mplus (map matches potentialExpected) of
      Left err -> assertFailure err
      Right () -> return ()
  where
    matches (mFP, expErr) = do
      matchesFilePath mFP
      matchesError expErr

    matchesFilePath Nothing = Right ()
    matchesFilePath (Just expectedPath) =
      case loc of
        ProperSpan (SourceSpan actualPath _ _ _ _) ->
          if expectedPath `isSuffixOf` actualPath
            then Right ()
            else Left "Wrong file"
        _ ->
          Left "Expected location"

    matchesError expectedErr =
      if ignoreQuotes expectedErr `isInfixOf` ignoreQuotes (T.unpack actual)
        then Right ()
        else Left $ "Unexpected error: " ++ show (T.unpack actual) ++ ".\nExpected: " ++ show expectedErr

{-------------------------------------------------------------------------------
  Assertions about type information
-------------------------------------------------------------------------------}

assertIdInfo :: IdeSession
             -> String                -- ^ Module
             -> (Int, Int, Int, Int)  -- ^ Location
             -> String                -- ^ Name
             -> IdNameSpace           -- ^ Namespace
             -> String                -- ^ Type
             -> String                -- ^ Defining module
             -> String                -- ^ Defining span
             -> String                -- ^ Home module
             -> String                -- ^ Scope
             -> Assertion
assertIdInfo session
             mod
             (frLine, frCol, toLine, toCol)
             expectedName
             expectedNameSpace
             expectedType
             expectedDefModule
             expectedDefSpan
             expectedHome
             expectedScope =
  assertIdInfo' session
                mod
                (frLine, frCol, toLine, toCol)
                (frLine, frCol, toLine, toCol)
                expectedName
                expectedNameSpace
                (case expectedType of "" -> []
                                      _  -> allVersions expectedType)
                (allVersions expectedDefModule)
                (allVersions expectedDefSpan)
                (allVersions expectedHome)
                (allVersions expectedScope)

-- | If no answer is specified for a given version, it will not be verified
type PerVersion a = [(GhcVersion, a)]

allVersions :: a -> PerVersion a
allVersions x = [(GHC_7_4, x), (GHC_7_8, x), (GHC_7_10, x)]

-- | One case for 7.4, and one for 7.8 and up
from78 :: a -> a -> PerVersion a
from78 x y = [(GHC_7_4, x), (GHC_7_8, y), (GHC_7_10, y)]

-- | One case for 7.4 and 7.8, and one for 7.10 and up
from710 :: a -> a -> PerVersion a
from710 x y = [(GHC_7_4, x), (GHC_7_8, x), (GHC_7_10, y)]

assertIdInfo' :: IdeSession
              -> String                -- ^ Module
              -> (Int, Int, Int, Int)  -- ^ Location
              -> (Int, Int, Int, Int)  -- ^ Precise location
              -> String                -- ^ Name
              -> IdNameSpace           -- ^ Namespace
              -> PerVersion String     -- ^ Type
              -> PerVersion String     -- ^ Defining module
              -> PerVersion String     -- ^ Defining span
              -> PerVersion String     -- ^ Home module
              -> PerVersion String     -- ^ Scope
              -> Assertion
assertIdInfo' session
              mod
              givenLocation
              expectedLocation
              expectedName
              expectedNameSpace
              expectedTypes
              expectedDefModules
              expectedDefSpans
              expectedHomes
              expectedScopes = do
    idInfo  <- getSpanInfo session
    version <- getGhcVersion session
    case idInfo (T.pack mod) givenSpan of
      (actualSpan, SpanId actualInfo) : _ -> compareIdInfo version actualSpan actualInfo
      (actualSpan, SpanQQ actualInfo) : _ -> compareIdInfo version actualSpan actualInfo
      _ -> assertFailure $ "No id info found for " ++ show expectedName
                        ++ " at " ++ show mod ++ ":" ++ show givenLocation
  where
    givenSpan, expectedSpan :: SourceSpan
    (_givenMod,    givenSpan)    = mkSpan mod givenLocation
    (_expectedMod, expectedSpan) = mkSpan mod expectedLocation

    compareIdInfo :: GhcVersion -> SourceSpan -> IdInfo -> Assertion
    compareIdInfo version actualSpan IdInfo{idProp = IdProp{..}, idScope} =
      collectErrors [
          assertEqual "name"      expectedName      (T.unpack idName)
        , assertEqual "location"  expectedSpan      actualSpan
        , assertEqual "namespace" expectedNameSpace idSpace

        , case lookup version expectedDefSpans of
            Nothing ->
              return ()
            Just expectedDefSpan ->
              assertEqual "def span" expectedDefSpan (show idDefSpan)

        , case lookup version expectedDefModules of
            Nothing ->
              return ()
            Just expectedDefModule ->
              assertEqual "def module" (ignoreVersions expectedDefModule)
                                       (ignoreVersions (show idDefinedIn))

        , case lookup version expectedScopes of
            Nothing            -> return ()
            Just expectedScope -> assertEqual "scope" (ignoreVersions expectedScope)
                                                      (ignoreVersions (show idScope))

        , case (lookup version expectedTypes, idType) of
            (Just expectedType, Just actualType) ->
              assertAlphaEquiv "type" expectedType (T.unpack actualType)
            (Just expectedType, Nothing) ->
              assertFailure $ "expected type " ++ expectedType ++ ", but got none"
            (Nothing, _) ->
              -- Not checking
              return ()

        , case (lookup version expectedHomes, idHomeModule) of
            (Just expectedHome, Nothing) ->
              assertEqual "home" expectedHome ""
            (Just expectedHome, Just actualHome) ->
              assertEqual "home" (ignoreVersions expectedHome)
                                 (ignoreVersions (show actualHome))
            (Nothing, _) ->
              -- Not checking
              return ()
        ]

assertExpTypes :: (ModuleName -> SourceSpan -> [(SourceSpan, T.Text)])
               -> String
               -> (Int, Int, Int, Int)
               -> [(Int, Int, Int, Int, String)]
               -> Assertion
assertExpTypes expTypes mod loc expected =
    assertAlphaEquiv "" expected actual
  where
    actual = flip map (uncurry expTypes $ mkSpan mod loc) $ \(span, typ) ->
      ( spanFromLine   span
      , spanFromColumn span
      , spanToLine     span
      , spanToColumn   span
      , T.unpack       typ
      )

assertUseSites :: (ModuleName -> SourceSpan -> [SourceSpan])
               -> String
               -> (Int, Int, Int, Int)
               -> String
               -> [String]
               -> Assertion
assertUseSites useSites mod loc symbol expected =
    assertEqual ("Use sites of `" ++ symbol ++ "` in " ++ show mod) expected actual
  where
    actual = map show (uncurry useSites $ mkSpan mod loc)

{------------------------------------------------------------------------------
  Replace (type variables) with numbered type variables

  i.e., change "b -> [c]" to "a1 -> [a2]"

  useful for comparing types for alpha-equivalence
------------------------------------------------------------------------------}

assertAlphaEquiv :: (IgnoreVarNames a, Eq a, Show a) => String -> a -> a -> Assertion
assertAlphaEquiv label a b =
  if ignoreVarNames a == ignoreVarNames b
    then return ()
    else assertFailure $ label ++ "\n"
                      ++ "expected: " ++ show a
                      ++ " but got: " ++ show b

class IgnoreVarNames a where
  ignoreVarNames :: a -> a

instance IgnoreVarNames a => IgnoreVarNames [a] where
  ignoreVarNames = map ignoreVarNames

instance IgnoreVarNames a => IgnoreVarNames (Int, Int, Int, Int, a) where
  ignoreVarNames (a, b, c, d, e) = (a, b, c, d, ignoreVarNames e)

instance IgnoreVarNames String where
  ignoreVarNames = unwords . go [] [] . tokenize
    where
      go :: [String] -> [String] -> [String] -> [String]
      go _vars acc [] = reverse acc
      go  vars acc (x:xs)
        | isVar x   = case elemIndex x vars of
                        Just n  -> go vars (var n : acc) xs
                        Nothing -> go (vars ++ [x]) acc (x : xs)
        | otherwise = go vars (x : acc) xs

      isVar :: String -> Bool
      isVar []    = False
      isVar (x:_) = isLower x

      var :: Int -> String
      var n = "a" ++ show n

-- | Repeatedly call lex
tokenize :: String -> [String]
tokenize [] = [[]]
tokenize xs = case lex xs of
                [(token, xs')] -> token : tokenize xs'
                _ -> error "tokenize failed"

{------------------------------------------------------------------------------
  Abstract away versions
------------------------------------------------------------------------------}

class IgnoreVersions a where
  ignoreVersions :: a -> a

instance IgnoreVersions String where
  ignoreVersions s = subRegex (mkRegex versionRegexp) s "X.Y.Z"
    where
      versionRegexp :: String
      versionRegexp = "[0-9]+(\\.[0-9]+)+(-[0-9a-z]+)?"

instance IgnoreVersions a => IgnoreVersions [a] where
  ignoreVersions = map ignoreVersions

instance IgnoreVersions a => IgnoreVersions (Maybe a) where
  ignoreVersions = fmap ignoreVersions

instance IgnoreVersions S.ByteString where
  ignoreVersions = S.fromString . ignoreVersions . S.toString

instance IgnoreVersions L.ByteString where
  ignoreVersions = L.fromString . ignoreVersions . L.toString

instance IgnoreVersions T.Text where
  ignoreVersions = T.pack . ignoreVersions . T.unpack

instance IgnoreVersions Import where
  ignoreVersions Import{..} = Import {
      importModule    = ignoreVersions importModule
    , importPackage   = importPackage
    , importQualified = importQualified
    , importImplicit  = importImplicit
    , importAs        = importAs
    , importEntities  = importEntities
    }

instance IgnoreVersions ModuleId where
  ignoreVersions ModuleId{..} = ModuleId {
      moduleName    = moduleName
    , modulePackage = ignoreVersions modulePackage
    }

-- From 7.10 and up the package key is a hash that includes the package version,
-- which is definitely not something we want to compare when we ignore versions.
-- So here we just replace the package key with the package name.
instance IgnoreVersions PackageId where
  ignoreVersions PackageId{..} = PackageId {
      packageName    = packageName
    , packageVersion = ignoreVersions packageVersion
    , packageKey     = packageName
    }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Compare two lists, both assumed sorted
--
-- @diff expected actual@ returns two lists:
--
-- * The first contains the elements in the first but not the second list
-- * The second contains the elements in the second but not the first list
--
-- If the first list is the "expected" list and the second list is the "actual"
-- list then the first list of the result are the "missing" elements and the
-- second list of the result are the "unexpected" elements.
-- or an empty lists if the input lists are identical
diff :: Ord a => [a] -> [a] -> ([a], [a])
diff [] [] = ([], [])
diff xs [] = (xs, [])
diff [] ys = ([], ys)
diff (x:xs) (y:ys)
  | x <  y    = let (missing, unexpected) = diff xs (y:ys)
                in (x:missing, unexpected)
  | x >  y    = let (missing, unexpected) = diff (x:xs) ys
                in (missing, y:unexpected)
  | otherwise = diff xs ys

-- | Replace everything that looks like a quote by a standard single quote.
ignoreQuotes :: String -> String
ignoreQuotes s = subRegex (mkRegex quoteRegexp) s "'"
  where
    quoteRegexp :: String
    quoteRegexp = "[‘‛’`\"]"

isAsyncException :: RunResult -> Bool
isAsyncException (RunProgException ex) =
     (ex == "AsyncException: user interrupt")
  || (ex == "SomeAsyncException: user interrupt")
isAsyncException _ = False

mkSpan :: String -> (Int, Int, Int, Int) -> (ModuleName, SourceSpan)
mkSpan mod (frLine, frCol, toLine, toCol) = (T.pack mod, span)
  where
    span = SourceSpan { spanFilePath   = mod ++ ".hs"
                      , spanFromLine   = frLine
                      , spanFromColumn = frCol
                      , spanToLine     = toLine
                      , spanToColumn   = toCol
                      }

{------------------------------------------------------------------------------
  Known problems
------------------------------------------------------------------------------}

-- | Which ghc versions are affected by these problems?
-- ([] if the bug is unrelated to the GHC version)
knownProblems :: [(String, [GhcVersion])]
knownProblems = [
    -- https://github.com/fpco/ide-backend/issues/32
    -- TODO: In 7.8 the error message does not include a filepath at all,
    -- so the error does not crop up. I don't know if this is true for _all_
    -- errors or just for this particular one (I tried a few but didn't see
    -- filepaths in any of them).
    ("#32", [GHC_7_4])
    -- https://github.com/fpco/ide-backend/issues/254
  , ("#254", [GHC_7_10])
  ]

fixme :: IdeSession -> String -> IO () -> IO String
fixme session bug io = do
  version <- getGhcVersion session
  let mAllAffected = lookup bug knownProblems
      isAffected   = case mAllAffected of
                       Nothing          -> False
                       Just allAffected -> null allAffected
                                        || version `elem` allAffected

  mErr <- Ex.catch (io >> return Nothing) $ \e ->
            case Ex.fromException e of
              Just (HUnitFailure err) -> return (Just err)
              Nothing -> return (Just $ show e)

  case mErr of
    Just err ->
      if isAffected
        then return "Expected failure"
        else Ex.throwIO . userError $ "Unexpected failure: " ++ err
    Nothing ->
      if isAffected
        then Ex.throwIO . userError $ "Unexpected success"
                                   ++ " (expected " ++ bug ++ ")"
        else return ""
