{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
-- | Convenience assertions
module TestSuite.Assertions (
    -- * General assertions
    collectErrors
    -- * Assertions about source code errors
  , assertNoErrors
    -- * Assertions about type information
  , assertIdInfo
  , assertIdInfo'
  , assertExpTypes
  , ignoreVersions
  , allVersions
  ) where

import Prelude hiding (mod, span)
import Control.Monad
import Data.Char
import Data.Either
import Data.List (elemIndex, intercalate)
import Test.HUnit
import Test.HUnit.Lang
import Text.Regex (mkRegex, subRegex)
import qualified Control.Exception         as Ex
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.ByteString.UTF8      as S
import qualified Data.Text                 as T

import IdeSession

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

{-------------------------------------------------------------------------------
  Assertions about source code errors
-------------------------------------------------------------------------------}

assertNoErrors :: IdeSession -> Assertion
assertNoErrors session = do
  errs <- getSourceErrors session
  assertBool ("Unexpected errors: " ++ show3errors errs) $ null errs

show3errors :: [SourceError] -> String
show3errors errs =
  let shown = intercalate "\n" (map show $ take 3 $ errs)
      more | length errs > 3 = "\n... and more ..."
           | otherwise       = ""
  in shown ++ more

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
                expectedDefModule
                (allVersions expectedDefSpan)
                expectedHome
                (allVersions expectedScope)

-- | If no answer is specified for a given version, it will not be verified
type PerVersion a = [(GhcVersion, a)]

allVersions :: a -> PerVersion a
allVersions x = [(GHC742, x), (GHC78, x)]

assertIdInfo' :: IdeSession
              -> String                -- ^ Module
              -> (Int, Int, Int, Int)  -- ^ Location
              -> (Int, Int, Int, Int)  -- ^ Precise location
              -> String                -- ^ Name
              -> IdNameSpace           -- ^ Namespace
              -> PerVersion String     -- ^ Type
              -> String                -- ^ Defining module
              -> PerVersion String     -- ^ Defining span
              -> String                -- ^ Home module
              -> PerVersion String     -- ^ Scope
              -> Assertion
assertIdInfo' session
              mod
              givenLocation
              expectedLocation
              expectedName
              expectedNameSpace
              expectedTypes
              expectedDefModule
              expectedDefSpans
              expectedHome
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
            Nothing              -> return ()
            Just expectedDefSpan -> assertEqual "def span" expectedDefSpan
                                                           (show idDefSpan)

        , assertEqual "def module" (ignoreVersions expectedDefModule)
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

        , case idHomeModule of
            Nothing         -> assertEqual "home" expectedHome ""
            Just actualHome -> assertEqual "home" (ignoreVersions expectedHome)
                                                  (ignoreVersions (show actualHome))
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
      versionRegexp = "[0-9]+(\\.[0-9]+)+"

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

instance IgnoreVersions PackageId where
  ignoreVersions PackageId{..} = PackageId {
      packageName    = packageName
    , packageVersion = ignoreVersions packageVersion
    }

