{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, MultiParamTypeClasses, GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-orphans #-}
module GhcShim.GhcShim742
  ( -- * Pretty-printing
    showSDoc
  , pretty
  , prettyM
  , prettyType
  , prettyTypeM
    -- * Errors
  , sourceErrorSpan
    -- * Breakpoints
  , getBreak
  , setBreak
    -- * Time
  , GhcTime
    -- * Setup
  , ghcGetVersion
  , packageDBFlags
  , setGhcOptions
  , storeDynFlags
    -- * Package keys (see GhcShim.hs).
  , PackageKey
  , PackageQualifier
  , lookupPackage
  , mainPackageKey
  , modulePackageKey
  , packageKeyString
  , stringToPackageKey
  , packageKeyToSourceId
  , findExposedModule
    -- * Folding
  , AstAlg(..)
  , fold
    -- * Operations on types
  , typeOfTyThing
    -- * Re-exports
  , tidyOpenType
  ) where

import Prelude hiding (id, span)
import Control.Monad (void, forM_, liftM)
import Data.IORef
import Data.Version
import System.IO.Unsafe (unsafePerformIO)
import System.Time (ClockTime)
import qualified Data.Maybe as Maybe

import Bag
import BasicTypes hiding (Version)
import DataCon
import DynFlags
import ErrUtils
import FastString
import GHC hiding (getBreak)
import GhcMonad
import Linker
import Module
import MonadUtils
import Outputable hiding (showSDoc)
import PackageConfig (PackageConfig)
import Pair
import PprTyThing
import Pretty
import SrcLoc
import TcEvidence
import TcHsSyn
import TcType
import Type
import TysWiredIn
import qualified BreakArray
import qualified Packages

import qualified Distribution.Package              as Cabal
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Text                 as Cabal
import qualified Distribution.Compat.ReadP         as Cabal

import GhcShim.API
import IdeSession.GHC.API (GhcVersion(..))

{------------------------------------------------------------------------------
  Pretty-printing
------------------------------------------------------------------------------}

showSDoc :: DynFlags -> PprStyle -> SDoc -> String
showSDoc _dflags pprStyle doc =
    showDocWith OneLineMode
  $ runSDoc doc
  $ initSDocContext pprStyle

pretty :: Outputable a => DynFlags -> PprStyle -> a -> String
pretty dynFlags pprStyle = showSDoc dynFlags pprStyle . ppr

prettyType :: DynFlags -> PprStyle -> Bool -> Type -> String
prettyType dynFlags pprStyle showForalls typ =
  showSDoc dynFlags pprStyle (pprTypeForUser showForalls typ)

prettyM :: (Outputable a, Monad m, HasDynFlags m) => PprStyle -> a -> m String
prettyM pprStyle x = do
  dynFlags <- getDynFlags
  return (pretty dynFlags pprStyle  x)

prettyTypeM :: (Monad m, HasDynFlags m) => PprStyle -> Bool -> Type -> m String
prettyTypeM pprStyle showForalls typ = do
  dynFlags <- getDynFlags
  return $ prettyType dynFlags pprStyle showForalls typ

{------------------------------------------------------------------------------
  Show instances
------------------------------------------------------------------------------}

deriving instance Show Severity

{------------------------------------------------------------------------------
  Source errors
------------------------------------------------------------------------------}

sourceErrorSpan :: ErrMsg -> Maybe SrcSpan
sourceErrorSpan errMsg = case errMsgSpans errMsg of
  [real@RealSrcSpan{}] -> Just real
  _                    -> Nothing

{------------------------------------------------------------------------------
  Breakpoints
------------------------------------------------------------------------------}

getBreak :: BreakArray -> Int -> Ghc (Maybe Bool)
getBreak array index = do
  val    <- liftIO $ BreakArray.getBreak array index
  return ((== 1) `liftM` val)

setBreak :: BreakArray -> Int -> Bool -> Ghc ()
setBreak array index value = do
  void . liftIO $ if value then BreakArray.setBreakOn  array index
                           else BreakArray.setBreakOff array index

{------------------------------------------------------------------------------
  Time
------------------------------------------------------------------------------}

type GhcTime = ClockTime

{------------------------------------------------------------------------------
  Setup
------------------------------------------------------------------------------}

ghcGetVersion :: GhcVersion
ghcGetVersion = GHC_7_4

packageDBFlags :: Bool -> [String] -> [String]
packageDBFlags userDB specificDBs =
     ["-no-user-package-conf" | not userDB]
  ++ concat [["-package-conf", db] | db <- specificDBs]

-- | Set GHC options
--
-- This is meant to be stateless. It is important to call storeDynFlags at least
-- once before calling setGhcOptions so that we know what state to restore to
-- before setting the options.
--
-- Returns unrecognized options and warnings
setGhcOptions :: [String] -> Ghc ([String], [String])
setGhcOptions opts = do
  dflags <- restoreDynFlags
  (dflags', leftover, warnings) <- parseDynamicFlags dflags (map noLoc opts)
  setupLinkerState =<< setSessionDynFlags dflags'
  return (map unLoc leftover, map unLoc warnings)

-- | Setup linker state to deal with changed package flags
--
-- This follows newDynFlags in ghci
setupLinkerState :: [PackageId] -> Ghc ()
setupLinkerState newPackages = do
  dflags <- getSessionDynFlags
  setTargets []
  load LoadAllTargets
  liftIO $ linkPackages dflags newPackages

{------------------------------------------------------------------------------
  Backup DynFlags

  Sadly, this hardcodes quite a bit of version-specific information about ghc's
  inner workings. Unfortunately, there is no easy way to know which parts of
  DynFlags should and should not be restored to restore flags. The flag
  specification is given by (see packageDynamicFlags in compiler/main/GHC.hs)

  > package_flags ++ dynamic_flags

  both of which are defined in DynFlags.hs. They are not exported, but this
  would not be particularly useful anyway, as the action associated with a
  flag is given by a shallow embedding, so we cannot walk over them and extract
  the necessary info about DynFlags. At least, we cannot do that in code -- we
  can do it manually, and that is precisely what I've done to obtain the list
  below. Of course, this means it's somewhat error prone.

  In order so that this code can be audited and cross-checked against the
  actual ghc version, and so that it can be modified for future ghc versions,
  we don't just list the end result if this manual traversal, but document the
  process.

  Each of the command line options are defined in terms of a auxiliary
  functions that specify their effect on DynFlags. These auxiliary functions
  are listed below, along with which parts of DynFlags they modify:

  > FUNCTION                   MODIFIES FIELD(s) OF DYNFLAGS
  > ----------------------------------------------------------------------------
  > addCmdlineFramework        cmdlineFrameworks
  > addCmdlineHCInclude        cmdlineHcIncludes
  > addDepExcludeMod           depExcludeMods
  > addDepSuffix               depSuffixes
  > addFrameworkPath           frameworkPaths
  > addHaddockOpts             haddockOptions
  > addImportPath              importPaths
  > addIncludePath             includePaths
  > addLibraryPath             libraryPaths
  > addOptP                    settings
  > addOptl                    settings
  > addPluginModuleName        pluginModNames
  > addPluginModuleNameOption  pluginModNameOpts
  > alterSettings              settings
  > disableGlasgowExts         flags, extensions, extensionFlags
  > enableGlasgowExts          flags, extensions, extensionFlags
  > exposePackage              packageFlags
  > exposePackageId            packageFlags
  > extraPkgConf_              extraPkgConfs
  > forceRecompile             flags
  > hidePackage                packageFlags
  > ignorePackage              packageFlags
  > parseDynLibLoaderMode      dynLibLoader
  > setDPHOpt                  optLevel, flags, maxSimplIterations, simplPhases
  > setDepIncludePkgDeps       depIncludePkgDeps
  > setDepMakefile             depMakefile
  > setDumpDir                 dumpDir
  > setDumpFlag                flags
  > setDumpFlag'               flags
  > setDumpPrefixForce         dumpPrefixForce
  > setDumpSimplPhases         flags, shouldDumpSimplPhase
  > setDylibInstallName        dylibInstallName
  > setDynFlag                 flags
  > setExtensionFlag           extensions, extensionFlags
  > setHcSuf                   hcSuf
  > setHiDir                   hiDir
  > setHiSuf                   hiSuf
  > setLanguage                language, extensionFlags
  > setMainIs                  mainFunIs, mainModIs
  > setObjTarget               hscTarget
  > setObjectDir               objectDir
  > setObjectSuf               objectSuf
  > setOptHpcDir               hpcDir
  > setOptLevel                optLevel, flags
  > setOutputDir               objectDir, hiDir, stubDir, includePaths, dumpDir
  > setOutputFile              outputFile
  > setOutputHi                outputHi
  > setPackageName             thisPackage
  > setPackageTrust            flags, pkgTrustOnLoc
  > setPgmP                    settings
  > setRtsOpts                 rtsOpts
  > setRtsOptsEnabled          rtsOptsEnabled
  > setSafeHaskell             safeHaskell
  > setStubDir                 stubDir
  > setTarget                  hscTarget
  > setTmpDir                  settings
  > setVerboseCore2Core        flags, shouldDumpSimplPhase
  > setVerbosity               verbosity
  > setWarningFlag             warningFlags
  > trustPackage               packageFlags
  > unSetDynFlag               flags
  > unSetExtensionFlag         extensions, extensionFlags
  > unSetWarningFlag           warningFlags

  Below is a list of the dynamic_flags in alphabetical order along with the
  auxiliary function that they use. A handful of these flags define their
  effect on DynFlags directly; these are marked (**).

  > FLAG                           DEFINED IN TERMS OF
  > ----------------------------------------------------------------------------
  > "#include"                     addCmdlineHCInclude
  > "D"                            addOptP
  > "F"                            setDynFlag
  > "I"                            addIncludePath
  > "L"                            addLibraryPath
  > "O"                            setOptLevel
  > "Odph"                         setDPHOpt
  > "Onot"                         setOptLevel
  > "U"                            addOptP
  > "W"                            setWarningFlag
  > "Wall"                         setWarningFlag
  > "Werror"                       setDynFlag
  > "Wnot"                         ** sets warningFlags
  > "Wwarn"                        unSetDynFlag
  > "auto"                         ** sets profAuto
  > "auto-all"                     ** sets profAuto
  > "caf-all"                      setDynFlag
  > "cpp"                          setExtensionFlag
  > "dasm-lint"                    setDynFlag
  > "dcmm-lint"                    setDynFlag
  > "dcore-lint"                   setDynFlag
  > "ddump-asm"                    setDumpFlag
  > "ddump-asm-coalesce"           setDumpFlag
  > "ddump-asm-conflicts"          setDumpFlag
  > "ddump-asm-expanded"           setDumpFlag
  > "ddump-asm-liveness"           setDumpFlag
  > "ddump-asm-native"             setDumpFlag
  > "ddump-asm-regalloc"           setDumpFlag
  > "ddump-asm-regalloc-stages"    setDumpFlag
  > "ddump-asm-stats"              setDumpFlag
  > "ddump-bcos"                   setDumpFlag
  > "ddump-cmm"                    setDumpFlag
  > "ddump-cmmz"                   setDumpFlag
  > "ddump-cmmz-cafs"              setDumpFlag
  > "ddump-cmmz-cbe"               setDumpFlag
  > "ddump-cmmz-dead"              setDumpFlag
  > "ddump-cmmz-info"              setDumpFlag
  > "ddump-cmmz-lower"             setDumpFlag
  > "ddump-cmmz-pretty"            setDumpFlag
  > "ddump-cmmz-proc"              setDumpFlag
  > "ddump-cmmz-procmap"           setDumpFlag
  > "ddump-cmmz-rewrite"           setDumpFlag
  > "ddump-cmmz-sp"                setDumpFlag
  > "ddump-cmmz-spills"            setDumpFlag
  > "ddump-cmmz-split"             setDumpFlag
  > "ddump-cmmz-stub"              setDumpFlag
  > "ddump-core-pipeline"          setDumpFlag
  > "ddump-core-stats"             setDumpFlag
  > "ddump-cpranal"                setDumpFlag
  > "ddump-cps-cmm"                setDumpFlag
  > "ddump-cs-trace"               setDumpFlag
  > "ddump-cse"                    setDumpFlag
  > "ddump-cvt-cmm"                setDumpFlag
  > "ddump-deriv"                  setDumpFlag
  > "ddump-ds"                     setDumpFlag
  > "ddump-file-prefix"            setDumpPrefixForce
  > "ddump-flatC"                  setDumpFlag
  > "ddump-foreign"                setDumpFlag
  > "ddump-hi"                     setDumpFlag
  > "ddump-hi-diffs"               setDumpFlag
  > "ddump-hpc"                    setDumpFlag
  > "ddump-if-trace"               setDumpFlag
  > "ddump-inlinings"              setDumpFlag
  > "ddump-llvm"                   setObjTarget, setDumpFlag'
  > "ddump-minimal-imports"        setDumpFlag
  > "ddump-mod-cycles"             setDumpFlag
  > "ddump-occur-anal"             setDumpFlag
  > "ddump-opt-cmm"                setDumpFlag
  > "ddump-parsed"                 setDumpFlag
  > "ddump-prep"                   setDumpFlag
  > "ddump-raw-cmm"                setDumpFlag
  > "ddump-rn"                     setDumpFlag
  > "ddump-rn-stats"               setDumpFlag
  > "ddump-rn-trace"               setDumpFlag
  > "ddump-rtti"                   setDumpFlag
  > "ddump-rule-firings"           setDumpFlag
  > "ddump-rule-rewrites"          setDumpFlag
  > "ddump-rules"                  setDumpFlag
  > "ddump-simpl"                  setDumpFlag
  > "ddump-simpl-iterations"       setDumpFlag
  > "ddump-simpl-phases"           setDumpSimplPhases
  > "ddump-simpl-stats"            setDumpFlag
  > "ddump-spec"                   setDumpFlag
  > "ddump-splices"                setDumpFlag
  > "ddump-stg"                    setDumpFlag
  > "ddump-stranal"                setDumpFlag
  > "ddump-tc"                     setDumpFlag
  > "ddump-tc-trace"               setDumpFlag
  > "ddump-ticked"                 setDumpFlag
  > "ddump-to-file"                setDumpFlag
  > "ddump-types"                  setDumpFlag
  > "ddump-vect"                   setDumpFlag
  > "ddump-view-pattern-commoning" setDumpFlag
  > "ddump-vt-trace"               setDumpFlag
  > "ddump-worker-wrapper"         setDumpFlag
  > "dep-makefile"                 setDepMakefile
  > "dep-suffix"                   addDepSuffix
  > "dfaststring-stats"            setDynFlag
  > "dno-llvm-mangler"             setDynFlag
  > "dshow-passes"                 forceRecompile, setVerbosity
  > "dsource-stats"                setDumpFlag
  > "dstg-lint"                    setDynFlag
  > "dstg-stats"                   setDynFlag
  > "dumpdir"                      setDumpDir
  > "dverbose-core2core"           setVerbosity, setVerboseCore2Core
  > "dverbose-stg2stg"             setDumpFlag
  > "dylib-install-name"           setDylibInstallName
  > "dynload"                      parseDynLibLoaderMode
  > "exclude-module"               addDepExcludeMod
  > "fasm"                         setObjTarget
  > "fbyte-code"                   setTarget
  > "fcontext-stack"               ** sets ctxtStkDepth
  > "ffloat-all-lams"              ** sets floatLamArgs
  > "ffloat-lam-args"              ** sets floatLamArgs
  > "fglasgow-exts"                enableGlasgowExts
  > "fliberate-case-threshold"     ** sets liberateCaseThreshold
  > "fllvm"                        setObjTarget
  > "fmax-simplifier-iterations"   ** sets maxSimplIterations
  > "fno-code"                     setTarget, ** sets ghcLink
  > "fno-glasgow-exts"             disableGlasgowExts
  > "fno-liberate-case-threshold"  ** sets liberateCaseThreshold
  > "fno-prof-auto"                ** sets profAuto
  > "fno-safe-infer"               setSafeHaskell
  > "fno-spec-constr-count"        ** sets specConstrCount
  > "fno-spec-constr-threshold"    ** sets specConstrThreshold
  > "fobject-code"                 setTarget
  > "fpackage-trust"               setPackageTrust
  > "fplugin"                      addPluginModuleName
  > "fplugin-opt"                  addPluginModuleNameOption
  > "fprof-auto"                   ** sets profAuto
  > "fprof-auto-calls"             ** sets profAuto
  > "fprof-auto-exported"          ** sets profAuto
  > "fprof-auto-top"               ** sets profAuto
  > "framework"                    addCmdlineFramework
  > "framework-path"               addFrameworkPath
  > "frule-check"                  ** sets ruleCheck
  > "fsimpl-tick-factor"           ** sets simplTickFactor
  > "fsimplifier-phases"           ** sets simplPhases
  > "fspec-constr-count"           ** sets specConstrCount
  > "fspec-constr-threshold"       ** sets specConstrThreshold
  > "fstrictness-before"           ** sets strictnessBefore
  > "fvia-C"                       <<warning only>>
  > "fvia-c"                       <<warning only>>
  > "haddock"                      setDynFlag
  > "haddock-opts"                 addHaddockOpts
  > "hcsuf"                        setHcSuf
  > "hidir"                        setHiDir
  > "hisuf"                        setHiSuf
  > "hpcdir"                       setOptHpcDir
  > "i"                            addImportPath
  > "include-pkg-deps"             setDepIncludePkgDeps
  > "keep-hc-file"                 setDynFlag
  > "keep-hc-files"                setDynFlag
  > "keep-llvm-file"               setObjTarget, setDynFlag
  > "keep-llvm-files"              setObjTarget, setDynFlag
  > "keep-raw-s-file"              <<warning only>>
  > "keep-raw-s-files"             <<warning only>>
  > "keep-s-file"                  setDynFlag
  > "keep-s-files"                 setDynFlag
  > "keep-tmp-files"               setDynFlag
  > "l"                            addOptl
  > "main-is"                      setMainIs
  > "monly-2-regs"                 <<warning only>>
  > "monly-3-regs"                 <<warning only>>
  > "monly-4-regs"                 <<warning only>>
  > "msse2"                        setDynFlag
  > "msse4.2"                      setDynFlag
  > "n"                            <<warning only>>
  > "no-auto"                      ** sets profAuto
  > "no-auto-all"                  ** sets profAuto
  > "no-auto-link-packages"        unSetDynFlag
  > "no-caf-all"                   unSetDynFlag
  > "no-hs-main"                   setDynFlag
  > "no-link"                      ** sets ghcLink
  > "no-recomp"                    setDynFlag
  > "no-rtsopts"                   setRtsOptsEnabled
  > "o"                            setOutputFile
  > "odir"                         setObjectDir
  > "ohi"                          setOutputHi
  > "optF"                         alterSettings
  > "optL"                         alterSettings
  > "optP"                         addOptP
  > "opta"                         alterSettings
  > "optc"                         alterSettings
  > "optdep--exclude-module"       addDepExcludeMod
  > "optdep--include-pkg-deps"     setDepIncludePkgDeps
  > "optdep--include-prelude"      setDepIncludePkgDeps
  > "optdep-f"                     setDepMakefile
  > "optdep-s"                     addDepSuffix
  > "optdep-w"                     <<warning only>>
  > "optdep-x"                     addDepExcludeMod
  > "optl"                         addOptl
  > "optlc"                        alterSettings
  > "optlo"                        alterSettings
  > "optm"                         <<warning only>>
  > "optwindres"                   alterSettings
  > "osuf"                         setObjectSuf
  > "outputdir"                    setOutputDir
  > "pgmF"                         alterSettings
  > "pgmL"                         alterSettings
  > "pgmP"                         setPgmP
  > "pgma"                         alterSettings
  > "pgmc"                         alterSettings
  > "pgmdll"                       alterSettings
  > "pgml"                         alterSettings
  > "pgmlc"                        alterSettings
  > "pgmlo"                        alterSettings
  > "pgmm"                         <<warning only>>
  > "pgms"                         alterSettings
  > "pgmwindres"                   alterSettings
  > "recomp"                       unSetDynFlag
  > "rtsopts"                      setRtsOptsEnabled
  > "rtsopts=all"                  setRtsOptsEnabled
  > "rtsopts=none"                 setRtsOptsEnabled
  > "rtsopts=some"                 setRtsOptsEnabled
  > "shared"                       ** sets ghcLink
  > "split-objs"                   setDynFlag
  > "stubdir"                      setStubDir
  > "tmpdir"                       setTmpDir
  > "v"                            setVerbosity
  > "w"                            ** sets warningFlags
  > "with-rtsopts"                 setRtsOpts

  Finally, there is a bunch flags defined in terms of setDynFlag, unSetDynFlag,
  setWarningFlag, unSetWarningFlag, setExtensionFlag, unSetExtensionFlag,
  setLanguage and setSafeHaskell.

  The same list for package_flags:

  > FLAG                           DEFINED IN TERMS OF
  > ----------------------------------------------------------------------------
  > "package-conf"           extraPkgConf_
  > "no-user-package-conf"   unSetDynFlag
  > "package-name"           setPackageName
  > "package-id"             exposePackageId
  > "package"                exposePackage
  > "hide-package"           hidePackage
  > "hide-all-packages"      setDynFlag
  > "ignore-package"         ignorePackage
  > "syslib"                 exposePackage
  > "trust"                  trustPackage
  > "distrust"               distrustPackage
  > "distrust-all-packages"  setDynFlag

  In addition to the above, we also reset one more field: pkgDatabase. The
  pkgDatabase is initialized on the first call to initPackages (and hence the
  first call to setSessionDynFlags), which happens at server startup.  After
  that, subsequent calls to setSessionDynFlags take the _existing_ pkgDatabase,
  but applies the "batch package flags" to it (hide-all-packages,
  distrust-all-packages). However, it doesn't "unapply" these batch flags. By
  restoring the pkgDatabase to the value it gets at server startup, we
  effectively restore these batch flags whenever we apply user settings.
------------------------------------------------------------------------------}

dynFlagsRef :: IORef DynFlags
{-# NOINLINE dynFlagsRef #-}
dynFlagsRef = unsafePerformIO $ newIORef (error "No DynFlags stored yet")

storeDynFlags :: Ghc ()
storeDynFlags = do
  dynFlags <- getSessionDynFlags
  liftIO $ writeIORef dynFlagsRef dynFlags

restoreDynFlags :: Ghc DynFlags
restoreDynFlags = do
  storedDynFlags  <- liftIO $ readIORef dynFlagsRef
  currentDynFlags <- getSessionDynFlags
  return (currentDynFlags `restoreDynFlagsFrom` storedDynFlags)

-- | Copy over all fields of DynFlags that are affected by dynamic_flags
-- and package_flags (and only those)
--
-- See detailed description above.
restoreDynFlagsFrom :: DynFlags -> DynFlags -> DynFlags
restoreDynFlagsFrom new old = new {
    cmdlineFrameworks     = cmdlineFrameworks     old
  , cmdlineHcIncludes     = cmdlineHcIncludes     old
  , ctxtStkDepth          = ctxtStkDepth          old
  , depExcludeMods        = depExcludeMods        old
  , depIncludePkgDeps     = depIncludePkgDeps     old
  , depMakefile           = depMakefile           old
  , depSuffixes           = depSuffixes           old
  , dumpDir               = dumpDir               old
  , dumpPrefixForce       = dumpPrefixForce       old
  , dylibInstallName      = dylibInstallName      old
  , dynLibLoader          = dynLibLoader          old
  , extensionFlags        = extensionFlags        old
  , extensions            = extensions            old
  , extraPkgConfs         = extraPkgConfs         old
  , flags                 = flags                 old
  , floatLamArgs          = floatLamArgs          old
  , frameworkPaths        = frameworkPaths        old
  , ghcLink               = ghcLink               old
  , haddockOptions        = haddockOptions        old
  , hcSuf                 = hcSuf                 old
  , hiDir                 = hiDir                 old
  , hiSuf                 = hiSuf                 old
  , hpcDir                = hpcDir                old
  , hscTarget             = hscTarget             old
  , importPaths           = importPaths           old
  , includePaths          = includePaths          old
  , language              = language              old
  , liberateCaseThreshold = liberateCaseThreshold old
  , libraryPaths          = libraryPaths          old
  , mainFunIs             = mainFunIs             old
  , mainModIs             = mainModIs             old
  , maxSimplIterations    = maxSimplIterations    old
  , objectDir             = objectDir             old
  , objectSuf             = objectSuf             old
  , optLevel              = optLevel              old
  , outputFile            = outputFile            old
  , outputHi              = outputHi              old
  , packageFlags          = packageFlags          old
  , pkgDatabase           = pkgDatabase           old
  , pkgTrustOnLoc         = pkgTrustOnLoc         old
  , pluginModNameOpts     = pluginModNameOpts     old
  , pluginModNames        = pluginModNames        old
  , profAuto              = profAuto              old
  , rtsOpts               = rtsOpts               old
  , rtsOptsEnabled        = rtsOptsEnabled        old
  , ruleCheck             = ruleCheck             old
  , safeHaskell           = safeHaskell           old
  , settings              = settings              old
  , shouldDumpSimplPhase  = shouldDumpSimplPhase  old
  , simplPhases           = simplPhases           old
  , simplTickFactor       = simplTickFactor       old
  , specConstrCount       = specConstrCount       old
  , specConstrThreshold   = specConstrThreshold   old
  , strictnessBefore      = strictnessBefore      old
  , stubDir               = stubDir               old
  , thisPackage           = thisPackage           old
  , verbosity             = verbosity             old
  , warningFlags          = warningFlags          old
  }

{-------------------------------------------------------------------------------
  Package keys
-------------------------------------------------------------------------------}

type PackageKey       = GHC.PackageId
type PackageQualifier = Maybe FastString

packageKeyString :: PackageKey -> String
packageKeyString = packageIdString

stringToPackageKey :: String -> PackageKey
stringToPackageKey = stringToPackageId

mainPackageKey :: PackageKey
mainPackageKey = mainPackageId

lookupPackage :: DynFlags -> PackageKey -> Maybe PackageConfig
lookupPackage = Packages.lookupPackage . Packages.pkgIdMap . pkgState

modulePackageKey :: Module -> PackageKey
modulePackageKey = modulePackageId

-- | Translate a package key to a source ID (name and version)
--
-- NOTE: The version of wired-in packages is completely wiped out, but we use a
-- leak in the form of a Cabal package id for the same package, which still
-- contains a version. See
-- <http://www.haskell.org/ghc/docs/7.4.2/html/libraries/ghc/Module.html#g:3>
packageKeyToSourceId :: DynFlags -> PackageKey -> Maybe (String, String)
packageKeyToSourceId dflags p = do
    pkgCfg <- lookupPackage dflags p
    let srcId   = Cabal.sourcePackageId pkgCfg
        instId  = installedToSourceId $ Cabal.installedPackageId pkgCfg
        name    = pkgName srcId
        version = Cabal.pkgVersion srcId `orIfZero` Cabal.pkgVersion instId
    return (name, showVersion (stripInPlace version))
  where
    orIfZero :: Version -> Version -> Version
    orIfZero v a = case v of Version [] [] -> a ; _otherwise -> v

    stripInPlace :: Version -> Version
    stripInPlace (Version bs ts) = Version bs (filter (/= "inplace") ts)

-- | Find an exposed module in an exposed package
findExposedModule :: DynFlags -> PackageQualifier -> ModuleName -> Maybe PackageKey
findExposedModule dflags pkgQual impMod = Maybe.listToMaybe pkgIds
  where
    pkgAll      = Packages.lookupModuleInAllPackages dflags impMod
    pkgExposed  = map fst $ filter isExposed pkgAll
    pkgMatching = filter (matchesQual pkgQual) pkgExposed
    pkgIds      = map Packages.packageConfigId pkgMatching

    matchesQual :: PackageQualifier -> PackageConfig -> Bool
    matchesQual Nothing   _ = True
    matchesQual (Just fs) p = unpackFS fs == pkgName (Cabal.sourcePackageId p)

    isExposed :: (PackageConfig, Bool) -> Bool
    isExposed (pkgCfg, moduleExposed) = Cabal.exposed pkgCfg && moduleExposed

{------------------------------------------------------------------------------
  Traversing the AST
------------------------------------------------------------------------------}

ifPostTc :: AstAlg m id -> a -> Maybe a
ifPostTc alg a =
    case astPhase alg of
      FoldPreTc  -> Nothing
      FoldPostTc -> Just a

instance Fold id (HsGroup id) where
  fold alg HsGroup { hs_valds
                   , hs_tyclds
                   , hs_instds
                   , hs_derivds
                   , hs_fixds
                   , hs_defds
                   , hs_fords
                   , hs_warnds
                   , hs_annds
                   , hs_ruleds
                   , hs_vects
                   , hs_docs } = astMark alg Nothing "HsGroup" $ do
    fold alg hs_valds
    fold alg hs_tyclds
    fold alg hs_instds
    fold alg hs_derivds
    fold alg hs_fixds
    fold alg hs_defds
    fold alg hs_fords
    fold alg hs_warnds
    fold alg hs_annds
    fold alg hs_ruleds
    fold alg hs_vects
    fold alg hs_docs

instance Fold id (HsValBinds id) where
  fold _alg (ValBindsIn {}) =
    fail "fold alg: Unexpected ValBindsIn"
  fold alg (ValBindsOut binds sigs) = astMark alg Nothing "ValBindsOut" $ do
    fold alg (map snd binds)
    -- ValBindsOut specifically stores Names, independent of the phase.
    -- Traverse only in the right mode (types force this)
    case astPhase alg of
      FoldPreTc  -> fold alg sigs
      FoldPostTc -> return Nothing

instance Fold id (LSig id) where
  fold alg (L span (TypeSig names tp)) = astMark alg (Just span) "TypeSig" $ do
    forM_ names $ \name -> astId alg name SigSite
    fold alg tp
  fold alg (L span (GenericSig names tp)) = astMark alg (Just span) "GenericSig" $ do
    forM_ names $ \name -> astId alg name SigSite
    fold alg tp

  -- Only in generated code
  fold alg (L span (IdSig _)) = astMark alg (Just span) "IdSig" $
    return Nothing

  -- Annotations
  fold alg (L span (FixSig _)) = astMark alg (Just span) "FixSig" $
    return Nothing
  fold alg (L span (InlineSig _ _)) = astMark alg (Just span) "InlineSig" $
    return Nothing
  fold alg (L span (SpecSig _ _ _)) = astMark alg (Just span) "SpecSig" $
    return Nothing
  fold alg (L span (SpecInstSig _)) = astMark alg (Just span) "SpecInstSig" $
    return Nothing

instance Fold id (LHsType id) where
  fold alg (L span (HsFunTy arg res)) = astMark alg (Just span) "HsFunTy" $
    fold alg [arg, res]
  fold alg (L span (HsTyVar name)) = astMark alg (Just span) "HsTyVar" $
    astId alg (L span name) UseSite
  fold alg (L span (HsForAllTy explicitFlag tyVars ctxt body)) = astMark alg (Just span) "hsForAllTy" $ do
    case explicitFlag of
      Explicit -> fold alg tyVars
      Implicit -> return Nothing
    fold alg ctxt
    fold alg body
  fold alg (L span (HsAppTy fun arg)) = astMark alg (Just span) "HsAppTy" $
    fold alg [fun, arg]
  fold alg (L span (HsTupleTy _tupleSort typs)) = astMark alg (Just span) "HsTupleTy" $
    -- tupleSort is unboxed/boxed/etc.
    fold alg typs
  fold alg (L span (HsListTy typ)) = astMark alg (Just span) "HsListTy" $
    fold alg typ
  fold alg (L span (HsPArrTy typ)) = astMark alg (Just span) "HsPArrTy" $
    fold alg typ
  fold alg (L span (HsParTy typ)) = astMark alg (Just span) "HsParTy" $
    fold alg typ
  fold alg (L span (HsEqTy a b)) = astMark alg (Just span) "HsEqTy" $
    fold alg [a, b]
  fold alg (L span (HsDocTy typ _doc)) = astMark alg (Just span) "HsDocTy" $
    -- I don't think HsDocTy actually makes it through the renamer
    fold alg typ
  fold alg (L span (HsWrapTy _wrapper _typ)) = astMark alg (Just span) "HsWrapTy" $
    -- This is returned only by the type checker, and _typ is not located
    return Nothing
  fold alg (L span (HsRecTy fields)) = astMark alg (Just span) "HsRecTy" $
    fold alg fields
  fold alg (L span (HsKindSig typ kind)) = astMark alg (Just span) "HsKindSig" $
    fold alg [typ, kind]
  fold alg (L span (HsBangTy _bang typ)) = astMark alg (Just span) "HsBangTy" $
    fold alg typ
  fold alg (L span (HsOpTy left (_wrapper, op) right)) = astMark alg (Just span) "HsOpTy" $ do
    fold alg [left, right]
    astId alg op UseSite
  fold alg (L span (HsIParamTy _var typ)) = astMark alg (Just span) "HsIParamTy" $
    -- _var is not located
    fold alg typ
  fold alg (L span (HsSpliceTy splice _freevars _postTcKind)) = astMark alg (Just span) "HsSpliceTy" $
    fold alg (L span splice) -- reuse location info
  fold alg (L span (HsCoreTy _)) = astMark alg (Just span) "HsCoreTy" $
    -- Not important: doesn't arise until later in the compiler pipeline
    return Nothing
  fold alg (L span (HsQuasiQuoteTy qquote))  = astMark alg (Just span) "HsQuasiQuoteTy" $
    fold alg (L span qquote) -- reuse location info
  fold alg (L span (HsExplicitListTy _postTcKind typs)) = astMark alg (Just span) "HsExplicitListTy" $
    fold alg typs
  fold alg (L span (HsExplicitTupleTy _postTcKind typs)) = astMark alg (Just span) "HsExplicitTupleTy" $
    fold alg typs

instance Fold id (Located (HsSplice id)) where
  fold alg (L span (HsSplice _id expr)) = astMark alg (Just span) "HsSplice" $ do
    fold alg expr

instance Fold id (Located (HsQuasiQuote id)) where
  fold alg (L span (HsQuasiQuote _id _srcSpan _enclosed)) = astMark alg (Just span) "HsQuasiQuote" $
    -- Unfortunately, no location information is stored within HsQuasiQuote at all
    return Nothing

instance Fold id (LHsTyVarBndr id) where
  fold alg (L span (UserTyVar name _postTcKind)) = astMark alg (Just span) "UserTyVar" $ do
    astId alg (L span name) DefSite
  fold alg (L span (KindedTyVar name kind _postTcKind)) = astMark alg (Just span) "KindedTyVar" $ do
    astId alg (L span name) DefSite
    fold alg kind

instance Fold id (LHsContext id) where
  fold alg (L span typs) = astMark alg (Just span) "LHsContext" $
    fold alg typs

instance Fold id (LHsBinds id) where
  fold alg = fold alg . bagToList

instance Fold id (LHsBind id) where
  fold alg (L span bind@(FunBind {})) = astMark alg (Just span) "FunBind" $ do
    astId alg (fun_id bind) DefSite
    fold alg (fun_matches bind)
  fold alg (L span bind@(PatBind {})) = astMark alg (Just span) "PatBind" $ do
    fold alg (pat_lhs bind)
    fold alg (pat_rhs bind)
  fold alg (L span _bind@(VarBind {})) = astMark alg (Just span) "VarBind" $
    -- These are only introduced by the type checker, and don't involve user
    -- written code. The ghc comments says "located 'only for consistency'"
    return Nothing
  fold alg (L span bind@(AbsBinds {})) = astMark alg (Just span) "AbsBinds" $ do
    forM_ (abs_exports bind) $ \abs_export ->
      astId alg (L typecheckOnly (abe_poly abs_export)) DefSite
    fold alg (abs_binds bind)

typecheckOnly :: SrcSpan
typecheckOnly = mkGeneralSrcSpan (fsLit "<typecheck only>")

instance Fold id (MatchGroup id) where
  -- We ignore the postTcType, as it doesn't have location information
  fold alg (MatchGroup matches _postTcType) = astMark alg Nothing "MatchGroup" $
    fold alg matches

instance Fold id (LMatch id) where
  fold alg (L span (Match pats _type rhss)) = astMark alg (Just span) "Match" $ do
    fold alg pats
    fold alg rhss

instance Fold id (GRHSs id) where
  fold alg (GRHSs rhss binds) = astMark alg Nothing "GRHSs" $ do
    fold alg rhss
    fold alg binds

instance Fold id (LGRHS id) where
  fold alg (L span (GRHS _guards rhs)) = astMark alg (Just span) "GRHS" $
    fold alg rhs

instance Fold id (HsLocalBinds id) where
  fold _alg EmptyLocalBinds =
    return Nothing
  fold alg (HsValBinds binds) = astMark alg Nothing "HsValBinds" $ do
    fold alg binds
  fold alg (HsIPBinds binds) =
    fold alg binds

instance Fold id (HsIPBinds id) where
  fold alg (IPBinds binds _evidence) =
    fold alg binds

instance Fold id (LIPBind id) where
  fold alg (L span (IPBind _name expr)) = astMark alg (Just span) "IPBind" $ do
    -- Name is not located :(
    fold alg expr

instance Fold id (LHsExpr id) where
  fold alg (L span (HsPar expr)) = astMark alg (Just span) "HsPar" $
    fold alg expr
  fold alg (L span (ExprWithTySig expr _type)) = astMark alg (Just span) "ExprWithTySig" $
    fold alg expr
  fold alg (L span (ExprWithTySigOut expr _type)) = astMark alg (Just span) "ExprWithTySigOut" $
    fold alg expr
  fold alg (L span (HsOverLit (OverLit{ol_type}))) = astMark alg (Just span) "HsOverLit" $ do
    astExpType alg span (ifPostTc alg ol_type)
  fold alg (L span (OpApp left op _fix right)) = astMark alg (Just span) "OpApp" $ do
    _leftTy  <- fold alg left
    opTy     <- fold alg op
    _rightTy <- fold alg right
    astExpType alg span (funRes2 <$> opTy)
  fold alg (L span (HsVar id)) = astMark alg (Just span) "HsVar" $ do
    astId alg (L span id) UseSite
  fold alg (L span (HsWrap wrapper expr)) = astMark alg (Just span) "HsWrap" $ do
    ty <- fold alg (L span expr)
    astExpType alg span (applyWrapper wrapper <$> ty)
  fold alg (L span (HsLet binds expr)) = astMark alg (Just span) "HsLet" $ do
    fold alg binds
    ty <- fold alg expr
    astExpType alg span ty -- Re-astId alg this with the span of the whole let
  fold alg (L span (HsApp fun arg)) = astMark alg (Just span) "HsApp" $ do
    funTy  <- fold alg fun
    _argTy <- fold alg arg
    astExpType alg span (funRes1 <$> funTy)
  fold alg (L span (HsLit lit)) =
    -- Intentional omission of the "astMark alg" debugging call here.
    -- The syntax "assert" is replaced by GHC by "assertError <span>", where
    -- both "assertError" and the "<span>" are assigned the source span of
    -- the original "assert". This means that the <span> (represented as an
    -- HsLit) might override "assertError" in the IdMap.
    astExpType alg span (ifPostTc alg (hsLitType lit))
  fold alg (L span (HsLam matches@(MatchGroup _ postTcType))) = astMark alg (Just span) "HsLam" $ do
    fold alg matches
    astExpType alg span (ifPostTc alg postTcType)
  fold alg (L span (HsDo _ctxt stmts postTcType)) = astMark alg (Just span) "HsDo" $ do
    -- ctxt indicates what kind of statement it is; AFAICT there is no
    -- useful information in it for us
    fold alg stmts
    astExpType alg span (ifPostTc alg postTcType)
  fold alg (L span (ExplicitList postTcType exprs)) = astMark alg (Just span) "ExplicitList" $ do
    fold alg exprs
    astExpType alg span (mkListTy <$> ifPostTc alg postTcType)
  fold alg (L span (RecordCon con postTcExpr recordBinds)) = astMark alg (Just span) "RecordCon" $ do
    fold alg recordBinds
    -- Only traverse the postTcExpr in the right phase (types force us! yay! :)
    case astPhase alg of
      FoldPreTc -> do
        astId alg con UseSite
        return Nothing
      FoldPostTc -> do
        conTy <- fold alg (L (getLoc con) postTcExpr)
        astExpType alg span (funResN <$> conTy)
  fold alg (L span (HsCase expr matches@(MatchGroup _ postTcType))) = astMark alg (Just span) "HsCase" $ do
    fold alg expr
    fold alg matches
    astExpType alg span (funRes1 <$> ifPostTc alg postTcType)
  fold alg (L span (ExplicitTuple args boxity)) = astMark alg (Just span) "ExplicitTuple" $ do
    argTys <- mapM (fold alg) args
    astExpType alg span (mkTupleTy (boxityNormalTupleSort boxity) <$> sequence argTys)
  fold alg (L span (HsIf _rebind cond true false)) = astMark alg (Just span) "HsIf" $ do
    _condTy <- fold alg cond
    _trueTy <- fold alg true
    falseTy <- fold alg false
    astExpType alg span falseTy
  fold alg (L span (SectionL arg op)) = astMark alg (Just span) "SectionL" $ do
    _argTy <- fold alg arg
    opTy   <- fold alg op
    astExpType alg span (mkSectionLTy <$> opTy)
   where
      mkSectionLTy ty = let (_arg1, arg2, res) = splitFunTy2 ty
                        in mkFunTy arg2 res
  fold alg (L span (SectionR op arg)) = astMark alg (Just span) "SectionR" $ do
    opTy   <- fold alg op
    _argTy <- fold alg arg
    astExpType alg span (mkSectionRTy <$> opTy)
   where
      mkSectionRTy ty = let (arg1, _arg2, res) = splitFunTy2 ty
                        in mkFunTy arg1 res
  fold alg (L span (HsIPVar _name)) = astMark alg (Just span) "HsIPVar" $
    -- _name is not located :(
    return Nothing
  fold alg (L span (NegApp expr _rebind)) = astMark alg (Just span) "NegApp" $ do
    ty <- fold alg expr
    astExpType alg span ty
  fold alg (L span (HsBracket th)) = astMark alg (Just span) "HsBracket" $
    fold alg th
  fold alg (L span (HsBracketOut th pendingSplices)) = astMark alg (Just span) "HsBracketOut" $ do
    -- Given something like
    --
    -- > \x xs -> [| x : xs |]
    --
    -- @pendingSplices@ contains
    --
    -- > [ "x",  "Language.Haskell.TH.Syntax.lift x"
    -- > , "xs", "Language.Haskell.TH.Syntax.lift xs"
    -- > ]
    --
    -- Sadly, however, ghc attaches <no location info> to these splices.
    -- Moreover, we don't get any type information about the whole bracket
    -- expression either :(
    case astPhase alg of
      FoldPreTc  -> fold alg th
      FoldPostTc -> do forM_ pendingSplices $ \(_name, splice) ->
                         fold alg splice
                       return Nothing
  fold alg (L span (RecordUpd expr binds _dataCons _postTcTypeInp _postTcTypeOutp)) = astMark alg (Just span) "RecordUpd" $ do
    recordTy <- fold alg expr
    fold alg binds
    astExpType alg span recordTy -- The type doesn't change
  fold alg (L span (HsProc pat body)) = astMark alg (Just span) "HsProc" $ do
    fold alg pat
    fold alg body
  fold alg (L span (HsArrApp arr inp _postTcType _arrType _orient)) = astMark alg (Just span) "HsArrApp" $ do
    fold alg [arr, inp]
  fold alg (L span (HsArrForm expr _fixity cmds)) = astMark alg (Just span) "HsArrForm" $ do
    fold alg expr
    fold alg cmds
  fold alg (L span (HsTick _tickish expr)) = astMark alg (Just span) "HsTick" $ do
    fold alg expr
  fold alg (L span (HsBinTick _trueTick _falseTick expr)) = astMark alg (Just span) "HsBinTick" $ do
    fold alg expr
  fold alg (L span (HsTickPragma _span expr)) = astMark alg (Just span) "HsTickPragma" $ do
    fold alg expr
  fold alg (L span (HsSCC _string expr)) = astMark alg (Just span) "HsSCC" $ do
    fold alg expr
  fold alg (L span (HsCoreAnn _string expr)) = astMark alg (Just span) "HsCoreAnn" $ do
    fold alg expr
  fold alg (L span (HsSpliceE splice)) = astMark alg (Just span) "HsSpliceE" $ do
    fold alg (L span splice) -- reuse span
  fold alg (L span (HsQuasiQuoteE qquote)) = astMark alg (Just span) "HsQuasiQuoteE" $ do
    fold alg (L span qquote) -- reuse span
  fold alg (L span (ExplicitPArr _postTcType exprs)) = astMark alg (Just span) "ExplicitPArr" $ do
    fold alg exprs
  fold alg (L span (PArrSeq _postTcType seqInfo)) = astMark alg (Just span) "PArrSeq" $ do
    fold alg seqInfo

  -- According to the comments in HsExpr.lhs,
  -- "These constructors only appear temporarily in the parser.
  -- The renamer translates them into the Right Thing."
  fold alg (L span EWildPat) = astMark alg (Just span) "EWildPat" $
    return Nothing
  fold alg (L span (EAsPat _ _)) = astMark alg (Just span) "EAsPat" $
    return Nothing
  fold alg (L span (EViewPat _ _)) = astMark alg (Just span) "EViewPat" $
    return Nothing
  fold alg (L span (ELazyPat _)) = astMark alg (Just span) "ELazyPat" $
    return Nothing
  fold alg (L span (HsType _ )) = astMark alg (Just span) "HsType" $
    return Nothing
  fold alg (L span (ArithSeq postTcExpr seqInfo)) = astMark alg (Just span) "ArithSeq" $ do
    fold alg seqInfo
    case astPhase alg of
      FoldPreTc  -> return Nothing
      FoldPostTc -> fold alg (L span postTcExpr)

instance Fold id (ArithSeqInfo id) where
  fold alg (From expr) = astMark alg Nothing "From" $
    fold alg expr
  fold alg (FromThen frm thn) = astMark alg Nothing "FromThen" $
    fold alg [frm, thn]
  fold alg (FromTo frm to) = astMark alg Nothing "FromTo" $
    fold alg [frm, to]
  fold alg (FromThenTo frm thn to) = astMark alg Nothing "FromThenTo" $
    fold alg [frm, thn, to]

instance Fold id (LHsCmdTop id) where
  fold alg (L span (HsCmdTop cmd _postTcTypeInp _postTcTypeRet _syntaxTable)) = astMark alg (Just span) "HsCmdTop" $
    fold alg cmd

instance Fold id (HsBracket id) where
  fold alg (ExpBr expr) = astMark alg Nothing "ExpBr" $
    fold alg expr
  fold alg (PatBr pat) = astMark alg Nothing "PatBr" $
    fold alg pat
  fold alg (DecBrG group) = astMark alg Nothing "DecBrG" $
    fold alg group
  fold alg (TypBr typ) = astMark alg Nothing "TypBr" $
    fold alg typ
  fold alg (VarBr _namespace _id) = astMark alg Nothing "VarBr" $
    -- No location information, sadly
    return Nothing
  fold alg (DecBrL decls) = astMark alg Nothing "DecBrL" $
    fold alg decls

instance Fold id (HsTupArg id) where
  fold alg (Present arg) =
    fold alg arg
  fold _alg (Missing _postTcType) =
    return Nothing

instance Fold id a => Fold id (HsRecFields id a) where
  fold alg (HsRecFields rec_flds _rec_dotdot) = astMark alg Nothing "HsRecFields" $
    fold alg rec_flds

instance Fold id a => Fold id (HsRecField id a) where
  fold alg (HsRecField id arg _pun) = astMark alg Nothing "HsRecField" $ do
    astId alg id UseSite
    fold alg arg

-- The meaning of the constructors of LStmt isn't so obvious; see various
-- notes in ghc/compiler/hsSyn/HsExpr.lhs
instance Fold id (LStmt id) where
  fold alg (L span (ExprStmt expr _seq _guard _postTcType)) = astMark alg (Just span) "ExprStmt" $
    -- Neither _seq nor _guard are located
    fold alg expr
  fold alg (L span (BindStmt pat expr _bind _fail)) = astMark alg (Just span) "BindStmt" $ do
    -- Neither _bind or _fail are located
    fold alg pat
    fold alg expr
  fold alg (L span (LetStmt binds)) = astMark alg (Just span) "LetStmt" $
    fold alg binds
  fold alg (L span (LastStmt expr _return)) = astMark alg (Just span) "LastStmt" $
    fold alg expr
  fold alg (L span stmt@(RecStmt {})) = astMark alg (Just span) "RecStmt" $ do
    fold alg (recS_stmts stmt)

  fold alg (L span (TransStmt {}))     = astUnsupported alg (Just span) "TransStmt"
  fold alg (L span (ParStmt _ _ _ _))  = astUnsupported alg (Just span) "ParStmt"

instance Fold id (LPat id) where
  fold alg (L span (WildPat postTcType)) = astMark alg (Just span) "WildPat" $
    astExpType alg span (ifPostTc alg postTcType)
  fold alg (L span (VarPat id)) = astMark alg (Just span) "VarPat" $
    astId alg (L span id) DefSite
  fold alg (L span (LazyPat pat)) = astMark alg (Just span) "LazyPat" $
    fold alg pat
  fold alg (L span (AsPat id pat)) = astMark alg (Just span) "AsPat" $ do
    astId alg id DefSite
    fold alg pat
  fold alg (L span (ParPat pat)) = astMark alg (Just span) "ParPat" $
    fold alg pat
  fold alg (L span (BangPat pat)) = astMark alg (Just span) "BangPat" $
    fold alg pat
  fold alg (L span (ListPat pats _postTcType)) = astMark alg (Just span) "ListPat" $
    fold alg pats
  fold alg (L span (TuplePat pats _boxity _postTcType)) = astMark alg (Just span) "TuplePat" $
    fold alg pats
  fold alg (L span (PArrPat pats _postTcType)) = astMark alg (Just span) "PArrPat" $
    fold alg pats
  fold alg (L span (ConPatIn con details)) = astMark alg (Just span) "ConPatIn" $ do
    -- Unlike ValBindsIn and HsValBindsIn, we *do* get ConPatIn
    astId alg con UseSite -- the constructor name is non-binding
    fold alg details
  fold alg (L span (ConPatOut {pat_con, pat_args})) = astMark alg (Just span) "ConPatOut" $ do
    -- this pattern match on unit is necessary to avoid ghc bug (not sure why)
    () <- case astPhase alg of
      FoldPreTc  -> do astId alg (L (getLoc pat_con) (dataConName (unLoc pat_con))) UseSite
                       return ()
      FoldPostTc -> return ()
    fold alg pat_args
  fold alg (L span (LitPat _)) = astMark alg (Just span) "LitPat" $
    return Nothing
  fold alg (L span (NPat _ _ _)) = astMark alg (Just span) "NPat" $
    return Nothing
  fold alg (L span (NPlusKPat id _lit _rebind1 _rebind2)) = astMark alg (Just span) "NPlusKPat" $ do
    astId alg id DefSite
  fold alg (L span (ViewPat expr pat _postTcType)) = astMark alg (Just span) "ViewPat" $ do
    fold alg expr
    fold alg pat
  fold alg (L span (SigPatIn pat typ)) = astMark alg (Just span) "SigPatIn" $ do
    fold alg pat
    fold alg typ
  fold alg (L span (SigPatOut pat _typ)) = astMark alg (Just span) "SigPatOut" $ do
    -- _typ is not located
    fold alg pat
  fold alg (L span (QuasiQuotePat qquote)) = astMark alg (Just span) "QuasiQuotePat" $
    fold alg (L span qquote) -- reuse span

  -- During translation only
  fold alg (L span (CoPat _ _ _)) = astMark alg (Just span) "CoPat" $
    return Nothing

instance (Fold id arg, Fold id rec) => Fold id (HsConDetails arg rec) where
  fold alg (PrefixCon args) = astMark alg Nothing "PrefixCon" $
    fold alg args
  fold alg (RecCon rec) = astMark alg Nothing "RecCon" $
    fold alg rec
  fold alg (InfixCon a b) = astMark alg Nothing "InfixCon" $
    fold alg [a, b]

instance Fold id (LTyClDecl id) where
  fold alg (L span decl@(TyData {})) = astMark alg (Just span) "TyData" $ do
    fold alg (tcdCtxt decl)
    astId alg (tcdLName decl) DefSite
    fold alg (tcdTyVars decl)
    fold alg (tcdTyPats decl)
    fold alg (tcdKindSig decl)
    fold alg (tcdCons decl)
    fold alg (tcdDerivs decl)
  fold alg (L span decl@(ClassDecl {})) = astMark alg (Just span) "ClassDecl" $ do
    fold alg (tcdCtxt decl)
    astId alg (tcdLName decl) DefSite
    fold alg (tcdTyVars decl)
    -- Sadly, we don't get location info for the functional dependencies
    fold alg (tcdSigs decl)
    fold alg (tcdMeths decl)
    fold alg (tcdATs decl)
    fold alg (tcdATDefs decl)
    fold alg (tcdDocs decl)
  fold alg (L span decl@(TySynonym {})) = astMark alg (Just span) "TySynonym" $ do
    -- See "Representation of indexed types" in compiler/hsSyn/HsDecls.lhs
    astId alg (tcdLName decl) (case tcdTyPats decl of
                                  Nothing -> DefSite
                                  Just _  -> UseSite)
    fold alg (tcdTyVars decl)
    fold alg (tcdTyPats decl)
    fold alg (tcdSynRhs decl)
  fold alg (L span decl@(TyFamily {})) = astMark alg (Just span) "TyFamily" $  do
    astId alg (tcdLName decl) DefSite
    fold alg (tcdTyVars decl)
    fold alg (tcdKind decl)
  fold alg (L span _decl@(ForeignType {})) = astUnsupported alg (Just span) "ForeignType"

instance Fold id (LConDecl id) where
  fold alg (L span decl@(ConDecl {})) = astMark alg (Just span) "ConDecl" $ do
    astId alg (con_name decl) DefSite
    fold alg (con_qvars decl)
    fold alg (con_cxt decl)
    fold alg (con_details decl)
    fold alg (con_res decl)

instance Fold id (ResType id) where
  fold alg ResTyH98 = astMark alg Nothing "ResTyH98" $ do
    return Nothing -- Nothing to do
  fold alg (ResTyGADT typ) = astMark alg Nothing "ResTyGADT" $ do
    fold alg typ

instance Fold id (ConDeclField id) where
  fold alg (ConDeclField name typ _doc) = do
    astId alg name DefSite
    fold alg typ

instance Fold id (LInstDecl id) where
  fold alg (L span (InstDecl typ binds sigs accTypes)) = astMark alg (Just span) "LInstDecl" $ do
    fold alg typ
    fold alg binds
    fold alg sigs
    fold alg accTypes

instance Fold id (LDerivDecl id) where
  fold alg (L span (DerivDecl deriv_type)) = astMark alg (Just span) "LDerivDecl" $ do
    fold alg deriv_type

instance Fold id (LFixitySig id) where
  fold alg (L span (FixitySig name _fixity)) = astMark alg (Just span) "LFixitySig" $ do
    astId alg name SigSite

instance Fold id (LDefaultDecl id) where
  fold alg (L span (DefaultDecl typs)) = astMark alg (Just span) "LDefaultDecl" $ do
    fold alg typs

instance Fold id (LForeignDecl id) where
  fold alg (L span (ForeignImport name sig _coercion _import)) = astMark alg (Just span) "ForeignImport" $ do
    astId alg name DefSite
    fold alg sig
  fold alg (L span (ForeignExport name sig _coercion _export)) = astMark alg (Just span) "ForeignExport" $ do
    astId alg name UseSite
    fold alg sig

instance Fold id (LWarnDecl id) where
  fold alg (L span (Warning name _txt)) = astMark alg (Just span) "Warning" $ do
    -- We use the span of the entire warning because we don't get location info for name
    astId alg (L span name) UseSite

instance Fold id (LAnnDecl id) where
  fold alg (L span _) = astUnsupported alg (Just span) "LAnnDecl"

instance Fold id (LRuleDecl id) where
  fold alg (L span _) = astUnsupported alg (Just span) "LRuleDecl"

instance Fold id (LVectDecl id) where
  fold alg (L span _) = astUnsupported alg (Just span) "LVectDecl"

instance Fold id LDocDecl where
  fold alg (L span _) = astMark alg (Just span) "LDocDec" $
    -- Nothing to do
    return Nothing

instance Fold id (Located (SpliceDecl id)) where
  fold alg (L span (SpliceDecl expr _explicit)) = astMark alg (Just span) "SpliceDecl" $ do
    fold alg expr

-- LHsDecl is a wrapper around the various kinds of declarations; the wrapped
-- declarations don't have location information of themselves, so we reuse
-- the location info of the wrapper
instance Fold id (LHsDecl id) where
  fold alg (L span (TyClD tyClD)) = astMark alg (Just span) "TyClD" $
    fold alg (L span tyClD)
  fold alg (L span (InstD instD)) = astMark alg (Just span) "InstD" $
    fold alg (L span instD)
  fold alg (L span (DerivD derivD)) = astMark alg (Just span) "DerivD" $
    fold alg (L span derivD)
  fold alg (L span (ValD valD)) = astMark alg (Just span) "ValD" $
    fold alg (L span valD)
  fold alg (L span (SigD sigD)) = astMark alg (Just span) "SigD" $
    fold alg (L span sigD)
  fold alg (L span (DefD defD)) = astMark alg (Just span) "DefD" $
    fold alg (L span defD)
  fold alg (L span (ForD forD)) = astMark alg (Just span) "ForD" $
    fold alg (L span forD)
  fold alg (L span (WarningD warningD)) = astMark alg (Just span) "WarningD" $
    fold alg (L span warningD)
  fold alg (L span (AnnD annD)) = astMark alg (Just span) "AnnD" $
    fold alg (L span annD)
  fold alg (L span (RuleD ruleD)) = astMark alg (Just span) "RuleD" $
    fold alg (L span ruleD)
  fold alg (L span (VectD vectD)) = astMark alg (Just span) "VectD" $
    fold alg (L span vectD)
  fold alg (L span (SpliceD spliceD)) = astMark alg (Just span) "SpliceD" $
    fold alg (L span spliceD)
  fold alg (L span (DocD docD)) = astMark alg (Just span) "DocD" $
    fold alg (L span docD)
  fold alg (L span (QuasiQuoteD quasiQuoteD)) = astMark alg (Just span) "QuasiQuoteD" $
    fold alg (L span quasiQuoteD)

{------------------------------------------------------------------------------
  Operations on types
------------------------------------------------------------------------------}

applyWrapper :: HsWrapper -> Type -> Type
applyWrapper WpHole            t = t -- identity
applyWrapper (WpTyApp t')      t = applyTy t t'
applyWrapper (WpEvApp _)       t = funRes1 t
applyWrapper (WpCompose w1 w2) t = applyWrapper w1 . applyWrapper w2 $ t
applyWrapper (WpCast coercion) _ = let Pair _ t = tcCoercionKind coercion in t
applyWrapper (WpTyLam v)       t = mkForAllTy v t
applyWrapper (WpEvLam v)       t = mkFunTy (evVarPred v) t
applyWrapper (WpLet _)         t = t -- we don't care about evidence _terms_

-- | Given @a -> b@, return @b@
funRes1 :: Type -> Type
funRes1 = snd . splitFunTy

-- | Given @a1 -> a2 -> b@, return @b@
funRes2 :: Type -> Type
funRes2 = funRes1 . funRes1

-- | Given @a1 -> a2 -> ... -> b@, return @b@
funResN :: Type -> Type
funResN = snd . splitFunTys

-- | Given @a -> b -> c@, return @(a, b, c)@
splitFunTy2 :: Type -> (Type, Type, Type)
splitFunTy2 ty0 = let (arg1, ty1) = splitFunTy ty0
                      (arg2, ty2) = splitFunTy ty1
                  in (arg1, arg2, ty2)

typeOfTyThing :: TyThing -> Maybe Type
typeOfTyThing (ADataCon dataCon) = Just $ dataConRepType dataCon
typeOfTyThing _ = Nothing

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Parse an installed package ID as if it was a source package ID
--
-- NOTE: This no longer works for ghc 7.10 and up.
installedToSourceId :: Cabal.InstalledPackageId -> Cabal.PackageId
installedToSourceId (Cabal.InstalledPackageId instId) = parseSourceId instId

-- | Parse a source package ID
--
-- Returns an empty package ID if the parse failed.
parseSourceId :: String -> Cabal.PackageId
parseSourceId = emptyOnParseFailure
              . Maybe.mapMaybe successfulParse
              . Cabal.readP_to_S Cabal.parse
  where
    successfulParse :: (a, String) -> Maybe a
    successfulParse (a, unparsed) = if null unparsed then Just a else Nothing

    emptyOnParseFailure :: [Cabal.PackageId] -> Cabal.PackageId
    emptyOnParseFailure (i:_) = i
    emptyOnParseFailure []    = Cabal.PackageIdentifier {
                                    pkgName    = Cabal.PackageName ""
                                  , pkgVersion = Version [] []
                                  }

pkgName :: Cabal.PackageIdentifier -> String
pkgName pkgId = let Cabal.PackageName nm = Cabal.pkgName pkgId in nm
