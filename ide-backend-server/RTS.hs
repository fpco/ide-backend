{-# LANGUAGE TemplateHaskell #-}
module RTS ( RtsInfo(..), deployRts )

where

import Data.FileEmbed (embedFile)
import System.IO.Temp (createTempDirectory)
import System.FilePath ( (</>) )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


-- | A tarball with a relocatable package-db, containing the rts.
--   File "embedded-rts.tgz" is automatically created by the build system.
rts_tarball :: BS.ByteString
rts_tarball = $(embedFile "embedded-rts.tgz")

data RtsInfo = RtsInfo { rtsPackageDb :: FilePath, rtsPackage :: String }


-- | Extracts the embedded rts-package to a temporary directory
--   inside the given directory, and returns its location.
deployRts :: FilePath -> IO RtsInfo
deployRts dir = do
  deployDir <- createTempDirectory dir "rts."

  extractTarball (LBS.fromStrict rts_tarball) deployDir

  return RtsInfo {
    rtsPackageDb = deployDir </> "embedded-rts" </> "pkgdb"
  , rtsPackage   = "ide-backend-rts"
  }
 

extractTarball :: LBS.ByteString -> FilePath -> IO ()
extractTarball tarball dest =
  Tar.unpack dest $ Tar.read $ GZip.decompress $ tarball
