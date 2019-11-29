{-# LANGUAGE NamedFieldPuns #-}

module Grow.Packer.Actions
  ( -- * Execute Actions
    runPacker,

    -- * Packer Manifest
    manifestPath,
    readPackerManifest,

    -- * Build Rules
    packerRules,
  )
where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Development.Shake
import Development.Shake.Command (IsCmdArgument)
import Grow.Terraform.Actions as T
import Grow.Types

-- | Run arbitary Packer command.
rawPackerCommand ::
  (IsCmdArgument args, CmdResult r) =>
  PackerConfig ->
  args ->
  Action r
rawPackerCommand PackerConfig {packerEnv, packerImageName} = cmd env "packer"
  where
    env :: [CmdOption]
    env =
      [ AddEnv "PACKER_BUILD_SUBNET" (terraformDMZSubnet packerEnv),
        AddEnv "PACKER_SECURITY_GROUP" (terraformDMZSecurityGroup packerEnv),
        AddEnv "PACKER_LOG" "1",
        AddEnv "PACKER_MANIFEST_DIRECTORY" "_build/growManifest.json",
        AddEnv "PACKER_IMAGE_PREFIX" packerImageName
      ]

-- | Run Packer command.
runPacker :: CmdResult r => PackerConfig -> PackerCmd -> Action r
runPacker config (Build path) = rawPackerCommand config ["build", path]

readPackerManifest ::
  -- | JSON Packer manifest path.
  FilePath ->
  Action (Maybe PackerManifest)
readPackerManifest path = liftIO $ decode <$> BL.readFile path

manifestPath :: FilePath
manifestPath = "_build/growManifest.json"

packerRules :: Rules ()
packerRules = do
  manifestPath %> \packerManifestPath -> do
    need [T.outputPath]
    out <- T.readTerraformOutput T.outputPath
    case out of
      Just packerEnv ->
        runPacker
          PackerConfig
            { packerEnv,
              packerManifestPath,
              packerImageName = "grow-nixos"
            }
          (Build "packer/nixos.json")
      _ -> error "couldn't parse terraform output!"
