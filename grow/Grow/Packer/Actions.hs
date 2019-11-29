{-# LANGUAGE NamedFieldPuns #-}

module Grow.Packer.Actions
  ( -- * Execute Actions
    runPacker,
  )
where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Development.Shake
import Development.Shake.Command (IsCmdArgument)
import Grow.Types

-- | Run arbitary Packer command.
rawPackerCommand ::
  (IsCmdArgument args, CmdResult r) =>
  TerraformOutput ->
  args ->
  Action r
rawPackerCommand env args = do
  putQuiet $ "environment: " ++ show (packerEnv env)
  cmd (packerEnv env) "packer" args

-- | Run Packer command.
runPacker :: CmdResult r => TerraformOutput -> PackerCmd -> Action r
runPacker env (Build path) = rawPackerCommand env $ "build " ++ path

-- | Set environment variables for a packer build.
packerEnv :: TerraformOutput -> [CmdOption]
packerEnv TerraformOutput {terraformDMZSubnet, terraformDMZSecurityGroup} =
  [ AddEnv "PACKER_BUILD_SUBNET" terraformDMZSubnet,
    AddEnv "PACKER_SECURITY_GROUP" terraformDMZSecurityGroup,
    AddEnv "PACKER_LOG" "1",
    Cwd "packer"
  ]

readPackerManifest ::
  -- | JSON Packer manifest path.
  FilePath ->
  Action (Maybe PackerManifest)
readPackerManifest path = liftIO $ decode <$> BL.readFile path
