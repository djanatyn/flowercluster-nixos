#!/usr/bin/env stack
{- stack script
  --resolver lts-14.2
  --package shake
  --package aeson
  --package text
  --package bytestring
  --package directory
-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module      : Grow
Description : Grows the infrastructure for flowercluster.

This module compiles to a build system used to maintain flowercluster, a digital
garden.

The build system manages tasks such as:

  * building infrastructure from Terraform modules,
  * parsing Terraform output files,
  * building AMI images from Packer templates,
  * parsing Packer manifest files.

Why maintain a complex build system?

  * Builds are reproducible: build artifacts can be reconstructed and updated
  * Builds are safe: each action in a build rule is typed
  * Builds are composable: complex build actions are defined by combining
    smaller actions
  * Builds are extensible: the build system is a Haskell library
-}

module Grow where

import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory               ( makeAbsolute )

import qualified Data.ByteString.Lazy          as BL

import           Data.Aeson

import qualified Data.Text                     as T

-- | Flowercluster build system.
main :: IO ()
main = shakeArgs shakeOptions $ do
  phony "init" $ runTerraform Init

  phony "clean" $ clean

  phony "plan" $ runTerraform $ Plan "grow.plan"

  phony "apply" $ do
    need ["_build/grow.plan"]
    runTerraform $ Apply "_build/grow.plan"

  phony "output" $ do
    need ["_build/terraformOutput.json"]
    terraformOutput <- readTerraformOutput "_build/terraformOutput.json"
    putNormal $ show terraformOutput

  "_build/terraformOutput.json" %> terraformOutput

  "_build/grow.plan" %> runTerraform . Plan

-- * Build Tasks
-- | Clean build directory.
clean :: Action ()
clean = do
  putQuiet $ "cleaning build directory"
  removeFilesAfter "_build" ["//*"]

-- * Types
-- | AWS resource IDs are Strings.
type ID = String

-- * Terraform
-- ** Executing Commands
-- Terraform actions.
data TerraformCmd = Init | Plan FilePath | Apply FilePath | Output FilePath

-- | Execute arbitrary Terraform command.
rawTerraformCmd :: CmdResult r => String -> Action r
rawTerraformCmd args = do
  putQuiet $ "executing '" ++ command ++ "'"
  cmd (Cwd "terraform") command
  where command = "terraform " ++ args

-- | Execute Terraform command.
runTerraform :: CmdResult r => TerraformCmd -> Action r
runTerraform Init          = rawTerraformCmd "init"
runTerraform (Output path) = rawTerraformCmd "output -json"
runTerraform (Plan   path) = do
  planPath <- liftIO $ makeAbsolute path
  rawTerraformCmd $ "plan -out=" ++ planPath where
runTerraform (Apply path) = do
  planPath <- liftIO $ makeAbsolute path
  rawTerraformCmd $ "apply " ++ planPath

-- ** Terraform Outputs
-- Terraform network module output.
data TerraformOutput = TerraformOutput
  { vpcID :: ID
  , dmzID :: ID
  , internalID :: ID
  , dmzSSHSecurityGroupID :: ID } deriving (Show)

-- | Parse Terraform output JSON.
instance FromJSON TerraformOutput where
  parseJSON = withObject "output" $ \o -> do
    vpcID                 <- o .: "vpc_id" >>= (.: "value")
    internalID            <- o .: "subnets" >>= (.: "value") >>= (.: "internal")
    dmzID                 <- o .: "subnets" >>= (.: "value") >>= (.: "dmz")
    dmzSSHSecurityGroupID <-
      o .: "security_groups" >>= (.: "value") >>= (.: "dmz_ssh")

    return TerraformOutput { .. }

-- | Read Terraform module output.
readTerraformOutput
  :: FilePath -- ^ json file
  -> Action (Maybe TerraformOutput)
readTerraformOutput path = do
  json <- liftIO $ BL.readFile path
  return $ decode json

-- | Write Terraform output to path.
terraformOutput :: FilePath -> Action ()
terraformOutput path = do
  Stdout output <- rawTerraformCmd "output -json"
  writeFileChanged path output

-- * Packer
-- ** Packer Environment
-- Set environment variables for a packer build.
packerEnv :: TerraformOutput -> [CmdOption]
packerEnv TerraformOutput {..} = [AddEnv "PACKER_BUILD_SUBNET" dmzID]

rawPackerCommand :: CmdResult r => String -> Action r
rawPackerCommand args = do
  putQuiet $ "executing Packer: " ++ command
  cmd (Cwd "packer") command
  where command = "packer " ++ args
