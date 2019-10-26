#!/usr/bin/env stack
{- stack script
  --resolver lts-14.2
  --package shake
  --package aeson
  --package text
  --package bytestring
-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Grow where

import           Development.Shake
import           Development.Shake.FilePath

import qualified Data.ByteString.Lazy          as BL

import           Data.Aeson

import           qualified Data.Text as T

main :: IO ()
main = shakeArgs shakeOptions $ do
  phony "init" $ runTerraform Init

  phony "clean" $ clean

  phony "output" $ do
    need ["_build/output.json"]
    networkOutput <- readNetworkOutput "_build/network-output.json"
    putNormal $ show networkOutput

  "_build/network-output.json" %> runTerraform . Output

-- * Types
-- | AWS resource IDs are Strings.
type ID = String

-- * Terraform
-- ** Executing Commands
-- Terraform actions.
data TerraformCmd = Init | Plan FilePath | Output FilePath

-- | Execute arbitrary Terraform command.
rawTerraformCmd :: CmdResult r => String -> Action r
rawTerraformCmd args = do
  putQuiet $ "executing '" ++ command ++ "'"
  cmd (Cwd "terraform") command where
    command = "terraform " ++ args

-- | Execute Terraform command.
runTerraform :: CmdResult r => TerraformCmd -> Action r
runTerraform Init          = rawTerraformCmd "init"
runTerraform (Plan path)   = rawTerraformCmd $ "plan -out=" ++ path
runTerraform (Output path) = rawTerraformCmd "output -json"

-- ** Terraform Outputs
-- Terraform network module output.
data NetworkModule = NetworkModule
  { vpcID :: ID
  , dmzID :: ID
  , internalID :: ID } deriving (Show)

-- | Parse Terraform output JSON.
instance FromJSON NetworkModule where
  parseJSON = withObject "output" $ \o -> do
    vpcID      <- o .: "vpc_id"  >>= (.: "value")
    internalID <- o .: "subnets" >>= (.: "value") >>= (.: "internal")
    dmzID      <- o .: "subnets" >>= (.: "value") >>= (.: "dmz")

    return NetworkModule {..}

-- | Read Terraform network module output.
readNetworkOutput
  :: FilePath -- ^ json file
  -> Action (Maybe NetworkModule)
readNetworkOutput path = do
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
packerEnv :: NetworkModule -> [CmdOption]
packerEnv NetworkModule {..} =
  [ AddEnv "PACKER_BUILD_SUBNET" dmzID ]

rawPackerCommand :: CmdResult r => String -> Action r
rawPackerCommand args = do
  putQuiet $ "executing Packer: " ++ command
  cmd (Cwd "packer") command where
    command = "packer " ++ args

-- | Clean build directory.
clean :: Action ()
clean = do
  putQuiet $ "cleaning build directory"
  removeFilesAfter "_build" ["//*"]
