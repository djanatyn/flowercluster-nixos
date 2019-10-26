#!/usr/bin/env stack
{- stack script
  --resolver lts-14.2
  --package shake
  --package aeson
  --package text
  --package bytestring
-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}
module Grow where

import           Development.Shake
import           Development.Shake.FilePath

import qualified Data.ByteString.Lazy          as BL

import           Data.Aeson

import           qualified Data.Text as T

-- | AWS resource IDs are Strings.
type ID = String

-- | Terraform network module output.
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
readNetworkOutput ::
  FilePath -> -- ^ json file
  Action (Maybe NetworkModule)
readNetworkOutput path = do
  json <- liftIO $ BL.readFile path
  return $ decode json

-- | Execute Terraform command.
terraformCmd :: CmdResult r => String -> Action r
terraformCmd args = do
  putQuiet $ "executing '" ++ command ++ "'"
  cmd (Cwd "terraform") command where
    command = "terraform " ++ args

-- | Write Terraform output to path.
terraformOutput :: FilePath -> Action ()
terraformOutput path = do
  Stdout output <- terraformCmd "output -json"
  writeFileChanged path output

-- | Clean build directory.
clean :: Action ()
clean = do
  putQuiet $ "cleaning build directory"
  removeFilesAfter "_build" ["//*"]

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["_build/network-output.json"]

  phony "init" $ terraformCmd "init"

  phony "clean" $ clean

  phony "output" $ do
    need ["_build/output.json"]
    networkOutput <- readNetworkOutput "_build/network-output.json"
    putNormal $ show networkOutput

  "_build/network-output.json" %> terraformOutput
