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

import           Data.Text

type ID = String

data NetworkModule = NetworkModule
  { vpcID :: ID
  , dmzID :: ID
  , internalID :: ID } deriving (Show)

instance FromJSON NetworkModule where
  parseJSON = withObject "output" $ \o -> do
    vpcID      <- o .: "vpc_id"  >>= (.: "value")
    internalID <- o .: "subnets" >>= (.: "value") >>= (.: "internal")
    dmzID      <- o .: "subnets" >>= (.: "value") >>= (.: "dmz")

    return NetworkModule {..}

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["_build/output.json"]

  phony "init" $ do
    putNormal "initializing terraform"
    cmd (Cwd "terraform") "terraform init" :: Action ()

  phony "clean" $ do
    putNormal "cleaning"
    removeFilesAfter "_build" ["//*"]

  phony "output" $ do
    need ["_build/output.json"]

    json <- liftIO $ BL.readFile "_build/output.json"
    putNormal $ show $ (decode json :: Maybe NetworkModule)

  "_build/output.json" %> \out -> do
    Stdout output <- cmd (Cwd "terraform") "terraform output -json"
    writeFileChanged "_build/output.json" output
