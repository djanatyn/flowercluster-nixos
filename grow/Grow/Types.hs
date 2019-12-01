{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Grow.Types
  ( -- * Type Synonyms
    ID,

    -- * Terraform

    -- ** Build Actions
    TerraformCmd (..),

    -- ** Build Output
    TerraformOutput (..),

    -- * Packer

    -- ** Build Configuration
    PackerConfig (..),

    -- ** Build Actions
    PackerCmd (..),

    -- ** Build Output
    PackerManifest (..),
  )
where

import Data.Aeson
import Data.Text (Text)

-- | AWS IDs are Strings.
type ID = String

-- | Terraform actions.
data TerraformCmd = Init | Plan FilePath | Apply FilePath | Output FilePath
  deriving (Show)

-- | Terraform output.
data TerraformOutput
  = TerraformOutput
      { terraformVPC :: ID,
        terraformDMZSubnet :: ID,
        terraformInternalSubnet :: ID,
        terraformDMZSecurityGroup :: ID
      }
  deriving (Show)

-- | Parse Terraform output JSON.
instance FromJSON TerraformOutput where
  parseJSON = withObject "output" $ \o -> do
    terraformVPC <-
      o .: "vpc_id" >>= (.: "value")
    terraformInternalSubnet <-
      o .: "subnets" >>= (.: "value") >>= (.: "internal")
    terraformDMZSubnet <-
      o .: "subnets" >>= (.: "value") >>= (.: "dmz")
    terraformDMZSecurityGroup <-
      o .: "security_groups" >>= (.: "value") >>= (.: "dmz_ssh")
    return TerraformOutput
      { terraformVPC,
        terraformDMZSubnet,
        terraformInternalSubnet,
        terraformDMZSecurityGroup
      }

data PackerManifest
  = PackerManifest
      { packerUUID :: Text,
        packerBuilds :: [PackerBuild]
      }
  deriving (Show)

data PackerBuild
  = PackerBuild
      { buildName :: Text,
        buildTime :: Int,
        buildArtifact :: Text,
        buildUUID :: Text
      }
  deriving (Show)

instance FromJSON PackerBuild where
  parseJSON = withObject "build" $ \build -> do
    buildName <- build .: "name"
    buildTime <- build .: "build_time"
    buildArtifact <- build .: "artifact_id"
    buildUUID <- build .: "packer_run_uuid"
    return PackerBuild {buildName, buildTime, buildArtifact, buildUUID}

instance FromJSON PackerManifest where
  parseJSON = withObject "manifest" $ \manifest -> do
    packerBuilds <- manifest .: "builds"
    packerUUID <- manifest .: "last_run_uuid"
    return PackerManifest {packerBuilds, packerUUID}

-- | Packer config.
data PackerConfig
  = PackerConfig
      { packerEnv :: TerraformOutput,
        packerManifestPath :: FilePath,
        packerImageName :: String
      }
  deriving (Show)

-- | Packer actions.
data PackerCmd = Build FilePath
  deriving (Show)
