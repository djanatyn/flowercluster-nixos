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

    -- ** Build Actions
    PackerCmd (..),
  )
where

import Data.Aeson

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

-- | Packer actions.
data PackerCmd = Build FilePath
  deriving (Show)
