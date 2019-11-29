-- |
-- Module      : Grow
-- Description : Grows the infrastructure for flowercluster.
--
-- This module compiles to a build system used to maintain flowercluster, a digital
-- garden.
--
-- The build system manages tasks such as:
--
--   * building infrastructure from Terraform modules,
--   * parsing Terraform output files,
--   * building AMI images from Packer templates,
--   * parsing Packer manifest files.
--
-- Why maintain this complex build system?
--
--   * Builds are reproducible: build artifacts can be reconstructed and updated
--   * Builds are safe: each action in a build rule is typed
--   * Builds are composable: complex build actions are defined by combining
--     smaller actions
--   * Builds are extensible: the build system is a Haskell library
module Grow where

import Development.Shake
import Grow.Rules

-- | Flowercluster build system.
main :: IO ()
main = shakeArgs shakeOptions {shakeColor = True} growRules
