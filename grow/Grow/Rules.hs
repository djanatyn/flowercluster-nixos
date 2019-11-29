module Grow.Rules (growRules) where

import Development.Shake
import qualified Grow.Packer.Actions as P
import qualified Grow.Terraform.Actions as T

-- | Clean build directory.
clean :: Action ()
clean = do
  putQuiet "cleaning build directory"
  removeFilesAfter "_build" ["//*"]

-- | Basic build rules.
basicRules :: Rules ()
basicRules = do
  phony "clean" clean
  phony "all" $ do
    need [T.outputPath]
    need [P.manifestPath]

-- | Build system rules.
growRules :: Rules ()
growRules = do
  basicRules
  T.terraformRules
  P.packerRules
