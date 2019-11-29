module Grow.Terraform.Actions
  ( -- * Execute Actions
    runTerraform,

    -- * Terraform Output
    readTerraformOutput,

    -- * Managed Terraform Files
    planPath,
    outputPath,
   
    -- * Build Rules
    terraformRules,
  )
where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Development.Shake
import Development.Shake.Command (IsCmdArgument)
import Grow.Types (TerraformCmd (..), TerraformOutput (..))
import System.Directory (makeAbsolute)

-- | Run arbitrary Terraform command.
rawTerraformCmd :: (CmdResult r, IsCmdArgument args) => args -> Action r
rawTerraformCmd = cmd (Cwd "terraform") "terraform"

-- | Run type-safe Terraform command.
runTerraform :: CmdResult r => TerraformCmd -> Action r
runTerraform Init = rawTerraformCmd "init"
runTerraform (Output path) =
  rawTerraformCmd ["output", "-json", "out=" ++ path]
runTerraform (Plan path) = do
  outPath <- liftIO $ makeAbsolute path
  rawTerraformCmd ["plan", "-out=" ++ outPath]
runTerraform (Apply path) = do
  applyPath <- liftIO $ makeAbsolute path
  rawTerraformCmd ["apply", applyPath]

-- | Write Terraform output to path.
terraformOutput ::
  -- | JSON Terraform output path.
  FilePath ->
  Action ()
terraformOutput path = do
  Stdout output <- rawTerraformCmd "output -json"
  writeFileChanged path output

-- | Read Terraform module output.
readTerraformOutput ::
  -- | JSON Terraform output path.
  FilePath ->
  Action (Maybe TerraformOutput)
readTerraformOutput path = liftIO $ decode <$> BL.readFile path

-- | Managed Terraform plan path.
planPath :: FilePath
planPath = "_build/grow.plan"

-- | Managed Terraform output path.
outputPath :: FilePath
outputPath = "_build/growOutput.json"

-- | Terraform build rules.
terraformRules :: Rules ()
terraformRules = do
  -- | Initialize Terraform.
  phony "init" (runTerraform Init)
  -- | Build the managed Terraform plan file.
  phony "plan" (runTerraform $ Plan planPath)
  planPath %> runTerraform . Plan
  -- | Apply the managed Terraform plan file.
  phony "apply" $ do
    need [planPath]
    runTerraform (Apply planPath)
  -- | Build the managed Terraform output file.
  phony "output" $ do
    need [outputPath]
    out <- readTerraformOutput outputPath
    putNormal (show out)
  outputPath %> terraformOutput
