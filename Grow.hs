#!/usr/bin/env stack
{- stack script
  --resolver lts-14.2
  --package shake
-}
module Grow where

import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["_build/output.json"]

  "terraform/.terraform" %> \out -> do
    cmd (Cwd "terraform") "terraform init"

  "_build/output.json" %> \out -> do
    need ["terraform/.terraform"]

    Stdout output <- cmd (Cwd "terraform") "terraform output -json"
    writeFileChanged "_build/output.json" output
  
