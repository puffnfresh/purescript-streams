module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Control.Process

main = run (repeatedly (eval (log "Running...")))
