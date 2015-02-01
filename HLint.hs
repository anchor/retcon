{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}
module HLint.HLint where

import "hint" HLint.Builtin.All
import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Generalise

ignore "Use import/export shortcut"

ignore "Redundant bracket" = Domains.Rest.Client
