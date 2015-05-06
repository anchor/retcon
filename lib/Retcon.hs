--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Re-export the public API to /Retcon/.
module Retcon (
    R.version,

    -- * Configuration
    module Retcon.Configuration,
    -- * Identifiers
    module Retcon.Identifier,
    -- * Diffs
    module Retcon.Diff,
    -- * Documents
    module Retcon.Document,
    -- * Data sources
    module Retcon.DataSource,
) where

import           Retcon.Configuration
import           Retcon.DataSource
import           Retcon.Diff
import           Retcon.Document
import           Retcon.Identifier

import           Paths_retcon         as R

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
