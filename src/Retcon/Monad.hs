--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: A monad for data source action.
--
-- This module implements a monad in which data source operations will be
-- executed.

module Retcon.Monad where

type Retcon a = IO a
