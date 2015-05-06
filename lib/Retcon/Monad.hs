--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Retcon.Monad where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Typeable

import           Retcon.Configuration
import           Retcon.DataSource


data RetconError
    = RetconFailed
    | RetconDBError String      -- ^ Describes an error with the retcon database.
    | RetconSourceError DataSourceError -- ^ An error occured with a data source.
    | RetconNotSupported String -- ^ An unsupported configuration.
    | RetconUnknown String      -- ^ Unknown entity, source, or document.
  deriving (Show, Typeable)

instance Exception RetconError

--------------------------------------------------------------------------------

-- | Environment for retcon to work in.
--
data RetconEnv = RetconEnv
    { _retconConfig :: Configuration }
makeLenses ''RetconEnv

-- | Monad for the Retcon system.
--
-- This monad provides access to an environment containing the state
-- of the retcon system and the state of the current action; error
-- handling through exceptions; and I/O.
newtype RetconMonad a = RetconMonad
  { _sync :: ReaderT RetconEnv (ExceptT RetconError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadReader RetconEnv,
            MonadError RetconError)

-- | Execute an action in the 'RetconMonad' monad.
runRetconMonad
  :: RetconEnv
  -> RetconMonad a
  -> IO (Either RetconError a)
runRetconMonad env = runExceptT . flip runReaderT env . _sync
