--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Monad for retcon handlers.
--
-- This module implements the monad in which the main "handler" actions
-- execute. This monad provides the following effects:
--
-- - Error handling
-- - Logging
-- - Accessing configuration
-- - I/O

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Retcon.Monad where

import Control.Applicative
import Control.Exception.Enclosed
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Retcon.Config
import Retcon.DataSource
import Retcon.Error
import Retcon.Options
import Retcon.Store

-- | Run an action in 'RetconHandler', catching any exceptions and propagating
-- them as 'RetconError's.
carefully :: RetconHandler s a -> RetconHandler s a
carefully = handleAny (throwError . RetconError)

-- * Handler monad

-- $ Retcon handlers (i.e. actions which process and response to an event) are
-- actions in the 'RetconHandler' monad.

-- | State held in the reader part of the 'RetconHandler' monad.
data RetconStore s => RetconState s = RetconState
    { retconOptions :: RetconOptions
    , retconState   :: [InitialisedEntity]
    , retconStore   :: s
    }

-- | Monad transformer stack used in the 'RetconHandler' monad.
type RetconHandlerStack s = ExceptT RetconError (LoggingT (ReaderT (RetconState s) IO))

-- | Monad for the retcon system.
--
-- This monad provides error handling (by throwing 'HandlerLog' exceptions),
-- logging (write 'HandlerLog'), access to configuration and PostgreSQL
-- connections (reader), and IO facilities.
newtype RetconHandler s a =
    RetconHandler {
        unRetconHandler :: (RetconHandlerStack s) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger,
  MonadReader (RetconState s), MonadError RetconError, MonadBase IO)

-- | MonadBaseControl used to catch IO exceptions
instance MonadBaseControl IO (RetconHandler s) where
    newtype StM (RetconHandler s) a = StHandler {
        unStHandler :: StM (RetconHandlerStack s) a
    }

    -- Unwrap the bits in our monad to get to the base.
    liftBaseWith f = RetconHandler . liftBaseWith $ \r ->
        f $ liftM StHandler . r . unRetconHandler

    -- Wrap things back up into our monad.
    restoreM       = RetconHandler . restoreM . unStHandler

-- | Run a 'RetconHandler' action with the given state.
runRetconHandler' :: RetconStore s
                  => RetconOptions
                  -> [InitialisedEntity]
                  -> s
                  -> RetconHandler s a
                  -> IO (Either RetconError a)
runRetconHandler' opt state store (RetconHandler a) =
    flip runReaderT (RetconState opt state store) . runLogging . runExceptT $ a
  where
    runLogging = case optLogging opt of
      LogStderr -> runStderrLoggingT
      LogStdout -> runStdoutLoggingT
      LogNone   -> (`runLoggingT` \_ _ _ _ -> return ())

-- | Run a 'RetconHandler' action with the given configuration.
runRetconHandler :: RetconStore s
                  => RetconOptions
                  -> RetconConfig
                  -> s
                  -> RetconHandler s a
                  -> IO (Either RetconError a)
runRetconHandler opt cfg store a = do
    state <- initialiseEntities (retconEntities cfg)
    ret <- runRetconHandler' opt state store a
    _ <- finaliseEntities state
    return ret

-- * Action monad

-- $ Data source operations execute in the 'DataSourceAction' monad. This monad
-- allows a restricted range of effects, helping to isolate code which
-- interacts with external systems and minimising their potential to
-- accidentally interfere with retcon's internal data.

-- | Run a 'DataSourceAction' action.
runDataSourceAction :: state
                    -> DataSourceAction state a
                    -> RetconHandler s (Either RetconError a)
runDataSourceAction s =
    liftIO
    . runStderrLoggingT
    . runExceptT
    . flip runReaderT s
    . unDataSourceAction

