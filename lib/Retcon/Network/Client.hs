--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | A typeclass and IO implementation of a client API for Retcon
--
-- With this API you will be able to:
-- * List conflicted diffs
-- * Force a conflicted diff to resolve with any operations you chose
--   pushed upstream.
-- * Notfiy retcon of datasources that need to be updated
--
-- Example usage:
--
-- @
--      main :: IO ()
--      main = do
--          runRetconZMQ "tcp://10.2.3.4:1234" $ do
--              conflicts <- getConflicted
--              liftIO . putStrLn $
--                  "Got " ++ show (length conflicts) ++ " conflicts."
--
--              \-\- This is probably a terrible idea
--              forM_ conflicts $ \\(_, _, diff_id, _) ->
--                  enqueueResolveDiff diff_id []
--
--              liftIO $ putStrLn "Marked them all resolved."
--          >>= either throwIO return
--
--
-- @
module Retcon.Network.Client
(
    -- * Operations
    getConflicted,
    enqueueResolveDiff,
    enqueueChangeNotification,
    flushWorkQueue,

    runRetconZMQ,
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Binary
import Data.ByteString.Lazy (fromStrict)
import Data.List.NonEmpty
import System.ZMQ4.Monadic

import Retcon.Diff
import Retcon.Document
import Retcon.Network.Server hiding (liftZMQ)

-- | Retrieve all documents that are currently marked as being conflicted
getConflicted
    :: (RetconClientConnection m, MonadError RetconAPIError m)
    =>  m [( Document
           , Diff ()
           , DiffID
           , [(ConflictedDiffOpID, DiffOp ())]
          )]
getConflicted = do
    ResponseConflicted response <- performRequest HeaderConflicted RequestConflicted
    return response

-- | Tell Retcon to apply the given operations upstream at some point
enqueueResolveDiff
    :: (RetconClientConnection m, MonadError RetconAPIError m)
    => DiffID
    -> [ConflictedDiffOpID]
    -> m ()
enqueueResolveDiff did ops =
    void $ performRequest HeaderResolve (RequestResolve did ops)

-- | Notify Retcon of an external change
enqueueChangeNotification
    :: (RetconClientConnection m, MonadError RetconAPIError m)
    => ChangeNotification
    -> m ()
enqueueChangeNotification notification =
    void $ performRequest HeaderChange (RequestChange notification)

-- | Tell Retcon to flush the work queue, processing all items immediately.
--
-- Returns the number of work items processed.
flushWorkQueue
    :: (RetconClientConnection m, MonadError RetconAPIError m)
    => m Int
flushWorkQueue = do
    ResponseFlushWork n <- performRequest HeaderFlushWork RequestFlushWork
    return n

newtype RetconClientZMQ z a =
    RetconClientZMQ {
        unRetconClientZMQ :: ExceptT RetconAPIError (ReaderT (Socket z Req) (ZMQ z)) a
      } deriving ( Functor, Applicative, Monad, MonadError RetconAPIError
                 , MonadReader (Socket z Req), MonadIO)


-- | This typeclass provides an abstraction for sending messages to and
-- recieving messages from a Retcon server.
class (MonadError RetconAPIError m, Functor m)
        => RetconClientConnection m where
    performRequest :: (Binary request, Binary response)
                   => Header request response -> request -> m response


liftZMQ :: ZMQ z a -> RetconClientZMQ z a
liftZMQ = RetconClientZMQ . lift . lift

-- | Concrete implementation of RetconClientConnection for an established ZMQ
-- monad connection.
instance RetconClientConnection (RetconClientZMQ z) where
    performRequest header request = do
        let n = encodeStrict $ fromEnum (SomeHeader header)
            req = encodeStrict request

        soc <- ask
        liftZMQ . sendMulti soc . fromList $ [n, req]
        response <- liftZMQ . receiveMulti $ soc
        case response of
            [success,body]
                | decode . fromStrict $ success ->
                    decodeStrict body
                | otherwise -> throwError =<< (toEnum <$> decodeStrict body)
            _ -> throwError InvalidNumberOfMessageParts

-- | Set up a connection to the target and then run some ZMQ action
runRetconZMQ
    :: forall a.
       String -- ^ ZMQ connection target, e.g. \"tcp://127.0.0.1:1234\"
    -> (forall z. RetconClientZMQ z a)
    -> IO (Either RetconAPIError a)
runRetconZMQ target action = runZMQ $ do
        soc <- socket Req
        connect soc target
        let action' = runExceptT $ unRetconClientZMQ action
        x <- runReaderT action' soc
        disconnect soc target
        close soc
        return x
