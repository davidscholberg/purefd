{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Stream
  ( Stream (..),
    concatIterateIO,
    foldStream,
    foldStream_,
    mapStream_,
    parConcatIterate,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Exception
import Data.Bifunctor
import Data.Function
import OpaqueStore

-- Data type representing a stream of data. The open function acquires the stream resource, the
-- next function returns the next object from the stream as well as the (potentially) updated stream
-- resource, and the close function releases the stream resource.
-- All implementations of next must ensure that the stream resource is closed upon any exception
-- that occurs within the body.
data Stream a = forall s. Stream
  { open :: IO s,
    next :: s -> IO (Maybe (a, s)),
    close :: s -> IO ()
  }

-- Streams can be concatted together.
-- TODO: This is not a performant way to concat together many streams since calls to next on the
-- concatted stream may have to traverse a deep call stack just to retrieve the next value. It's
-- much more efficient to concat streams by using a sequence of streams or stream frames as the
-- state object of the concatted stream.
instance Semigroup (Stream a) where
  Stream open1 next1 close1 <> Stream open2 next2 close2 =
    Stream
      { open = Left <$> open1,
        next = \case
          Left state1 -> do
            result1 <- next1 state1
            case result1 of
              Just (v, state1') ->
                pure $ Just (v, Left state1')
              Nothing -> do
                close1 state1
                state2 <- open2
                result2 <- next2 state2
                pure $ second Right <$> result2
          Right state2 -> do
            result2 <- next2 state2
            pure $ second Right <$> result2,
        close = \case
          Left state1 -> close1 state1
          Right state2 -> close2 state2
      }

-- Streams can be concattable and empty.
instance Monoid (Stream a) where
  mempty =
    Stream
      { open = pure (),
        next = \_ -> pure Nothing,
        close = \_ -> pure ()
      }

-- Streams can be mapped over by pure functions.
instance Functor Stream where
  fmap f (Stream open' next' close') =
    Stream
      { open = open',
        next = \state -> do
          result <- next' state
          pure $ first f <$> result,
        close = close'
      }

-- Similar to a Stream except that it carries its own state with it. Useful for combining multiple
-- streams into one on the fly.
-- All implementations of nextF must ensure that the stream resource is closed upon any exception
-- that occurs within the body.
data StreamFrame a = forall s. StreamFrame
  { stateF :: s,
    nextF :: s -> IO (Maybe (a, s)),
    closeF :: s -> IO ()
  }

-- StreamFrames can be concatted together.
-- TODO: This has the same performance problems as the Semigroup instance for Streams. The
-- concatIterateIO function has a much more performant example of concatting stream frames (by
-- maintaining a stack of stream frames).
instance Semigroup (StreamFrame a) where
  StreamFrame seedState1 next1 close1 <> StreamFrame seedState2 next2 close2 =
    StreamFrame
      { stateF = Left seedState1,
        nextF = \case
          Left state1 -> do
            result1 <- next1 state1 `onException` close2 seedState2
            case result1 of
              Just (v, state1') ->
                pure $ Just (v, Left state1')
              Nothing -> do
                result2 <- next2 seedState2 `onException` close1 state1
                case result2 of
                  Just (v, state2) -> do
                    close1 state1 `onException` close2 state2
                    pure $ Just (v, Right state2)
                  Nothing -> pure Nothing
          Right state2 -> do
            result2 <- next2 state2
            pure $ second Right <$> result2,
        closeF = \case
          Left state1 -> do
            close1 state1 `finally` close2 seedState2
          Right state2 -> do
            close2 state2
      }

-- StreamFrames can be concattable and empty.
instance Monoid (StreamFrame a) where
  mempty =
    StreamFrame
      { stateF = (),
        nextF = \_ -> pure Nothing,
        closeF = \_ -> pure ()
      }

-- Left associative strict fold over a stream.
-- This function will close the stream resource upon any exception from the supplied f.
foldStream :: (b -> a -> IO b) -> b -> Stream a -> IO b
foldStream f identity (Stream open' next' close') = do
  initialState <- open'
  (result, finalState) <- go identity initialState
  close' finalState
  pure result
  where
    go acc state = do
      maybeValue <- next' state
      case maybeValue of
        Just (v, state') -> do
          acc' <- f acc v `onException` close' state'
          go acc' state'
        Nothing -> pure (acc, state)

-- Same as foldStream except the results are discarded.
foldStream_ :: (b -> a -> IO b) -> b -> Stream a -> IO ()
foldStream_ f identity stream = do
  _ <- foldStream f identity stream
  pure ()

-- Map stream values to IO actions and discard the results.
-- This function will close the stream resource upon any exception from the supplied f.
mapStream_ :: (a -> IO b) -> Stream a -> IO ()
mapStream_ f = foldStream_ (const f) undefined

closeStateStore :: OpaqueStore c (StreamFrame a) => c (StreamFrame a) -> IO ()
closeStateStore stateStore = do
  case peek stateStore of
    Just (StreamFrame currentState _ currentClose) ->
      currentClose currentState `finally` closeStateStore (pop stateStore)
    Nothing -> pure ()

-- Create stream that prepends new streams that are found by applying the supplied f to one of the
-- stream values. Useful for recursive depth-first traversal of (for example) a directory tree.
-- Stole this function idea straight from streamly lmaooooooooo
concatIterateIO :: (a -> Maybe (Stream a)) -> Stream a -> Stream a
concatIterateIO f (Stream seedOpen seedNext seedClose) =
  Stream
    { open = do
        seedState <- seedOpen
        let stateStack = makeEmpty :: Stack (StreamFrame a)
        pure $ push (StreamFrame seedState seedNext seedClose) stateStack,
      next = fix $ \self stateStack -> do
        case peek stateStack of
          Just (StreamFrame currentState currentNext currentClose) -> do
            maybeResult <- currentNext currentState
            case maybeResult of
              Just (v, currentState') -> do
                let maybeStream = f v
                case maybeStream of
                  Just (Stream newOpen newNext newClose) -> do
                    newState <- newOpen `onException` (currentClose currentState' `finally` closeStateStore (pop stateStack))
                    pure $ Just (v, push (StreamFrame newState newNext newClose) $ replaceNext (StreamFrame currentState' currentNext currentClose) stateStack)
                  Nothing -> pure $ Just (v, replaceNext (StreamFrame currentState' currentNext currentClose) stateStack)
              Nothing -> do
                -- Normally we wouldn't catch and handle exceptions from next calls (since they're
                -- supposed to do their own cleanup), but in this case we can't clean up the current
                -- stream until we verify that the recursive next call below does not return Nothing.
                -- As a result, we catch any exception and close the stream that was left dangling in
                -- the current recursive call frame.
                maybeResult' <- self (pop stateStack) `onException` currentClose currentState
                case maybeResult' of
                  Just result@(_, stateStack') -> do
                    currentClose currentState `onException` closeStateStore stateStack'
                    pure $ Just result
                  Nothing -> pure Nothing
          Nothing -> pure Nothing,
      close = \stateStack -> do
        closeStateStore stateStack
    }

data ParConcatState a b
  = ParConcatState
      (TVar (Queue (Stream a)))
      (TVar (Queue ([b], Integer)))
      (TVar Int)
      (TVar Int)
      [Async ()]

parConcatStreamWorker ::
  (a -> Maybe (Stream a)) ->
  (a -> Maybe b) ->
  TVar (Queue (Stream a)) ->
  TVar (Queue ([b], Integer)) ->
  TVar Int ->
  TVar Int ->
  Integer ->
  IO ()
parConcatStreamWorker newStreamF pipelineF streamQ resultQ activeTCount pendingTCount batchSize =
  go1 ([], 0)
  where
    go1 batch@(_, currentBatchSize) = do
      maybeStream <-
        atomically $ do
          q <- readTVar streamQ
          case peek q of
            Just v -> do
              writeTVar streamQ $ pop q
              modifyTVar' activeTCount (+1)
              pure $ Just v
            Nothing -> do
              tc <- readTVar activeTCount
              if tc > 0
                then
                  retry
                else do
                  when
                    (currentBatchSize > 0)
                    (modifyTVar' resultQ (push batch))
                  modifyTVar' pendingTCount (subtract 1)
                  pure Nothing
      case maybeStream of
        Just (Stream streamOpen streamNext streamClose) -> do
          initialState <- streamOpen
          batch' <- go2 (StreamFrame initialState streamNext streamClose) batch
          go1 batch'
        Nothing ->
          pure ()
    go2 (StreamFrame streamState streamNext streamClose) batch@(batchList, currentBatchSize) = do
      maybeValue <- streamNext streamState
      case maybeValue of
        Just (v, streamState') -> do
          let maybeNewStream = newStreamF v
          case maybeNewStream of
            Just newStream ->
              atomically (modifyTVar' streamQ (push newStream)) `onException` streamClose streamState'
            Nothing -> pure ()
          case pipelineF v of
            Just !pipelinedV -> do
              let batch' = (pipelinedV : batchList, currentBatchSize + 1)
              if currentBatchSize < batchSize - 1
                then
                  go2 (StreamFrame streamState' streamNext streamClose) batch'
                else do
                  atomically (modifyTVar' resultQ (push batch')) `onException` streamClose streamState'
                  go2 (StreamFrame streamState' streamNext streamClose) ([], 0)
            Nothing ->
              go2 (StreamFrame streamState' streamNext streamClose) batch
        Nothing -> do
          streamClose streamState
          atomically (modifyTVar' activeTCount (subtract 1))
          pure batch

-- NOTE: The stream returned by this function does not guarantee a particular order to the stream
-- elements.
-- NOTE: It is the callers responsibility to ensure that any part of a stream element that relies on
-- thread-local resources has its evaluation sufficiently forced to avoid leaking those resources to
-- other threads.
-- TODO: We could place a constraint on the stream element to have an instance of NFData and then
-- the thread worker could force all stream elements to NF before enqueueing them.
parConcatIterate :: (a -> Maybe (Stream a)) -> (a -> Maybe b) -> (Int -> Int) -> Integer -> Stream a -> Stream ([b], Integer)
parConcatIterate newStreamF pipelineF threadCountF batchSize seedStream =
  Stream
    { open = do
        streamQ <- newTVarIO (makeEmpty :: Queue (Stream a))
        resultQ <- newTVarIO (makeEmpty :: Queue ([b], Integer))
        activeTCount <- newTVarIO (0 :: Int)
        pendingTCount <- newTVarIO (0 :: Int)
        atomically (modifyTVar' streamQ (push seedStream))
        pure $ ParConcatState streamQ resultQ activeTCount pendingTCount [],
      next = fix $ \self (ParConcatState streamQ resultQ activeTCount pendingTCount threads) -> do
        if not $ Prelude.null threads
          then do
            maybeValue <-
              atomically
                ( do
                    rq <- readTVar resultQ
                    case peek rq of
                      Just v -> do
                        writeTVar resultQ $ pop rq
                        pure $ Just v
                      Nothing -> do
                        tc <- readTVar pendingTCount
                        if tc > 0
                          then
                            retry
                          else
                            pure Nothing
                )
                `onException` cancelMany threads
            case maybeValue of
              Just v ->
                pure $ Just (v, ParConcatState streamQ resultQ activeTCount pendingTCount threads)
              Nothing ->
                pure Nothing
          else do
            threadCount <- fmap threadCountF getNumCapabilities
            atomically $ writeTVar pendingTCount threadCount
            newThreads <-
              replicateM
                threadCount
                ( do
                    t <- async $ parConcatStreamWorker newStreamF pipelineF streamQ resultQ activeTCount pendingTCount batchSize
                    link t
                    pure t
                )
            self $ ParConcatState streamQ resultQ activeTCount pendingTCount newThreads,
      close = \(ParConcatState _ _ _ _ threads) ->
        unless
          (Prelude.null threads)
          (cancelMany threads)
    }
