{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Stream
  ( Stream (..),
    concatIterateIO,
    foldStream,
    foldStream_,
    mapStream_,
    parConcatIterateIO,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.PQueue.Prio.Min
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

parConcatIndexToKey :: Stack Integer -> [Integer]
parConcatIndexToKey = reverse . OpaqueStore.toList

type ParConcatResultQueue a =
  MinPQueue
    [Integer]
    (Maybe a, [Integer])

type ParConcatStreamQueue a =
  Queue (Stream a, Stack Integer)

data ParConcatState a
  = ParConcatState
      (TVar (ParConcatStreamQueue a))
      (TVar (ParConcatResultQueue a))
      [Async ()]
      [Integer]

parConcatStreamWorker ::
  (a -> Maybe (Stream a)) ->
  TVar (ParConcatStreamQueue a) ->
  TVar (ParConcatResultQueue a) ->
  IO ()
parConcatStreamWorker f streamQ resultQ = do
  (Stream streamOpen streamNext streamClose, initialIndex) <-
    atomically $ do
      q <- readTVar streamQ
      case peek q of
        Just v -> do
          writeTVar streamQ $ pop q
          pure v
        Nothing -> retry
  initialState <- streamOpen
  go (StreamFrame initialState streamNext streamClose) initialIndex
  parConcatStreamWorker f streamQ resultQ
  where
    go (StreamFrame streamState streamNext streamClose) qindex = do
      let qkey = parConcatIndexToKey qindex
      maybeValue <- streamNext streamState
      case maybeValue of
        Just (v, streamState') -> do
          let qindex' = updateNext (+ 1) $! qindex
          let qkey' = parConcatIndexToKey qindex'
          let maybeNewStream = f v
          ( case maybeNewStream of
              Just newStream -> do
                let childIndex = push 0 qindex
                let childKey = parConcatIndexToKey childIndex
                atomically $ do
                  q <- readTVar resultQ
                  writeTVar resultQ (insert qkey (Just v, childKey) q)
                atomically $ do
                  q <- readTVar streamQ
                  writeTVar streamQ (push (newStream, childIndex) q)
              Nothing -> do
                atomically $ do
                  q <- readTVar resultQ
                  writeTVar resultQ (insert qkey (Just v, qkey') q)
            )
            `onException` streamClose streamState'
          go (StreamFrame streamState' streamNext streamClose) $! qindex'
        Nothing -> do
          let parentNextKey = parConcatIndexToKey $ updateNext (+ 1) $ pop qindex
          atomically
            ( do
                q <- readTVar resultQ
                writeTVar resultQ (insert qkey (Nothing, parentNextKey) q)
            )
            `onException` streamClose streamState
          streamClose streamState

-- NOTE: It is the callers responsibility to ensure that any part of a stream element that relies on
-- thread-local resources has its evaluation sufficiently forced to avoid leaking those resources to
-- other threads.
-- TODO: We could place a constraint on the stream element to have an instance of NFData and then
-- the thread worker could force all stream elements to NF before enqueueing them.
parConcatIterateIO :: (a -> Maybe (Stream a)) -> Stream a -> Stream a
parConcatIterateIO f seedStream =
  Stream
    { open = do
        streamQ <- newTVarIO (makeEmpty :: ParConcatStreamQueue a)
        resultQ <- newTVarIO (empty :: ParConcatResultQueue a)
        pure $ ParConcatState streamQ resultQ [] [0],
      next = fix $ \self (ParConcatState streamQ resultQ threads expectedKey) -> do
        if not $ Prelude.null threads
          then do
            (maybeValue, nextKey) <-
              atomically
                ( do
                    q <- readTVar resultQ
                    case q of
                      (k, v) :< q' ->
                        if k == expectedKey
                          then do
                            writeTVar resultQ q'
                            pure v
                          else
                            retry
                      Empty -> retry
                )
                `onException` cancelMany threads
            case maybeValue of
              Just v ->
                pure $ Just (v, ParConcatState streamQ resultQ threads nextKey)
              Nothing -> do
                if Prelude.null nextKey
                  then
                    pure Nothing
                  else
                    self $ ParConcatState streamQ resultQ threads nextKey
          else do
            threadCount <- getNumCapabilities
            newThreads <-
              replicateM
                threadCount
                ( do
                    t <- asyncBound $ parConcatStreamWorker f streamQ resultQ
                    link t
                    pure t
                )
            atomically (modifyTVar' streamQ (push (seedStream, push 0 makeEmpty))) `onException` cancelMany newThreads
            self $ ParConcatState streamQ resultQ newThreads expectedKey,
      close = \(ParConcatState _ _ threads _) ->
        unless
          (Prelude.null threads)
          (cancelMany threads)
    }
