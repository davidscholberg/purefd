{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Stream
  ( Stream (..),
    concatIterateIO,
    foldStream,
    foldStream_,
    mapStream_,
  )
where

import Control.Exception
import Data.Bifunctor
import Data.Function
import qualified Sequential as S

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

closeStateSequence :: S.Sequential (StreamFrame a) -> IO ()
closeStateSequence stateSequence = do
  case S.peek stateSequence of
    Just (StreamFrame currentState _ currentClose) ->
      currentClose currentState `finally` closeStateSequence (S.pop stateSequence)
    Nothing -> pure ()

-- Create stream that prepends new streams that are found by applying the supplied f to one of the
-- stream values. Useful for recursive depth-first traversal of (for example) a directory tree.
-- Stole this function idea straight from streamly lmaooooooooo
concatIterateIO :: (a -> IO (Maybe (Stream a))) -> Stream a -> Stream a
concatIterateIO f (Stream seedOpen seedNext seedClose) =
  Stream
    { open = do
        seedState <- seedOpen
        let stateStack = S.makeStack :: S.Sequential (StreamFrame a)
        pure $ S.push (StreamFrame seedState seedNext seedClose) stateStack,
      next = fix $ \self stateStack -> do
        case S.peek stateStack of
          Just (StreamFrame currentState currentNext currentClose) -> do
            maybeResult <- currentNext currentState
            case maybeResult of
              Just (v, currentState') -> do
                maybeStream <- f v `onException` (currentClose currentState' `finally` closeStateSequence (S.pop stateStack))
                case maybeStream of
                  Just (Stream newOpen newNext newClose) -> do
                    newState <- newOpen `onException` (currentClose currentState' `finally` closeStateSequence (S.pop stateStack))
                    pure $ Just (v, S.push (StreamFrame newState newNext newClose) $ S.replaceFirst (StreamFrame currentState' currentNext currentClose) stateStack)
                  Nothing -> pure $ Just (v, S.replaceFirst (StreamFrame currentState' currentNext currentClose) stateStack)
              Nothing -> do
                -- Normally we wouldn't catch and handle exceptions from next calls (since they're
                -- supposed to do their own cleanup), but in this case we can't clean up the current
                -- stream until we verify that the recursive next call below does not return Nothing.
                -- As a result, we catch any exception and close the stream that was left dangling in
                -- the current recursive call frame.
                maybeResult' <- self (S.pop stateStack) `onException` currentClose currentState
                case maybeResult' of
                  Just result@(_, stateStack') -> do
                    currentClose currentState `onException` closeStateSequence stateStack'
                    pure $ Just result
                  Nothing -> pure Nothing
          Nothing -> pure Nothing,
      close = \stateStack -> do
        closeStateSequence stateStack
    }
