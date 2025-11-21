{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Stream
  ( Stream (..),
    foldStream,
    foldStream_,
    mapStream_,
  )
where

import Control.Exception
import Data.Bifunctor

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
