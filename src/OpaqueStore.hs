{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpaqueStore
  ( fromList,
    isEmpty,
    makeEmpty,
    OpaqueStore,
    peek,
    pop,
    push,
    replaceNext,
    Stack,
    updateNext,
  )
where

import Data.Maybe

-- | OpaqueStore is an abstraction over stacks and queues. A given type that implements OpaqueStore
-- | will either be a stack or a queue depending on the underlying container and implementations of
-- | the typeclass functions.
-- | Both the container type and element type are arguments to this typeclass so that instances can
-- | place additional constraints on both of them.
class OpaqueStore c a where
  fromList :: [a] -> c a
  fromList l = go l makeEmpty
    where
      go (v : vs) s = go vs (push v $! s)
      go [] s = s
  isEmpty :: c a -> Bool
  isEmpty = isNothing . peek
  makeEmpty :: c a
  push :: a -> c a -> c a
  peek :: c a -> Maybe a
  pop :: c a -> c a
  replaceNext :: a -> c a -> c a
  updateNext :: (a -> a) -> c a -> c a
  updateNext f s =
    case peek s of
      Just v -> replaceNext (f v) s
      Nothing -> s

-- | General stack data type.
-- | For the Ord instance, comparison starts at the top of the stack.
newtype Stack a = Stack [a]
  deriving (Eq, Ord, Show)

instance OpaqueStore Stack a where
  makeEmpty = Stack []
  push v (Stack c) = Stack $ v : c
  peek (Stack c) =
    case c of
      (v : _) -> Just v
      [] -> Nothing
  pop (Stack c) =
    case c of
      (_ : vs) -> Stack vs
      [] -> Stack []
  replaceNext v (Stack c) =
    case c of
      (_ : vs) -> Stack $ v : vs
      [] -> Stack [v]
