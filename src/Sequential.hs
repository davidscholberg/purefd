{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Sequential
  ( Sequential,
    makeStack,
    peek,
    pop,
    push,
    replaceFirst,
  )
where

-- Sequential is an abstraction over stacks and queues. A given sequential object will either be a
-- stack or a queue depending on the container and functions supplied to the object's constructor.
-- There's likely a better name for this than "sequential" but fuck it we ball.
data Sequential a = forall c. Sequential
  { _container :: c,
    _push :: a -> c -> c,
    _peek :: c -> Maybe a,
    _pop :: c -> c,
    _replaceFirst :: a -> c -> c
  }

makeStack :: Sequential a
makeStack =
  Sequential
    { _container = [],
      _push = (:),
      _peek = \case
        (v : _) -> Just v
        [] -> Nothing,
      _pop = \case
        (_ : vs) -> vs
        [] -> [],
      _replaceFirst = \v c ->
        case c of
          (_ : vs) -> v : vs
          [] -> [v]
    }

push :: a -> Sequential a -> Sequential a
push v (Sequential c push' peek' pop' replaceFirst') = Sequential (push' v c) push' peek' pop' replaceFirst'

peek :: Sequential a -> Maybe a
peek (Sequential c _ peek' _ _) = peek' c

pop :: Sequential a -> Sequential a
pop (Sequential c push' peek' pop' replaceFirst') = Sequential (pop' c) push' peek' pop' replaceFirst'

replaceFirst :: a -> Sequential a -> Sequential a
replaceFirst v (Sequential c push' peek' pop' replaceFirst') = Sequential (replaceFirst' v c) push' peek' pop' replaceFirst'
