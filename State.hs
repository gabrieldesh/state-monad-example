module State
( State(..) ) where

data State s a = State { unwrap :: s -> (a, s) }

instance Functor (State s) where
  fmap f x = State (\s ->
                    let (y, s') = (unwrap x) s
                    in  (f y, s'))

instance Applicative (State s) where
  pure x = State (\s -> (x, s))

  sf <*> sa = State (\s ->
                     let (f, s')  = (unwrap sf) s
                         (a, s'') = (unwrap sa) s'
                     in  (f a, s''))

instance Monad (State s) where
  return = pure

  x >>= f = State (\s ->
                   let (y, s') = (unwrap x) s
                   in  (unwrap (f y)) s')