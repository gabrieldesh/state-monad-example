module State
( State(..) ) where

newtype State s a = State { unwrap :: s -> (a, s) }

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
--return :: a -> (State s) a
  return x = State (\s -> (x, s))

--(>>=) :: (State s) a -> (a -> (State s) b) -> (State s) b
  sa >>= f = State (\s ->
                    let (a, s')  = (unwrap sa) s
                        sb       = f a
                        (b, s'') = (unwrap sb) s'
                    in  (b, s''))