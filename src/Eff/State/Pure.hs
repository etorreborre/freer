{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Eff.State.Pure
  ( module Eff.State
  , module Eff.State.Pure
  ) where

import Eff.Internal
import Eff.State
import Data.Proxy

-- | Handler for State effects
runState :: forall r s w o . (MemberOut (State s) r o) =>
  s -> Eff r w -> Eff o (w, s)
runState s = handleRelayS s (\s' a -> pure (a, s')) go
  where go :: s
           -> State s v
           -> (s -> Arr o v (a, s))
           -> Eff o (a, s)

        go s' Get k = k s' s'
        go _s' (Put s'') k = k s'' ()

evalState :: (MemberOut (State s) r o) => s -> Eff (State s ': r) w -> Eff r w
evalState = (fmap fst .) . runState

execState :: (MemberOut (State s) r o) => s -> Eff (State s ': r) w -> Eff r s
execState = (fmap snd .) . runState

-- |
-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
transactionState :: forall s r w. Member (State s) r =>
                    Proxy s -> Eff r w -> Eff r w
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r w -> Eff r w
   loop s (Val x) = put s >> return x
   loop s (E (u :: Union r b) q) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (qApp q s)
     Just (Put s') -> loop s'(qApp q ())
     _             -> E u (tsingleton k) where k = qComp q (loop s)
