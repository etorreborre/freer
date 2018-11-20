{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Eff.Writer.Pure
  ( module Eff.Writer
  , module Eff.Writer.Pure
  ) where

import Eff
import Eff.Writer

-- | Simple handler for Writer effects
runWriter :: forall w r o a . (MemberOut (Writer w) r o, Monoid w) => Eff r a -> Eff o (a, w)
runWriter = handleRelay (\x -> return (x, mempty)) go
   where go :: Writer w v -> Arr o v (a, w) -> Eff o (a, w)
         go (Writer w) k =
           do (x, l) <- k ()
              pure (x, w `mappend` l)

ignoreWriter :: forall w r o a. (MemberOut (Writer w) r o) => Eff r a -> Eff o a
ignoreWriter = handleRelay pure bind
  where
    bind :: forall x. Writer w x -> (x -> Eff o a) -> Eff o a
    bind (Writer _) arr = arr ()
