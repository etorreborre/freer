{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Eff.Reader.Pure
  ( module Eff.Reader
  , module Eff.Reader.Pure
  ) where

import Eff
import Eff.Reader

------------------------------------------------------------------------------
-- | Handler for reader effects
runReader :: forall r e o w . (MemberOut (Reader e) r o) => e -> Eff r w -> Eff o w
runReader e = handleRelay @r @(Reader e) @o pure $ \Reader k -> k e


------------------------------------------------------------------------------
-- | Interpret a 'Reader' with a monadic action.
runReaderM :: forall s r o m a
            . (MemberOut (Reader s) r o, Member m o)
           => m s
           -> Eff r a
           -> Eff o a
runReaderM mval = runNat nat
  where
    nat :: forall x. Reader s x -> m x
    nat Reader = mval
