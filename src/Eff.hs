{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Eff
Description : Freer - an extensible effects library
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Eff (
  Member,
  MemberOut,
  Members,
  Eff,
  run,
  runM,
  runNat,
  handleRelay,
  handleRelayS,
  replaceRelay,
  replaceRelayS,
  raise,
  send,
  Arr,

  NonDetEff(..),
  makeChoiceA,
  msplit
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (pure)
#endif

import Eff.Internal

runNat :: forall e r o m w . (MemberOut e r o, Member m o)
  => (forall v. e v -> m v) -> Eff r w -> Eff o w
runNat f = handleRelay pure (\v -> (send (f v) >>=))
