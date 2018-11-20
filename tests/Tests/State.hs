{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Tests.State (
  testPutGet,
  testPutGetPutGetPlus,
  testGetStart,
  testInveffmap,
  testStateException,
  testExceptionState,
) where

import Eff
import Eff.State.Pure
import Eff.Exc.Pure
import Eff.Functor (inveffmap)
import Data.Tuple (swap)

type Stack = '[State Int]
type Stack2 = '[Exc String, State Int]

testPutGet :: Int -> Int -> (Int,Int)
testPutGet n start = run . runState @Stack start $ put n >> get

testPutGetPutGetPlus :: Int -> Int -> Int -> (Int,Int)
testPutGetPutGetPlus p1 p2 start = run . runState @Stack start $ do
  put p1
  x <- get
  put p2
  y <- get
  return (x+y)

testGetStart :: Int -> (Int,Int)
testGetStart = run . flip (runState @Stack) get

testInveffmap :: (Int, String) -> String
testInveffmap n = fst . run $ runState @'[State (Int, String)] (0 :: Int, "hello") $ inveffmap swap swap go
  where
    go = do
      put $ swap n
      (s, i :: Int) <- get
      pure $ show i ++ s

testStateException :: Int -> Either String (Int, Int)
testStateException n = run . runError . runState @Stack2 (0 :: Int) $ put n >> throwError "boom"

testExceptionState :: Int -> (Either String Int, Int)
testExceptionState n = run . runState (0 :: Int) . runError @Stack2 $ put n >> throwError "boom"
