{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Tests.Exc (
  TooBig(..),

  testExceptionTakesPriority,

  ter1,
  ter2,
  ter3,
  ter4,

  ex2rr,
  ex2rr1,
  ex2rr2,
) where

import Eff
import Eff.Exc.Pure
import Eff.Reader.Pure
import Eff.State.Pure

import Tests.Common

testExceptionTakesPriority :: Int -> Int -> Either Int Int
testExceptionTakesPriority x y = run $ runError @'[Exc Int] (go x y)
  where go a b = return a `add` throwError b

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    No instance for (Member (Exc Int) Void)
      arising from a use of `et2'
-}

-- exceptions and state
incr :: Member (State Int) r => Eff r ()
incr = get >>= put . (+ (1::Int))

type Stack = '[Exc String, State Int]

tes1 :: (Members '[State Int, Exc String] r) => Eff r b
tes1 = do
 incr
 throwError "exc"

ter1 :: (Either String Int, Int)
ter1 = run $ runState 1 (runError @Stack tes1)

ter2 :: Either String (String, Int)
ter2 = run $ runError (runState @Stack 1 tes1)

teCatch :: Member (Exc String) r => Eff r a -> Eff r String
teCatch m = catchError (m >> return "done") (\e -> return (e::String))

ter3 :: (Either String String, Int)
ter3 = run $ runState (1 :: Int) (runError @Stack (teCatch tes1))

ter4 :: Either String (String, Int)
ter4 = run $ runError (runState @Stack 1 (teCatch tes1))

-- The example from the paper
newtype TooBig = TooBig Int deriving (Eq, Show)

ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: forall r a o . (MemberOut (Exc TooBig) r o) => Eff r a -> Eff o (Either TooBig a)
runErrBig = runError

type Stack2 = '[Reader Int, Exc TooBig]

ex2rr :: Either TooBig Int
ex2rr = run go
  where go = runReader (5 :: Int) (runErrBig @Stack2 (ex2 ask))

ex2rr1 :: Either TooBig Int
ex2rr1 = run $ runReader (7 :: Int) (runErrBig @Stack2 (ex2 ask))

-- Different order of handlers (layers)
ex2rr2 :: Either TooBig Int
ex2rr2 = run $ runErrBig (runReader @Stack2 (7 :: Int) (ex2 ask))
