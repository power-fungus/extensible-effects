{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
import Control.Exception (ErrorCall, catch)
import Data.Typeable

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

import Test.HUnit hiding (State)
import Test.QuickCheck

import qualified Control.Eff1 as E1
import qualified Control.Eff.Reader.Lazy1 as E1.LazyR
import qualified Control.Eff.Reader.Strict1 as E1.StrictR
import Control.Monad (liftM2)

import Control.Eff
import Control.Eff.Example as Eg
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Operational as Op
import Control.Eff.Operational.Example as Op.Eg
import Control.Eff.State.Lazy as LazyS
import Control.Eff.Writer.Lazy as LazyW
import Control.Eff.State.Strict as StrictS
import Control.Eff.Writer.Strict as StrictW
import Data.Void

main :: IO ()
main = defaultMain tests

tests = [
  $(testGroupGenerator)
#if __GLASGOW_HASKELL__ >= 708
  , testProperty "Test nested Eff." testNestedEff
#endif
        ]

-- {{{ utils

withError :: a -> ErrorCall -> a
withError a _ = a

assertUndefined :: a -> Assertion
assertUndefined a = catch (seq a $ assertFailure "") (withError $ return ())

assertNoUndefined :: a -> Assertion
assertNoUndefined a = catch (seq a $ return ()) (withError $ assertFailure "")

allEqual :: Eq a => [a] -> Bool
allEqual = all (uncurry (==)) . pairs
  where
    pairs l = zip l $ tail l

safeLast [] = Nothing
safeLast l = Just $ last l

-- }}}

-- {{{ Documentation example

prop_Documentation_example :: [Integer] -> Property
prop_Documentation_example l = let
  (total1, ()) = run $ LazyS.runState 0 $ Eg.sumAll l
  (last1, ()) = run $ LazyW.runLastWriter $ Eg.writeAll l
  (total2, (last2, ())) = run $ LazyS.runState 0 $ LazyW.runLastWriter $ Eg.writeAndAdd l
  (last3, (total3, ())) = run $ LazyW.runLastWriter $ LazyS.runState 0 $ Eg.writeAndAdd l
  in
   allEqual [safeLast l, last1, last2, last3]
   .&&. allEqual [sum l, total1, total2, total3]

-- }}}

-- {{{ Reader

add = liftM2 (+)
t1 = E1.LazyR.ask `add` return (1::Int)

case_Lazy1_Reader_t1 :: Assertion
case_Lazy1_Reader_t1 = let
  t1' = do v <- E1.LazyR.ask; return (v + 1 :: Int)
  t1r = E1.LazyR.runReader t1 (10::Int)
  in
    -- 'E1.LazyR.run t1' should result in type-error
    11 @=? (E1.run t1r)

t2 = do
  v1 <- E1.LazyR.ask
  v2 <- E1.LazyR.ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))


case_Lazy1_Reader_t2 :: Assertion
case_Lazy1_Reader_t2 = let
  t2r = E1.LazyR.runReader t2 (10::Int)
  t2rr = flip E1.LazyR.runReader (20::Float) . flip E1.LazyR.runReader (10::Int) $ t2
  in
    33.0 @=? (E1.run t2rr)

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}
case_Lazy1_Reader_t2' :: Assertion
case_Lazy1_Reader_t2' = 33.0 @=?
  (E1.run $ E1.LazyR.runReader (E1.LazyR.runReader t2 (20::Float)) (10::Int))


case_Lazy1_Reader_t3 :: Assertion
case_Lazy1_Reader_t3 = let
  t3 = t1 `add` E1.LazyR.local (+ (10::Int)) t1
  in
    212 @=? (E1.run $ E1.LazyR.runReader t3 (100::Int))

-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftM2 (+) (E1.LazyR.local (+ (10::Int)) t2)
                (E1.LazyR.local (+ (30::Float)) t2)

case_Lazy1_Reader_t4 :: Assertion
case_Lazy1_Reader_t4 = 106.0 @=?
  (E1.run $ E1.LazyR.runReader (E1.LazyR.runReader t4 (20::Float)) (10::Int))

-- The opposite order of layers gives the same result
case_Lazy1_Reader_t4' :: Assertion
case_Lazy1_Reader_t4' = 106.0 @=?
  (E1.run $ E1.LazyR.runReader (E1.LazyR.runReader t4 (20::Float)) (10::Int))

-- Map an effectful function
case_Lazy1_Reader_tmap :: Assertion
case_Lazy1_Reader_tmap = let
  tmap = mapM f [1..5]
  in
    ([11,12,13,14,15] :: [Int]) @=?
    (E1.run $ E1.LazyR.runReader tmap (10::Int))
  where
    f x = E1.LazyR.ask `add` return x

-- {{{ Reader.runReader

case_Lazy1_Reader_runReader :: Assertion
case_Lazy1_Reader_runReader = let
  e = E1.run $ E1.LazyR.runReader voidReader (undefined :: ())
  in
   assertNoUndefined (e :: ())
  where
    voidReader = do
        _ <- (E1.LazyR.ask :: E1.Eff '[E1.LazyR.Reader ()] ())
        return ()

case_Strict1_Reader_runReader :: Assertion
case_Strict1_Reader_runReader = let
  e = E1.run $ E1.StrictR.runReader voidReader (undefined :: ())
  in
   assertUndefined (e :: ())
  where
    voidReader = do
        _ <- (E1.StrictR.ask :: E1.Eff '[E1.StrictR.Reader ()] ())
        return ()


-- }}}

-- }}}

-- {{{ State.runState

case_Lazy_State_runState :: Assertion
case_Lazy_State_runState = let
  (r, ()) = run
            $ LazyS.runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertNoUndefined r
  where
    getVoid :: Eff (LazyS.State () :> Void) ()
    getVoid = LazyS.get

    putVoid :: () -> Eff (LazyS.State () :> Void) ()
    putVoid = LazyS.put

case_Strict_State_runState :: Assertion
case_Strict_State_runState = let
  (r, ()) = run
            $ StrictS.runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertUndefined r
  where
    getVoid :: Eff (StrictS.State () :> Void) ()
    getVoid = StrictS.get

    putVoid :: () -> Eff (StrictS.State () :> Void) ()
    putVoid = StrictS.put

-- }}}

-- {{{ Writer

addGet x = E1.LazyR.ask >>= \i -> return (i+x)

addN n = foldl (>>>) return (replicate n addGet) 0
 where f >>> g = (>>= g) . f

-- {{{ Writer.censor

prop_Lazy_Writer_censor :: [Integer] -> Property
prop_Lazy_Writer_censor l =
  property
  $ listE (mapM_ (LazyW.tell . inc) l) == listE (LazyW.censor inc $ mapM_ LazyW.tell l)
  where
    inc :: Integer -> Integer
    inc = (+1)

    listE :: Eff (LazyW.Writer Integer :> Void) () -> [Integer]
    listE = fst . run . LazyW.runWriter (:) []

-- }}}

-- {{{ Writer.runFirstWriter

case_Lazy_Writer_runFirstWriter :: Assertion
case_Lazy_Writer_runFirstWriter = let
  (Just m, ()) = run $ LazyW.runFirstWriter $ mapM_ LazyW.tell [(), undefined]
  in
   assertNoUndefined (m :: ())

-- }}}

-- {{{ Writer.runLastWriter

case_Lazy_Writer_runLastWriter :: Assertion
case_Lazy_Writer_runLastWriter = let
  (Just m, ()) = run $ LazyW.runLastWriter $ mapM_ LazyW.tell [undefined, ()]
  in
   assertNoUndefined (m :: ())

case_Strict_Writer_runLastWriter :: Assertion
case_Strict_Writer_runLastWriter = let
  (Just m, ()) = run $ StrictW.runLastWriter $ mapM_ StrictW.tell [undefined, ()]
  in
   assertUndefined (m :: ())

-- }}}

-- }}}

-- {{{ Eff Failure

case_Failure_Effect :: Assertion
case_Failure_Effect =
  let go :: Eff (Exc () :> StrictW.Writer Int :> Void) Int
         -> Int
      go = fst . run . StrictW.runWriter (+) 0 . ignoreFail
      ret = go $ do
        StrictW.tell (1 :: Int)
        StrictW.tell (2 :: Int)
        StrictW.tell (3 :: Int)
        () <- die
        StrictW.tell (4 :: Int)
        return 5
   in assertEqual "Fail should stop writing" 6 ret

-- }}}

#if __GLASGOW_HASKELL__ >= 708
#define Typeable1 Typeable
#endif

-- {{{ test Lift building

-- | Ensure that https://github.com/RobotGymnast/extensible-effects/issues/11 stays resolved.
case_Lift_building :: Assertion
case_Lift_building = runLift possiblyAmbiguous
  where
    possiblyAmbiguous :: (Typeable1 m, Monad m, SetMember Lift (Lift m) r) => Eff r ()
    possiblyAmbiguous = lift $ return ()

-- }}}

-- {{{ Nested Eff

#if __GLASGOW_HASKELL__ >= 708
testNestedEff :: Property
testNestedEff = forAll arbitrary (\x -> property (qu x == x))
  where
    qu :: Bool -> Bool
    qu x = E1.run $ E1.StrictR.runReader (readerAp x) readerId

    readerAp :: Bool -> E1.Eff '[E1.StrictR.Reader (E1.Eff '[E1.StrictR.Reader Bool] Bool)] Bool
    readerAp x = do
      f <- E1.StrictR.ask
      return . E1.run $ E1.StrictR.runReader f x

    readerId :: E1.Eff '[E1.StrictR.Reader Bool] Bool
    readerId = do
      x <- E1.StrictR.ask
      return x
#endif

-- }}}

-- {{{ Operational Monad

case_Operational_Monad :: Assertion
case_Operational_Monad =
  let comp :: (Member (LazyS.State [String]) r
               , Member (LazyW.Writer String) r)
              => Eff r ()
      comp = Op.runProgram Op.Eg.adventPure Op.Eg.prog
      go = fst . run . LazyW.runMonoidWriter . LazyS.evalState ["foo", "bar"] $ comp
  in
   assertEqual
   "Evaluating Operational Monad example"
   "getting input...\nok\nthe input is foo\n" go

-- }}}
