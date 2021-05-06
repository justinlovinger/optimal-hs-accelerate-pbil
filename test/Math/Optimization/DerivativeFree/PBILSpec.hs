{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Optimization.DerivativeFree.PBILSpec
  ( spec
  ) where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as A
import           Math.Optimization.DerivativeFree.PBIL
                                                ( MutateHyperparameters
                                                , State
                                                , StepHyperparameters
                                                , awhile
                                                , defaultIsConvergedHyperparameters
                                                , defaultMutateHyperparameters
                                                , defaultStepHyperparameters
                                                , finalize
                                                , fromState
                                                , initialState
                                                , isConverged
                                                , mutate
                                                , mutateHyperparameters
                                                , state
                                                , step
                                                , stepHyperparameters
                                                )
import           System.Random                  ( Random )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                , shouldSatisfy
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , Gen
                                                , choose
                                                , chooseInt
                                                , infiniteListOf
                                                , property
                                                , suchThatMap
                                                )

instance (Num a, Ord a, Random a, A.Fractional a, A.Ord a) => Arbitrary (State a) where
  arbitrary = suchThatMap arbStateTuple (uncurry state)   where
    arbStateTuple = do
      n  <- chooseInt (0, 100)
      ps <- take n <$> infiniteListOf (choose (0, 1))
      g  <- take n <$> infiniteListOf arbitrary
      pure (A.fromList (A.Z A.:. n) ps, A.fromList (A.Z A.:. n) g)

instance (Fractional a, Ord a, Random a, A.Elt a) => Arbitrary (StepHyperparameters a) where
  arbitrary =
    suchThatMap (zipGen arbitrary (choose (0, 1))) $ uncurry stepHyperparameters

instance (Fractional a, Ord a, Random a, A.Elt a) => Arbitrary (MutateHyperparameters a) where
  arbitrary = suchThatMap (zipGen (choose (0, 1)) (choose (0, 1)))
    $ uncurry mutateHyperparameters

zipGen :: Gen a -> Gen b -> Gen (a, b)
zipGen x y = do
  a <- x
  b <- y
  pure (a, b)

spec :: Spec
spec =
  describe "Math"
    $ describe "Optimization"
    $ describe "DerivativeFree"
    $ describe "PBIL"
    $ do
        it "should be able to solve sphere problem" $ do
          let f        = sphere'
              t        = -0.01
              optimize = do
                s0 <- initialState sphereN
                let vn = head . A.toList . A.run $ f $ finalize $ awhile
                      ( A.map A.not
                      . isConverged defaultIsConvergedHyperparameters
                      )
                      ( mutate (defaultMutateHyperparameters sphereN)
                      . step defaultStepHyperparameters f
                      )
                      s0
                if vn > t then pure vn else optimize
          vn <- optimize
          vn `shouldSatisfy` (> t)

        describe "step" $ do
          it "should be able to improve objective value"
            -- For all step hypererparameters,
            -- there exists an inital state
            -- such that 1 step
            -- improves objective value.
            $ property
            $ \h -> do
                let
                  f       = sphere'
                  oneStep = do
                    s0 <- initialState sphereN
                    let v0 = head . A.toList . A.run $ f $ finalize s0
                        v1 =
                          head . A.toList . A.run $ f $ finalize $ step h f s0
                    if v1 > v0 then pure True else oneStep
                done <- oneStep
                done `shouldBe` True

        describe "mutate" $ do
          it "should not change length of probabilities"
            $ property
            $ \h (s :: State Double) ->
                let length' = length . A.toList . fst . A.run . fromState
                in  length' s `shouldBe` length' (mutate h s)

sphere' :: A.Acc (A.Vector Bool) -> A.Acc (A.Scalar Double)
sphere' = A.map A.negate . sphere . fromBits (-5) 5 . A.reshape
  (A.constant $ A.Z A.:. sphereD A.:. sphereBPD)

sphereN :: Int
sphereN = sphereD * sphereBPD

sphereD :: Int
sphereD = 2

sphereBPD :: Int
sphereBPD = 8

sphere :: (A.Num a) => A.Acc (A.Vector a) -> A.Acc (A.Scalar a)
sphere = A.sum . A.map (^ (2 :: Int))

-- | Reduce innermost dimension
-- to numbers within lower and upper bound
-- from bits.
fromBits
  :: (A.Shape sh, A.Fractional a)
  => A.Exp a -- ^ Lower bound
  -> A.Exp a -- ^ Upper bound
  -> A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh a)
-- Guard on 0 bits
-- to avoid division by 0.
fromBits lb ub bs = A.acond (nb A.== 0)
                            (A.fill (A.indexTail sh) lb)
                            ((a *! fromBits' bs) !+ lb)
 where
  a  = (ub - lb) / (2 A.^ nb - 1) -- range / max int
  nb = A.indexHead sh
  sh = A.shape bs

-- | Reduce innermost dimension
-- to base 10 integer representations of bits.
-- Leftmost bit is least significant.
fromBits'
  :: (A.Shape sh, A.Num a)
  => A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh a)
fromBits' = A.sum . A.imap (\i x -> fromBool x * 2 A.^ A.indexHead i)

fromBool :: (A.Num a) => A.Exp Bool -> A.Exp a
fromBool x = A.cond x 1 0

-- | Add scalar to each element of an array.
(!+)
  :: (A.Shape sh, A.Num a)
  => A.Acc (A.Array sh a)
  -> A.Exp a
  -> A.Acc (A.Array sh a)
(!+) a x = A.map (+ x) a

-- | Multiply scalar by each element of an array.
(*!)
  :: (A.Shape sh, A.Num a)
  => A.Exp a
  -> A.Acc (A.Array sh a)
  -> A.Acc (A.Array sh a)
(*!) x = A.map (x *)
