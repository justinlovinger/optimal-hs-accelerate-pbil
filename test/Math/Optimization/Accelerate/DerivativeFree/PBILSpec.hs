{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Math.Optimization.Accelerate.DerivativeFree.PBILSpec
  ( spec
  ) where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as AI
import           Data.Maybe                     ( fromJust )
import           Math.Optimization.Accelerate.Binary
                                                ( reversedBitsToFrac )
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL
                                               as PBIL
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL.Default
                                               as PBILD
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldSatisfy
                                                )

spec :: Spec
spec =
  describe "Math"
    $ describe "Optimization"
    $ describe "DerivativeFree"
    $ describe "PBIL"
    $ do
        it "should be able to solve sphere problem" $ fromJust $ do
          s  <- PBIL.step 0.1 0.1 0.05 sphere'
          ic <- PBIL.isConverged 0.9
          pure $ sphereSpec (PBIL.initialState 10) s ic PBIL.finalize

        it "should be able to solve sphere problem with default hyperparameters"
          $ sphereSpec PBILD.initialState
                       (PBILD.step sphereN sphere')
                       PBILD.isConverged
                       PBILD.finalize

 where
  sphereSpec initialState step isConverged finalize = do
    let t        = -0.01
        optimize = do
          s0 <- initialState sphereN
          let vn = head $ A.toList $ AI.run $ sphere' $ finalize $ A.awhile
                (A.map A.not . isConverged)
                step
                s0
          if vn > t then pure vn else optimize
    vn <- optimize
    vn `shouldSatisfy` (> t)

sphere'
  :: (A.Shape sh)
  => A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh Double)
sphere' xs = A.map A.negate $ sphere $ reversedBitsToFrac (-5) 5 $ A.reshape
  (A.lift $ A.indexTail (A.shape xs) A.:. sphereD A.:. sphereBPD)
  xs

sphereN :: Int
sphereN = sphereD * sphereBPD

sphereD :: Int
sphereD = 2

sphereBPD :: Int
sphereBPD = 8

sphere
  :: (A.Shape sh, A.Num a)
  => A.Acc (A.Array (sh A.:. Int) a)
  -> A.Acc (A.Array sh a)
sphere = A.sum . A.map (^ (2 :: Int))
