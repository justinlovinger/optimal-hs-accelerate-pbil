{-# LANGUAGE ScopedTypeVariables #-}

module Math.Optimization.DerivativeFree.PBILSpec where

import           Data.Array.Accelerate          ( (:.)(..)
                                                , Z(Z)
                                                )
import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as A
import qualified Data.Array.Accelerate.System.Random.SFC
                                               as SFC
import           Data.Bifunctor                 ( first )
import           Data.Maybe                     ( fromJust )
import           Data.Word                      ( Word64 )
import           Math.Optimization.DerivativeFree.PBIL
                                                ( ClampHyperparameters
                                                , State
                                                , clamp
                                                , clampHyperparameters
                                                , fromState
                                                , unsafeState
                                                )
import           System.Random                  ( Random )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , choose
                                                , property
                                                , suchThatMap
                                                )

newtype ArbitraryClampHyperparameters a = ArbitraryClampHyperparameters (ClampHyperparameters a)

instance (Fractional a, Ord a, Random a, A.Elt a) => Arbitrary (ArbitraryClampHyperparameters a) where
  arbitrary =
    ArbitraryClampHyperparameters
      <$> suchThatMap (choose (0, 1)) clampHyperparameters

newtype ArbitraryClosedBounded01Num a = ArbitraryClosedBounded01Num a
  deriving (Eq, Ord, Show)

instance (Num a, Ord a, Random a) => Arbitrary (ArbitraryClosedBounded01Num a) where
  arbitrary = ArbitraryClosedBounded01Num <$> choose (0, 1)

fromArbC :: ArbitraryClosedBounded01Num a -> a
fromArbC (ArbitraryClosedBounded01Num x) = x

spec :: Spec
spec =
  describe "Math"
    $ describe "Optimization"
    $ describe "DerivativeFree"
    $ describe "PBIL"
    $ do
        describe "clamp" $ do
          it "should return probabilities bounded by threshold"
            $ property
            $ \ps' g' ->
                let
                  ub = 0.9
                  -- We can't use `0.1` for `lb`
                  -- because of floating point precision errors.
                  lb = (1 - 0.9)
                  s  = unsafeState' (fmap fromArbC ps', createWith' g')
                  (ps1, _) =
                    fromState' $ clamp (fromJust $ clampHyperparameters ub) s
                in
                  all (betweenInc lb ub) ps1
          it "should return the same result for 1 - t"
            $ property
            $ \(ArbitraryClosedBounded01Num t) ps' g' ->
                let
                  s = unsafeState' (fmap fromArbC ps', createWith' g')
                  (psA, _) =
                    fromState' $ clamp (fromJust $ clampHyperparameters t) s
                  (psB, _) = fromState'
                    $ clamp (fromJust $ clampHyperparameters $ 1 - t) s
                in
                  psA == psB
          it "should not change probabilities if within threshold"
            $ property
            $ \ps' g' ->
                let ps0 = fmap fromArbC ps'
                    (ps1, _) =
                      fromState'
                        $ clamp (fromJust $ clampHyperparameters 1)
                        $ unsafeState' (ps0, createWith' g')
                in  ps0 == ps1
        -- describe "adjust" $ do
        --   it "should return a number bounded by a and b"
        --     $ property
        --     $ \rate' (a :: Double) b ->
        --         let rate = fromArbC rate'
        --         in  betweenInc (min a b) (max a b) (adjust rate a b)


createWith' :: [(Word64, Word64, Word64)] -> SFC.Gen
createWith' xs =
  A.run $ SFC.createWith $ A.use $ A.fromList (Z :. length xs) xs

unsafeState' :: ([Double], SFC.Gen) -> State Double
unsafeState' (ps, g) = unsafeState $ A.use (A.fromList (Z :. length ps) ps, g)

fromState' :: State Double -> ([Double], SFC.Gen)
fromState' = first A.toList . A.run . fromState

betweenInc :: Ord a => a -> a -> a -> Bool
betweenInc lb ub x = x >= lb && x <= ub
