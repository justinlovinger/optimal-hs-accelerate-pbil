{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Optimization.Accelerate.DerivativeFree.PBIL.Internal.Default
  ( PBILI.State(..)
  , PBILI.fromAccState
  , PBILI.initialProbabilities
  , initialStepGen
  , PBILI.initialMutateGen
  , adjust
  , mutate
  , isConverged
  , PBILI.finalize
  ) where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.System.Random.SFC
                                               as SFC
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL.Internal
                                               as PBILI
import           Math.Optimization.Accelerate.DerivativeFree.PBIL.Probability.Internal
                                                ( Probability(..) )

initialStepGen
  :: Int -- ^ number of bits in each sample
  -> IO (A.Acc SFC.Gen)
initialStepGen = PBILI.initialStepGen 20

-- | Adjust probabilities towards the best bits
-- in a set of samples.
adjust
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.FromIntegral A.Word8 a
     , A.Fractional a
     , A.Ord a
     , SFC.Uniform a
     , A.Ord b
     )
  => A.Acc (A.Vector (Probability a))
  -> A.Acc (A.Matrix Bool) -- ^ Rows of samples
  -> A.Acc (A.Vector b) -- ^ Objective values corresponding to samples
  -> A.Acc (A.Vector (Probability a))
adjust = PBILI.adjust 0.1

-- | Randomly adjust probabilities.
mutate
  :: (A.Fractional a, A.Ord a, SFC.Uniform a)
  => Int -- ^ number of bits in each sample
  -> A.Acc (A.Vector (Probability a))
  -> A.Acc SFC.Gen -- ^ same length as probabilities
  -> (A.Acc (A.Vector (Probability a)), A.Acc SFC.Gen)
mutate n = PBILI.mutate (f n) 0.05 where
  f x | x < 1     = 1
      | otherwise = 1 / fromIntegral x

-- | Have probabilities converged?
isConverged
  :: (A.Unlift A.Exp (Probability (A.Exp a)), A.Fractional a, A.Ord a)
  => A.Acc (A.Vector (Probability a))
  -> A.Acc (A.Scalar Bool)
isConverged = PBILI.isConverged 0.75
