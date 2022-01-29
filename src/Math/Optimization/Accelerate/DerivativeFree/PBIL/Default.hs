{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | PBIL with recommended default hyperparameters.
module Math.Optimization.Accelerate.DerivativeFree.PBIL.Default
  ( Probability
  , PBILI.State
  , initialState
  , step
  , isConverged
  , PBIL.finalize
  ) where

import           Control.Lens                   ( over
                                                , view
                                                )
import           Control.Lens.Unsound           ( lensProduct )
import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate.Control.Lens
                                                ( _1
                                                , _2
                                                , _3
                                                )
import qualified Data.Array.Accelerate.System.Random.SFC
                                               as SFC
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL
                                               as PBIL
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL.Internal
                                               as PBILI
import           Math.Optimization.Accelerate.DerivativeFree.PBIL.Probability.Internal
                                                ( Probability
                                                , samples
                                                )

-- | Return recommended initial 'State'.
initialState
  :: Int -- ^ number of bits in each sample
  -> IO
       (A.Acc (PBIL.State (A.Vector (Probability Double), SFC.Gen, SFC.Gen)))
initialState = PBIL.initialState PBILI.defaultNumSamples

-- | Take 1 PBIL step towards a higher objective value.
step
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.FromIntegral A.Word8 a
     , A.Num a
     , A.Ord a
     , SFC.Uniform a
     , Fractional a
     , A.Ord b
     )
  => Int -- ^ number of bits in each sample
  -> (A.Acc (A.Matrix Bool) -> A.Acc (A.Vector b)) -- ^ objective function, maximize
  -> A.Acc (PBIL.State (A.Vector (Probability a), SFC.Gen, SFC.Gen))
  -> A.Acc (PBIL.State (A.Vector (Probability a), SFC.Gen, SFC.Gen))
step n f =
  A.lift
    . PBILI.State
    . ( over (lensProduct _1 _3) (uncurry $ PBILI.mutate mh)
      . over (lensProduct _1 _2) (uncurry step')
      )
    . PBILI.fromAccState
 where
  step' ps g0 = (PBILI.adjustProbabilities ah ps bss (f bss), g1)
    where (A.T2 bss g1) = samples ps g0
  ah = PBILI.defaultAdjustHyperparameters
  mh = PBILI.defaultMutateHyperparameters n

-- | Has 'State' converged?
isConverged
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.Fractional a
     , A.Ord a
     , Fractional a
     , Ord a
     , A.Arrays b
     , A.Arrays c
     )
  => A.Acc (PBILI.State (A.Vector (Probability a), b, c))
  -> A.Acc (A.Scalar Bool)
isConverged =
  PBILI.isConverged PBILI.defaultIsConvergedHyperparameters
    . view _1
    . PBILI.fromAccState
