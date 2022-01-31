{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Optimization.Accelerate.DerivativeFree.PBIL
  ( Probability
  , probability
  , PBILI.State
  , initialState
  , state
  , step
  , isConverged
  , finalize
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
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL.Internal
                                               as PBILI
import           Math.Optimization.Accelerate.DerivativeFree.PBIL.Probability.Internal
                                                ( Probability(..)
                                                , probability
                                                , samples
                                                )

-- | Return recommended initial 'State'.
initialState
  :: Int -- ^ number of samples in each step
  -> Int -- ^ number of bits in each sample
  -> IO
       ( A.Acc
           (PBILI.State (A.Vector (Probability Double), SFC.Gen, SFC.Gen))
       )
initialState ns nb = do
  let ps0 = PBILI.initialProbabilities nb
  gs0 <- PBILI.initialStepGen ns nb
  gm0 <- PBILI.initialMutateGen nb
  pure $ A.lift $ PBILI.State $ A.T3 ps0 gs0 gm0

-- | Return 'State' if valid.
state
  :: (A.Fractional a, A.Ord a)
  => A.Vector (Probability a)
  -> A.Vector (A.Word64, A.Word64, A.Word64) -- ^ seed, length of (number of samples * length of probabilities), number of samples >= 2
  -> A.Vector (A.Word64, A.Word64, A.Word64) -- ^ seed, same length as probabilities
  -> Maybe
       ( A.Acc
           ( PBILI.State
               (A.Vector (Probability a), SFC.Gen, SFC.Gen)
           )
       )
state ps g1 g2
  | A.arraySize ps == 0 = Nothing
  | A.arraySize g1 `rem` A.arraySize ps /= 0 = Nothing
  | A.arraySize g1 `div` A.arraySize ps < 2 = Nothing
  | A.arrayShape g2 /= A.arrayShape ps = Nothing
  | otherwise = Just $ A.lift $ PBILI.State $ A.T3 (A.use ps)
                                                   (SFC.createWith $ A.use g1)
                                                   (SFC.createWith $ A.use g2)

-- | Take 1 PBIL step towards a higher objective value.
step
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.FromIntegral A.Word8 a
     , A.Fractional a
     , A.Ord a
     , SFC.Uniform a
     , A.Ord b
     )
  => A.Exp a -- ^ adjust rate, in range (0,1], clamped
  -> A.Exp a -- ^ mutation chance, in range (0,1], clamped
  -> A.Exp a -- ^ mutation adjust rate, in range (0,1], clamped
  -> (A.Acc (A.Matrix Bool) -> A.Acc (A.Vector b)) -- ^ objective function, maximize
  -> A.Acc
       (PBILI.State (A.Vector (Probability a), SFC.Gen, SFC.Gen))
  -> A.Acc
       ( PBILI.State
           (A.Vector (Probability a), SFC.Gen, SFC.Gen)
       )
step ar mc mar f =
  A.lift
    . PBILI.State
    . ( over (lensProduct _1 _3) (uncurry $ PBILI.mutate mc mar)
      . over (lensProduct _1 _2) (uncurry adjust')
      )
    . PBILI.fromAccState
 where
  adjust' ps g0 = (PBILI.adjustProbabilities ar ps bss (f bss), g1)
    where (A.T2 bss g1) = samples ps g0

-- | Has 'State' converged?
isConverged
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.Fractional a
     , A.Ord a
     , A.Arrays b
     , A.Arrays c
     )
  => A.Exp a -- ^ threshold, in range (0.5,1), clamped
  -> A.Acc (PBILI.State (A.Vector (Probability a), b, c))
  -> A.Acc (A.Scalar Bool)
isConverged ub = PBILI.isConverged ub . view _1 . PBILI.fromAccState

-- | Finalize 'State' probabilities into bits.
finalize
  :: (A.Ord a, Fractional a, A.Arrays b, A.Arrays c)
  => A.Acc (PBILI.State (A.Vector (Probability a), b, c))
  -> A.Acc (A.Vector Bool)
finalize = PBILI.finalize . view _1 . PBILI.fromAccState
