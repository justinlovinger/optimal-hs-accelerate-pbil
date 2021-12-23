{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Optimization.Accelerate.DerivativeFree.PBIL
  ( Probability
  , probability
  , State
  , PBILI.defaultNumSamples
  , initialState
  , state
  , StepHyperparameters
  , defaultStepHyperparameters
  , stepHyperparameters
  , step
  , PBILI.IsConvergedHyperparameters
  , PBILI.defaultIsConvergedHyperparameters
  , PBILI.isConvergedHyperparameters
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
import qualified Data.Array.Accelerate.Smart   as AS
import qualified Data.Array.Accelerate.System.Random.SFC
                                               as SFC
import           GHC.Prim                       ( coerce )
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL.Internal
                                               as PBILI
import           Math.Optimization.Accelerate.DerivativeFree.PBIL.Probability.Internal
                                                ( Probability(..)
                                                , probability
                                                , samples
                                                )

newtype State a = State a
  deriving (A.Arrays, Show)

instance (A.Lift A.Acc a) => A.Lift A.Acc (State a) where
  type Plain (State a) = State (A.Plain a)
  lift =
    coerce @(a -> A.Acc (A.Plain a)) @(State a -> A.Acc (State (A.Plain a)))
      A.lift

deriving instance (A.Unlift A.Acc a) => A.Unlift A.Acc (State a)

-- | Return recommended initial 'State'.
initialState
  :: Int -- ^ number of samples in each step
  -> Int -- ^ number of bits in each sample
  -> IO (A.Acc (State (A.Vector (Probability Double), SFC.Gen, SFC.Gen)))
initialState ns nb = do
  let ps0 = PBILI.initialProbabilities nb
  gs0 <- PBILI.initialStepGen ns nb
  gm0 <- PBILI.initialMutateGen nb
  pure $ A.lift $ State $ A.T3 ps0 gs0 gm0

-- | Return 'State' if valid.
state
  :: (A.Fractional a, A.Ord a)
  => A.Vector (Probability a)
  -> A.Vector (A.Word64, A.Word64, A.Word64) -- ^ seed, length of (number of samples * length of probabilities), number of samples >= 2
  -> A.Vector (A.Word64, A.Word64, A.Word64) -- ^ seed, same length as probabilities
  -> Maybe
       (A.Acc (State (A.Vector (Probability a), SFC.Gen, SFC.Gen)))
state ps g1 g2
  | A.arraySize ps == 0 = Nothing
  | A.arraySize g1 `rem` A.arraySize ps /= 0 = Nothing
  | A.arraySize g1 `div` A.arraySize ps < 2 = Nothing
  | A.arrayShape g2 /= A.arrayShape ps = Nothing
  | otherwise = Just $ A.lift $ State $ A.T3 (A.use ps)
                                             (SFC.createWith $ A.use g1)
                                             (SFC.createWith $ A.use g2)

newtype StepHyperparameters a = StepHyperparameters (PBILI.AdjustHyperparameters a, PBILI.MutateHyperparameters a)
  deriving Show

-- | Return default 'StepHyperparameters'.
defaultStepHyperparameters
  :: (A.Elt a, Fractional a)
  => Int -- ^ number of bits in each sample
  -> StepHyperparameters a
defaultStepHyperparameters n = StepHyperparameters
  (PBILI.defaultAdjustHyperparameters, PBILI.defaultMutateHyperparameters n)

-- | Return 'StepHyperparameters' if valid.
stepHyperparameters
  :: (A.Elt a, Num a, Ord a)
  => a -- ^ adjust rate, in range (0,1]
  -> a -- ^ mutation chance, in range (0,1]
  -> a -- ^ mutation adjust rate, in range (0,1]
  -> Maybe (StepHyperparameters a)
stepHyperparameters ar mc mar = do
  ah <- PBILI.adjustHyperparameters ar
  mh <- PBILI.mutateHyperparameters mc mar
  pure $ StepHyperparameters (ah, mh)

-- | Take 1 PBIL step towards a higher objective value.
step
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.FromIntegral A.Word8 a
     , A.Num a
     , A.Ord a
     , SFC.Uniform a
     , A.Ord b
     )
  => StepHyperparameters a
  -> (A.Acc (A.Matrix Bool) -> A.Acc (A.Vector b)) -- ^ objective function, maximize
  -> A.Acc (State (A.Vector (Probability a), SFC.Gen, SFC.Gen))
  -> A.Acc (State (A.Vector (Probability a), SFC.Gen, SFC.Gen))
step (StepHyperparameters (ah, mh)) f =
  A.lift
    . State
    . ( over (lensProduct _1 _3) (uncurry $ PBILI.mutate mh)
      . over (lensProduct _1 _2) (uncurry step')
      )
    . fromAccState
 where
  step' ps g0 = (PBILI.adjustProbabilities ah ps bss (f bss), g1)
    where (A.T2 bss g1) = samples ps g0

-- | Has 'State' converged?
isConverged
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.Fractional a
     , A.Ord a
     , A.Arrays b
     , A.Arrays c
     )
  => PBILI.IsConvergedHyperparameters a
  -> A.Acc (State (A.Vector (Probability a), b, c))
  -> A.Acc (A.Scalar Bool)
isConverged ub = PBILI.isConverged ub . view _1 . fromAccState

-- | Finalize 'State' probabilities into bits.
finalize
  :: (A.Ord a, Fractional a, A.Arrays b, A.Arrays c)
  => A.Acc (State (A.Vector (Probability a), b, c))
  -> A.Acc (A.Vector Bool)
finalize = PBILI.finalize . view _1 . fromAccState

fromAccState :: A.Acc (State a) -> A.Acc a
fromAccState = fromState . A.unlift

fromState :: State a -> a
fromState (State s) = s
