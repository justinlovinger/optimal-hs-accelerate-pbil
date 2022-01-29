{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Math.Optimization.Accelerate.DerivativeFree.PBIL.Internal
  ( State(..)
  , fromAccState
  , initialProbabilities
  , defaultNumSamples
  , initialStepGen
  , initialMutateGen
  , AdjustHyperparameters(..)
  , defaultAdjustHyperparameters
  , adjustHyperparameters
  , adjustProbabilities
  , MutateHyperparameters(..)
  , defaultMutateHyperparameters
  , mutateHyperparameters
  , mutate
  , IsConvergedHyperparameters(..)
  , defaultIsConvergedHyperparameters
  , isConvergedHyperparameters
  , isConverged
  , finalize
  ) where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Smart   as AS
import qualified Data.Array.Accelerate.System.Random.MWC
                                               as MWC
import qualified Data.Array.Accelerate.System.Random.SFC
                                               as SFC
import           GHC.Prim                       ( coerce )
import           Math.Optimization.Accelerate.DerivativeFree.PBIL.Probability.Internal
                                                ( Probability(..)
                                                , adjust
                                                , adjustArray
                                                , fromBool
                                                , invert
                                                , probability
                                                )

newtype State a = State a
  deriving (A.Arrays, Show)

instance (A.Lift A.Acc a) => A.Lift A.Acc (State a) where
  type Plain (State a) = State (A.Plain a)
  lift =
    coerce @(a -> A.Acc (A.Plain a)) @(State a -> A.Acc (State (A.Plain a)))
      A.lift

deriving instance (A.Unlift A.Acc a) => A.Unlift A.Acc (State a)

fromAccState :: A.Acc (State a) -> A.Acc a
fromAccState = fromState . A.unlift

fromState :: State a -> a
fromState (State s) = s

-- | Return recommended initial probabilities.
initialProbabilities
  :: Int -- ^ number of bits in each sample
  -> A.Acc (A.Vector (Probability Double))
initialProbabilities n =
  A.fill (A.constant $ A.Z A.:. n) $ A.constant (Probability 0.5)

defaultNumSamples :: Int
defaultNumSamples = 20

initialStepGen
  :: Int -- ^ number of samples in each step
  -> Int -- ^ number of bits in each sample
  -> IO (A.Acc SFC.Gen)
initialStepGen ns nb = initialGen $ ns * nb

initialMutateGen
  :: Int -- ^ number of bits in each sample
  -> IO (A.Acc SFC.Gen)
initialMutateGen = initialGen

initialGen :: Int -> IO (A.Acc SFC.Gen)
initialGen n =
  SFC.createWith . A.use <$> MWC.randomArray MWC.uniform (A.Z A.:. n)

newtype AdjustHyperparameters a = AdjustHyperparameters
  { adjustRate :: A.Exp (Probability a)
  }
  deriving Show

-- | Return default 'AdjustHyperparameters'.
defaultAdjustHyperparameters
  :: (A.Elt a, Fractional a) => AdjustHyperparameters a
defaultAdjustHyperparameters =
  AdjustHyperparameters $ A.constant $ Probability 0.1

-- | Return 'AdjustHyperparameters' if valid.
adjustHyperparameters
  :: (A.Elt a, Num a, Ord a)
  => a -- ^ adjust rate, in range (0,1]
  -> Maybe (AdjustHyperparameters a)
adjustHyperparameters r
  | r <= 0    = Nothing
  | otherwise = AdjustHyperparameters . A.constant <$> probability r

-- | Adjust probabilities towards the best bits
-- in a set of samples.
adjustProbabilities
  :: ( A.Unlift A.Exp (Probability (A.Exp a))
     , A.FromIntegral A.Word8 a
     , A.Num a
     , A.Ord a
     , SFC.Uniform a
     , A.Ord b
     )
  => AdjustHyperparameters a
  -> A.Acc (A.Vector (Probability a))
  -> A.Acc (A.Matrix Bool) -- ^ Rows of samples
  -> A.Acc (A.Vector b) -- ^ Objective values corresponding to samples
  -> A.Acc (A.Vector (Probability a))
adjustProbabilities (AdjustHyperparameters ar) ps bss fs = adjustArray
  ar
  ps
  (A.map fromBool $ maximumSliceBy fs bss)
 where
  maximumSliceBy
    :: (A.Shape sh, A.Slice sh, A.Elt a, A.Ord b)
    => A.Acc (A.Array sh b)
    -> A.Acc (A.Array (sh A.:. Int) a)
    -> A.Acc (A.Array (A.SliceShape (sh A.:. A.All)) a)
  maximumSliceBy ys xs = A.slice xs (A.lift $ imaximumAll ys A.:. A.All)

  imaximumAll :: (A.Shape sh, A.Ord e) => A.Acc (A.Array sh e) -> A.Exp sh
  imaximumAll xs = A.fst $ A.the $ A.fold1All
    (\ix ix' -> A.cond (A.snd ix A.> A.snd ix') ix ix')
    (A.indexed xs)

data MutateHyperparameters a = MutateHyperparameters
  { mutationChance     :: A.Exp (Probability a)
  , mutationAdjustRate :: A.Exp (Probability a)
  }
  deriving Show

-- | Return default 'MutateHyperparameters'.
defaultMutateHyperparameters
  :: (A.Elt a, Fractional a)
  => Int -- ^ number of bits in each sample
  -> MutateHyperparameters a
defaultMutateHyperparameters n = MutateHyperparameters
  (A.constant $ f n)
  (A.constant $ Probability 0.05)
 where
  f 0 = Probability 1
  f x = Probability $ 1 / fromIntegral x

-- | Return 'MutateHyperparameters' if valid.
mutateHyperparameters
  :: (A.Elt a, Num a, Ord a)
  => a -- ^ mutation chance, in range (0,1]
  -> a -- ^ mutation adjust rate, in range (0,1]
  -> Maybe (MutateHyperparameters a)
mutateHyperparameters mc mar
  | mc <= 0 = Nothing
  | mar <= 0 = Nothing
  | otherwise = do
    mc'  <- probability mc
    mar' <- probability mar
    pure $ MutateHyperparameters (A.constant mc') (A.constant mar')

-- | Randomly adjust probabilities.
mutate
  :: (A.Num a, A.Ord a, SFC.Uniform a)
  => MutateHyperparameters a
  -> A.Acc (A.Vector (Probability a))
  -> A.Acc SFC.Gen -- ^ same length as probabilities
  -> (A.Acc (A.Vector (Probability a)), A.Acc SFC.Gen)
mutate (MutateHyperparameters mc mar) ps g0 =
  ( A.zipWith3
    (\r1 p r2 -> A.cond
      (r1 A.<= mc)
      (A.lift3
        (adjust :: (A.Num a)
          => Probability (A.Exp a)
          -> Probability (A.Exp a)
          -> Probability (A.Exp a)
          -> Probability (A.Exp a)
        )
        mar
        p
        r2
      )
      p
    )
    rs1
    ps
    rs2
  , g2
  )
 where
  (rs2, g2) = SFC.runRandom g1 SFC.randomVector
  (rs1, g1) = SFC.runRandom g0 SFC.randomVector

newtype IsConvergedHyperparameters a = IsConvergedHyperparameters (A.Exp (Probability a))
  deriving (Show)

-- | Return default 'IsConvergedHyperparameters'.
defaultIsConvergedHyperparameters
  :: (Fractional a, Ord a, A.Elt a) => IsConvergedHyperparameters a
defaultIsConvergedHyperparameters =
  IsConvergedHyperparameters $ A.constant $ Probability 0.75

-- | Return 'IsConvergedHyperparameters' if valid.
isConvergedHyperparameters
  :: (A.Elt a, Fractional a, Ord a)
  => a -- ^ threshold, in range (0.5,1)
  -> Maybe (IsConvergedHyperparameters a)
isConvergedHyperparameters t
  | t <= 0.5  = Nothing
  | t >= 1.0  = Nothing
  | otherwise = Just $ IsConvergedHyperparameters $ A.constant $ Probability t

-- | Have probabilities converged?
isConverged
  :: (A.Unlift A.Exp (Probability (A.Exp a)), A.Fractional a, A.Ord a)
  => IsConvergedHyperparameters a
  -> A.Acc (A.Vector (Probability a))
  -> A.Acc (A.Scalar Bool)
isConverged (IsConvergedHyperparameters ub) = A.all
  (\x -> x A.< lb A.|| x A.> ub)
 where
  lb = A.lift1
    (invert :: (A.Num a) => Probability (A.Exp a) -> Probability (A.Exp a))
    ub

-- | Finalize probabilities to bits.
finalize
  :: (A.Ord a, Fractional a)
  => A.Acc (A.Vector (Probability a))
  -> A.Acc (A.Vector Bool)
finalize = A.map (A.>= A.constant (Probability 0.5))
