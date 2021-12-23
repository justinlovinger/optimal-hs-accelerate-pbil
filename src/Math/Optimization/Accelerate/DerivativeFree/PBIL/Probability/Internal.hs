{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Optimization.Accelerate.DerivativeFree.PBIL.Probability.Internal
  ( Probability(..)
  , probability
  , fromProbability
  , fromBool
  , adjustArray
  , adjust
  , invert
  , samples
  , sample
  ) where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Smart   as AS
import qualified Data.Array.Accelerate.System.Random.SFC
                                               as SFC
import           GHC.Prim                       ( coerce )
import qualified Math.Optimization.Accelerate.Binary
                                               as B

newtype Probability a = Probability a
  deriving (A.Elt, A.Eq, A.Ord, SFC.Uniform, Eq, Ord, Show)

instance (A.Lift A.Exp a) => A.Lift A.Exp (Probability a) where
  type Plain (Probability a) = Probability (A.Plain a)
  lift =
    coerce @(a -> A.Exp (A.Plain a))
      @(Probability a -> A.Exp (Probability (A.Plain a)))
      A.lift

deriving instance (A.Unlift A.Exp a) => A.Unlift A.Exp (Probability a)

probability :: (Num a, Ord a) => a -> Maybe (Probability a)
probability x | x < 0     = Nothing
              | x > 1     = Nothing
              | otherwise = Just $ Probability x

fromProbability :: Probability a -> a
fromProbability (Probability x) = x

-- | Return '1' for 'True'
-- and '0' for 'False'.
fromBool :: A.FromIntegral A.Word8 a => A.Exp Bool -> A.Exp (Probability a)
fromBool = A.lift . Probability . B.fromBool

-- | Adjust each number in `a` to corresponding number in `b`
-- at given rate.
adjustArray
  :: (A.Shape sh, A.Num a)
  => A.Exp (Probability a) -- ^ adjustment rate, in range [0, 1]
  -> A.Acc (A.Array sh (Probability a)) -- ^ from
  -> A.Acc (A.Array sh (Probability a)) -- ^ to
  -> A.Acc (A.Array sh (Probability a))
adjustArray rate = A.zipWith
  (A.lift3
    (adjust :: (A.Num a)
      => Probability (A.Exp a)
      -> Probability (A.Exp a)
      -> Probability (A.Exp a)
      -> Probability (A.Exp a)
    )
    rate
  )

-- | Adjust a number from `x` to `y`
-- at given rate.
adjust
  :: (Num a)
  => Probability a -- ^ adjustment rate
  -> Probability a -- ^ from
  -> Probability a -- ^ to
  -> Probability a
adjust (Probability rate) (Probability x) (Probability y) =
  Probability $ x + rate * (y - x)

invert :: (Num a) => Probability a -> Probability a
invert (Probability x) = Probability $ 1 - x

samples
  :: (A.Ord a, SFC.Uniform a)
  => A.Acc (A.Vector (Probability a)) -- ^ row of probabilities, replicated for each row of result
  -> A.Acc SFC.Gen -- ^ random generator, gen `div` probabilities == 0
  -> A.Acc (A.Matrix Bool, SFC.Gen)
samples ps g = A.T2 (sample' ps' (A.reshape (A.shape ps') rs)) g'
 where
  ps'      = A.replicate (A.lift (A.Z A.:. n A.:. A.All)) ps
  n        = A.size rs `A.div` A.size ps
  (rs, g') = SFC.runRandom g SFC.randomVector

sample
  :: (A.Shape sh, A.Ord a, SFC.Uniform a)
  => A.Acc (A.Array sh (Probability a))
  -> A.Acc SFC.Gen -- ^ random generator, size must match probabilities
  -> A.Acc (A.Array sh Bool, SFC.Gen)
sample ps g = A.T2 (sample' ps (A.reshape (A.shape ps) rs)) g'
  where (rs, g') = SFC.runRandom g SFC.randomVector

sample'
  :: (A.Shape sh, A.Ord a)
  => A.Acc (A.Array sh (Probability a))
  -> A.Acc (A.Array sh (Probability a)) -- ^ random numbers in range [0, 1]
  -> A.Acc (A.Array sh Bool)
sample' ps rs = A.zipWith (A.<=) rs ps
