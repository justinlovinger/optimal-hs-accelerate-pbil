{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Math.Optimization.DerivativeFree.PBIL
  ( State
  , initialState
  , unsafeState
  , fromState
  , StepHyperparameters
  , defaultStepHyperparameters
  , stepHyperparameters
  , step
  , MutateHyperparameters
  , defaultMutateHyperparameters
  , mutateHyperparameters
  , mutate
  , ClampHyperparameters
  , defaultClampHyperparameters
  , clampHyperparameters
  , clamp
  , finalize
  ) where

import           Data.Array.Accelerate          ( (:.)(..)
                                                , Acc
                                                , Array
                                                , Arrays
                                                , Elt
                                                , Exp
                                                , Scalar
                                                , Shape
                                                , pattern T2
                                                , pattern T3
                                                , Vector
                                                , Z(Z)
                                                )
import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.System.Random.MWC
                                               as MWC
import           Data.Array.Accelerate.System.Random.SFC
                                                ( Gen
                                                , Uniform
                                                , createWith
                                                , randomVector
                                                , runRandom
                                                )

type Probability = ClosedBounded01Num

newtype ClosedBounded01Num a = ClosedBounded01Num a
  deriving (Eq, Ord, Show)

closedBounded01Num :: (Num a, Ord a) => a -> Maybe (ClosedBounded01Num a)
closedBounded01Num x | x < 0     = Nothing
                     | x > 1     = Nothing
                     | otherwise = Just $ ClosedBounded01Num x

aConstantCB01N :: (Elt a) => ClosedBounded01Num a -> ClosedBounded01Num (Exp a)
aConstantCB01N (ClosedBounded01Num x) = ClosedBounded01Num $ A.constant x

newtype State a = State (Acc (Vector a, Gen))

-- | Return recommended initial state.
initialState
  :: Int -- ^ Number of bits in each sample
  -> IO (State Double)
initialState nb = do
  g <- createWith . A.use <$> MWC.randomArray MWC.uniform sh
  pure $ State $ T2 (A.fill (A.constant sh) $ A.constant 0.5) g
  where sh = Z :. nb

-- | Return state without safety checking.
-- Probabilities must be in range [0,1].
-- Random generator must have same shape as probabilities.
unsafeState
  :: (Elt a)
  => Acc (Vector a, Gen) -- ^ (probabilities, random state)
  -> State a
unsafeState = State

fromState :: (Elt a) => State a -> Acc (Vector a, Gen)
fromState (State s) = s

data StepHyperparameters a = StepHyperparameters
  { sampleSize :: Exp Int
  , adjustRate :: ClosedBounded01Num (Exp a)
  }

-- | Return default 'StepHyperparameters'.
defaultStepHyperparameters
  :: (A.Fractional a, A.Ord a) => StepHyperparameters a
defaultStepHyperparameters = StepHyperparameters 20 (ClosedBounded01Num 0.1)

-- | Return 'StepHyperparameters'.
stepHyperparameters
  :: (Fractional a, Ord a, A.Elt a)
  => Int -- ^ Sample size, > 0.
  -> a -- ^ Adjust rate, in range [0,1].
  -> Maybe (StepHyperparameters a)
stepHyperparameters n r
  | n <= 0
  = Nothing
  | otherwise
  = StepHyperparameters (A.constant n) . aConstantCB01N <$> closedBounded01Num r

-- | Take 1 step towards a 'State' with a higher objective value.
-- by adjusting probabilities towards the best bits
-- in a set of samples.
step
  :: (A.Num a, A.Ord a, Uniform a, A.Ord b)
  => StepHyperparameters a
  -> (Acc (Vector Bool) -> Acc (Scalar b)) -- ^ Objective function. Maximize.
  -> State a
  -> State a
step (StepHyperparameters n ar) f (State (T2 ps g0)) = State $ T2 ps' g1 where
  (T3 _ bsStar g1) = aiterate
    (n - 1)
    (\(T3 fbs bs g) ->
      let (T2 bs' g') = sample ps g
          fbs'        = f bs'
      in  A.acond (A.the fbs A.< A.the fbs') (T3 fbs' bs' g') (T3 fbs bs g')
    )
    (let (T2 bs g) = sample ps g0 in T3 (f bs) bs g)
  -- 'adjust' from a probability to a bit
  -- will always be a valid probability,
  -- because a bit is 0 or 1
  -- and 'adjust' will return a value between that range.
  ps' = adjustArray ar ps (A.map fromBool bsStar)

-- | Repeatedly apply a function a fixed number of times.
aiterate
  :: (Arrays a)
  => Exp Int -- ^ number of times to apply function
  -> (Acc a -> Acc a) -- ^ function to apply
  -> Acc a -- ^ initial value
  -> Acc a
aiterate n f xs0 = A.asnd $ A.awhile
  (A.unit . (A.< n) . A.the . A.afst)
  (\(A.T2 i xs) -> A.T2 (i !+ 1) (f xs))
  (A.lift (A.unit $ A.constant (0 :: Int), xs0))

-- | Add scalar to each element of an array.
(!+) :: (Shape sh, A.Num a) => Acc (Array sh a) -> Exp a -> Acc (Array sh a)
(!+) a x = A.map (+ x) a

sample
  :: (A.Ord a, Uniform a) => Acc (Vector a) -> Acc Gen -> Acc (Vector Bool, Gen)
sample ps g = A.lift (A.zipWith (A.<=) rs ps, g')
  where (rs, g') = runRandom g randomVector

fromBool :: (A.Num a) => Exp Bool -> Exp a
fromBool x = A.cond x 1 0

data MutateHyperparameters a = MutateHyperparameters
  { mutationChance     :: Probability (Exp a)
  , mutationAdjustRate :: ClosedBounded01Num (Exp a)
  }

-- | Return default 'MutateHyperparameters'.
defaultMutateHyperparameters
  :: (A.Fractional a, A.Ord a)
  => Int -- ^ Number of bits in each sample
  -> MutateHyperparameters a
defaultMutateHyperparameters n = MutateHyperparameters
  (ClosedBounded01Num . f $ n)
  (ClosedBounded01Num 0.05)
 where
  f 0 = 1
  f x = 1 / fromIntegral x

-- | Return 'MutateHyperparameters'.
mutateHyperparameters
  :: (Fractional a, Ord a, Elt a)
  => a -- ^ Mutation chance, in range [0,1].
  -> a -- ^ Mutation adjust rate, in range [0,1].
  -> Maybe (MutateHyperparameters a)
mutateHyperparameters mc mar = do
  mc'  <- closedBounded01Num mc
  mar' <- closedBounded01Num mar
  pure $ MutateHyperparameters (aConstantCB01N mc') (aConstantCB01N mar')

-- | Randomly adjust probabilities.
mutate
  :: (A.Num a, A.Ord a, Uniform a)
  => MutateHyperparameters a
  -> State a
  -> State a
mutate (MutateHyperparameters (ClosedBounded01Num mc) mar) (State (T2 ps g0)) =
  State $ T2
  -- 'adjust' from a probability to a number in range 0 to 1
  -- will always be a valid probability,
  -- because 'adjust' will return a value between that range.
    (A.zipWith3 (\r1 p r2 -> A.cond (r1 A.<= mc) (adjust mar p r2) p) rs1 ps rs2
    )
    g2
 where
  (rs1, g1) = runRandom g0 randomVector
  (rs2, g2) = runRandom g1 randomVector

-- | Adjust each number in `a` to corresponding number in `b`
-- at given rate.
adjustArray
  :: (Shape sh, A.Num a)
  => ClosedBounded01Num (Exp a) -- ^ Adjustment rate
  -> Acc (Array sh a) -- ^ From
  -> Acc (Array sh a) -- ^ To
  -> Acc (Array sh a)
adjustArray rate = A.zipWith (adjust rate)

-- | Adjust a number from `a` to `b`
-- at given rate.
adjust
  :: (Num a)
  => ClosedBounded01Num a  -- ^ Adjustment rate
  -> a -- ^ From
  -> a -- ^ To
  -> a
adjust (ClosedBounded01Num rate) a b = a + rate * (b - a)

newtype ClampHyperparameters a = ClampHyperparameters (ClosedBounded01Num (Exp a))

-- | Return default 'ClampHyperparameters'.
defaultClampHyperparameters
  :: (Fractional a, Ord a, Elt a) => ClampHyperparameters a
defaultClampHyperparameters =
  ClampHyperparameters $ ClosedBounded01Num $ A.constant 0.9

-- | Return 'ClampHyperparameters'.
clampHyperparameters
  :: (Fractional a, Ord a, A.Elt a)
  => a -- ^ Threshold, in range [0,1].
  -> Maybe (ClampHyperparameters a)
clampHyperparameters =
  fmap (ClampHyperparameters . aConstantCB01N) . closedBounded01Num

-- | Constrain probabilities
-- bounded by given threshold.
-- Threshold squeezes towards the center,
-- 0.5 .
clamp
  :: (A.Fractional a, A.Ord a)
  => ClampHyperparameters a -- ^ Threshold
  -> State a
  -> State a
clamp (ClampHyperparameters (ClosedBounded01Num t)) (State (T2 ps g)) =
  State $ T2 (A.map (A.min ub . A.max lb) ps) g where
  lb = A.cond (t A.> 0.5) (1 - t) t
  ub = A.cond (t A.> 0.5) t (1 - t)

-- | Finalize 'State' probabilities into 'Bits'.
finalize :: (A.Fractional a, A.Ord a) => State a -> Acc (Vector Bool)
finalize (State (T2 ps _)) = A.map (A.>= 0.5) ps
