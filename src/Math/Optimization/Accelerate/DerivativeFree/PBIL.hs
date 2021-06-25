{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Math.Optimization.Accelerate.DerivativeFree.PBIL
  ( State
  , initialState
  , state
  , unsafeState
  , fromState
  , awhile
  , StepHyperparameters
  , defaultStepHyperparameters
  , stepHyperparameters
  , step
  , MutateHyperparameters
  , defaultMutateHyperparameters
  , mutateHyperparameters
  , mutate
  , IsConvergedHyperparameters
  , defaultIsConvergedHyperparameters
  , isConvergedHyperparameters
  , isConverged
  , finalize
  ) where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as AI
import qualified Data.Array.Accelerate.System.Random.MWC
                                               as MWC
import qualified Data.Array.Accelerate.System.Random.SFC
                                               as SFC

newtype State a = State (A.Acc (A.Vector a, SFC.Gen))
  deriving (Show)

-- | Return recommended initial 'State'.
initialState
  :: Int -- ^ number of bits in each sample
  -> IO (State Double)
initialState nb = do
  g <- SFC.createWith . A.use <$> MWC.randomArray MWC.uniform sh
  pure $ State $ A.T2 (A.fill (A.constant sh) $ A.constant 0.5) g
  where sh = A.Z A.:. nb

-- | Return 'State' if valid.
state
  :: (A.Fractional a, A.Ord a)
  => A.Vector a -- ^ probabilities, values in range [0,1]
  -> A.Vector (A.Word64, A.Word64, A.Word64) -- ^ seed, same length as probabilities
  -> Maybe (State a)
state ps g
  | A.arrayShape ps /= A.arrayShape g
  = Nothing
  | head $ A.toList $ AI.run $ A.any (\x -> x A.< 0 A.|| x A.> 1) $ A.use ps
  = Nothing
  | otherwise
  = Just $ State $ A.T2 (A.use ps) (SFC.createWith $ A.use g)

-- | Return 'State' without safety checking.
-- Probabilities must be in range [0,1].
-- Random generator must have same shape as probabilities.
unsafeState
  :: (A.Elt a)
  => A.Acc (A.Vector a, SFC.Gen) -- ^ (probabilities, random state)
  -> State a
unsafeState = State

fromState :: (A.Elt a) => State a -> A.Acc (A.Vector a, SFC.Gen)
fromState (State s) = s

-- | Run Accelerate `awhile` with 'State'.
awhile
  :: (A.Elt a)
  => (State a -> A.Acc (A.Scalar Bool))
  -> (State a -> State a)
  -> State a
  -> State a
awhile f g (State x) = State $ A.awhile (f . State) (fromState . g . State) x

data StepHyperparameters a = StepHyperparameters
  { sampleSize :: A.Exp Int
  , adjustRate :: A.Exp a
  }
  deriving Show

-- | Return default 'StepHyperparameters'.
defaultStepHyperparameters
  :: (A.Fractional a, A.Ord a) => StepHyperparameters a
defaultStepHyperparameters = StepHyperparameters 20 0.1

-- | Return 'StepHyperparameters' if valid.
stepHyperparameters
  :: (Fractional a, Ord a, A.Elt a)
  => Int -- ^ sample size, >= 2
  -> a -- ^ adjust rate, in range [0,1]
  -> Maybe (StepHyperparameters a)
stepHyperparameters n r
  | n < 2
  = Nothing
  | otherwise
  = StepHyperparameters (A.constant n) . A.constant <$> maybeInClosedRange01 r

-- | Take 1 step towards a 'State' with a higher objective value.
-- by adjusting probabilities towards the best bits
-- in a set of samples.
step
  :: (A.Num a, A.Ord a, SFC.Uniform a, A.Ord b)
  => StepHyperparameters a
  -> (A.Acc (A.Vector Bool) -> A.Acc (A.Scalar b)) -- ^ objective function, maximize
  -> State a
  -> State a
step (StepHyperparameters n ar) f (State (A.T2 ps g0)) = State $ A.T2 ps' g1 where
  (A.T3 _ bsStar g1) = aiterate
    (n - 1)
    (\(A.T3 fbs bs g) ->
      let (A.T2 bs' g') = sample ps g
          fbs'          = f bs'
      in  A.acond (A.the fbs A.< A.the fbs') (A.T3 fbs' bs' g') (A.T3 fbs bs g')
    )
    (let (A.T2 bs g) = sample ps g0 in A.T3 (f bs) bs g)
  -- 'adjust' from a probability to a bit
  -- will always be a valid probability,
  -- because a bit is 0 or 1
  -- and 'adjust' will return a value between that range.
  ps' = adjustArray ar ps (A.map fromBool bsStar)

-- | Repeatedly apply a function a fixed number of times.
aiterate
  :: (A.Arrays a)
  => A.Exp Int -- ^ number of times to apply function
  -> (A.Acc a -> A.Acc a) -- ^ function to apply
  -> A.Acc a -- ^ initial value
  -> A.Acc a
aiterate n f xs0 = A.asnd $ A.awhile
  (A.unit . (A.< n) . A.the . A.afst)
  (\(A.T2 i xs) -> A.T2 (i !+ 1) (f xs))
  (A.lift (A.unit $ A.constant (0 :: Int), xs0))

-- | Add scalar to each element of an array.
(!+)
  :: (A.Shape sh, A.Num a)
  => A.Acc (A.Array sh a)
  -> A.Exp a
  -> A.Acc (A.Array sh a)
(!+) a x = A.map (+ x) a

sample
  :: (A.Ord a, SFC.Uniform a)
  => A.Acc (A.Vector a)
  -> A.Acc SFC.Gen
  -> A.Acc (A.Vector Bool, SFC.Gen)
sample ps g = A.lift (A.zipWith (A.<=) rs ps, g')
  where (rs, g') = SFC.runRandom g SFC.randomVector

fromBool :: (A.Num a) => A.Exp Bool -> A.Exp a
fromBool x = A.cond x 1 0

data MutateHyperparameters a = MutateHyperparameters
  { mutationChance     :: A.Exp a
  , mutationAdjustRate :: A.Exp a
  }
  deriving Show

-- | Return default 'MutateHyperparameters'.
defaultMutateHyperparameters
  :: (A.Fractional a, A.Ord a)
  => Int -- ^ number of bits in each sample
  -> MutateHyperparameters a
defaultMutateHyperparameters n = MutateHyperparameters (f n) 0.05
 where
  f 0 = 1
  f x = 1 / fromIntegral x

-- | Return 'MutateHyperparameters' if valid.
mutateHyperparameters
  :: (Fractional a, Ord a, A.Elt a)
  => a -- ^ mutation chance, in range [0,1]
  -> a -- ^ mutation adjust rate, in range [0,1]
  -> Maybe (MutateHyperparameters a)
mutateHyperparameters mc mar = do
  mc'  <- maybeInClosedRange01 mc
  mar' <- maybeInClosedRange01 mar
  pure $ MutateHyperparameters (A.constant mc') (A.constant mar')

maybeInClosedRange01 :: (Fractional a, Ord a) => a -> Maybe a
maybeInClosedRange01 x | x < 0     = Nothing
                       | x > 1     = Nothing
                       | otherwise = Just x

-- | Randomly adjust probabilities.
mutate
  :: (A.Num a, A.Ord a, SFC.Uniform a)
  => MutateHyperparameters a
  -> State a
  -> State a
mutate (MutateHyperparameters mc mar) (State (A.T2 ps g0)) = State $ A.T2
  -- 'adjust' from a probability to a number in range 0 to 1
  -- will always be a valid probability,
  -- because 'adjust' will return a value between that range.
  (A.zipWith3 (\r1 p r2 -> A.cond (r1 A.<= mc) (adjust mar p r2) p) rs1 ps rs2)
  g2
 where
  (rs1, g1) = SFC.runRandom g0 SFC.randomVector
  (rs2, g2) = SFC.runRandom g1 SFC.randomVector

-- | Adjust each number in `a` to corresponding number in `b`
-- at given rate.
adjustArray
  :: (A.Shape sh, A.Num a)
  => A.Exp a -- ^ adjustment rate, in range [0, 1]
  -> A.Acc (A.Array sh a) -- ^ from
  -> A.Acc (A.Array sh a) -- ^ to
  -> A.Acc (A.Array sh a)
adjustArray rate = A.zipWith (adjust rate)

-- | Adjust a number from `a` to `b`
-- at given rate.
adjust
  :: (Num a)
  => a  -- ^ adjustment rate, in range [0, 1]
  -> a -- ^ from
  -> a -- ^ to
  -> a
adjust rate a b = a + rate * (b - a)

newtype IsConvergedHyperparameters a = IsConvergedHyperparameters (A.Exp a)
  deriving (Show)

-- | Return default 'IsConvergedHyperparameters'.
defaultIsConvergedHyperparameters
  :: (Fractional a, Ord a, A.Elt a) => IsConvergedHyperparameters a
defaultIsConvergedHyperparameters =
  IsConvergedHyperparameters $ A.constant 0.75

-- | Return 'IsConvergedHyperparameters' if valid.
isConvergedHyperparameters
  :: (Fractional a, Ord a, A.Elt a)
  => a -- ^ threshold, in range (0.5,1)
  -> Maybe (IsConvergedHyperparameters a)
isConvergedHyperparameters t
  | t <= 0.5  = Nothing
  | t >= 1.0  = Nothing
  | otherwise = Just $ IsConvergedHyperparameters $ A.constant t

-- | Has 'State' converged?
isConverged
  :: (A.Fractional a, A.Ord a)
  => IsConvergedHyperparameters a
  -> State a
  -> A.Acc (A.Scalar Bool)
isConverged (IsConvergedHyperparameters ub) (State (A.T2 ps _)) = A.all
  (\x -> x A.< lb A.|| x A.> ub)
  ps
  where lb = 1 - ub

-- | Finalize 'State' probabilities into bits.
finalize :: (A.Fractional a, A.Ord a) => State a -> A.Acc (A.Vector Bool)
finalize (State (A.T2 ps _)) = A.map (A.>= 0.5) ps
