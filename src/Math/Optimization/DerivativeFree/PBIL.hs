{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Math.Optimization.DerivativeFree.PBIL
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
import qualified Data.Array.Accelerate.Interpreter
                                               as AI
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
  deriving (Show)

-- | Return recommended initial 'State'.
initialState
  :: Int -- ^ number of bits in each sample
  -> IO (State Double)
initialState nb = do
  g <- createWith . A.use <$> MWC.randomArray MWC.uniform sh
  pure $ State $ T2 (A.fill (A.constant sh) $ A.constant 0.5) g
  where sh = Z :. nb

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
  = Just $ State $ T2 (A.use ps) (createWith $ A.use g)

-- | Return 'State' without safety checking.
-- Probabilities must be in range [0,1].
-- Random generator must have same shape as probabilities.
unsafeState
  :: (Elt a)
  => Acc (Vector a, Gen) -- ^ (probabilities, random state)
  -> State a
unsafeState = State

fromState :: (Elt a) => State a -> Acc (Vector a, Gen)
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
  { sampleSize :: Exp Int
  , adjustRate :: ClosedBounded01Num (Exp a)
  }
  deriving Show

-- | Return default 'StepHyperparameters'.
defaultStepHyperparameters
  :: (A.Fractional a, A.Ord a) => StepHyperparameters a
defaultStepHyperparameters = StepHyperparameters 20 (ClosedBounded01Num 0.1)

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
  = StepHyperparameters (A.constant n) . aConstantCB01N <$> closedBounded01Num r

-- | Take 1 step towards a 'State' with a higher objective value.
-- by adjusting probabilities towards the best bits
-- in a set of samples.
step
  :: (A.Num a, A.Ord a, Uniform a, A.Ord b)
  => StepHyperparameters a
  -> (Acc (Vector Bool) -> Acc (Scalar b)) -- ^ objective function, maximize
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
  deriving Show

-- | Return default 'MutateHyperparameters'.
defaultMutateHyperparameters
  :: (A.Fractional a, A.Ord a)
  => Int -- ^ number of bits in each sample
  -> MutateHyperparameters a
defaultMutateHyperparameters n = MutateHyperparameters
  (ClosedBounded01Num . f $ n)
  (ClosedBounded01Num 0.05)
 where
  f 0 = 1
  f x = 1 / fromIntegral x

-- | Return 'MutateHyperparameters' if valid.
mutateHyperparameters
  :: (Fractional a, Ord a, Elt a)
  => a -- ^ mutation chance, in range [0,1]
  -> a -- ^ mutation adjust rate, in range [0,1]
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
  => ClosedBounded01Num (Exp a) -- ^ adjustment rate
  -> Acc (Array sh a) -- ^ from
  -> Acc (Array sh a) -- ^ to
  -> Acc (Array sh a)
adjustArray rate = A.zipWith (adjust rate)

-- | Adjust a number from `a` to `b`
-- at given rate.
adjust
  :: (Num a)
  => ClosedBounded01Num a  -- ^ adjustment rate
  -> a -- ^ from
  -> a -- ^ to
  -> a
adjust (ClosedBounded01Num rate) a b = a + rate * (b - a)

newtype IsConvergedHyperparameters a = IsConvergedHyperparameters (Exp a)
  deriving (Show)

-- | Return default 'IsConvergedHyperparameters'.
defaultIsConvergedHyperparameters
  :: (Fractional a, Ord a, Elt a) => IsConvergedHyperparameters a
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
isConverged (IsConvergedHyperparameters ub) (State (T2 ps _)) = A.all
  (\x -> x A.< lb A.|| x A.> ub)
  ps
  where lb = 1 - ub

-- | Finalize 'State' probabilities into bits.
finalize :: (A.Fractional a, A.Ord a) => State a -> Acc (Vector Bool)
finalize (State (T2 ps _)) = A.map (A.>= 0.5) ps
