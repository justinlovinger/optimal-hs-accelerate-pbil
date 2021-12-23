# optimal-hs-accelerate-pbil

Run PBIL derivative-free (metaheuristic) optimization
backed by [Accelerate](https://github.com/AccelerateHS/accelerate).

Optimize until convergence like:
```haskell
import qualified Data.Array.Accelerate         as A
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL
                                               as PBIL

optimize = do
  s0 <- PBIL.initialState PBIL.defaultNumSamples n
  pure $ PBIL.finalize $ A.awhile
    ( A.map A.not
    . PBIL.isConverged PBIL.defaultIsConvergedHyperparameters
    )
    (PBIL.step (PBIL.defaultStepHyperparameters n) f)
    s0
```
where `n` is the number of bits in a binary problem vector
and `f` is an objective function.
