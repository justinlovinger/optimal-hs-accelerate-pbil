# optimal-hs-accelerate-pbil

Run PBIL derivative-free (metaheuristic) optimization
backed by [Accelerate](https://github.com/AccelerateHS/accelerate).

Construct a single PBIL step like:
```haskell
PBIL.mutate (PBIL.defaultMutateHyperparameters n)
  . PBIL.step PBIL.defaultStepHyperparameters f`
```
where `n` is the number of bits in a binary problem vector
and `f` is an objective function.
Optimize until convergence like:
```haskell
import qualified Data.Array.Accelerate         as A
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL
                                               as PBIL

optimize = do
  s0 <- PBIL.initialState n
  pure $ PBIL.finalize $ PBIL.awhile
    ( A.map A.not
    . PBIL.isConverged PBIL.defaultIsConvergedHyperparameters
    )
    ( PBIL.mutate (PBIL.defaultMutateHyperparameters n)
    . PBIL.step PBIL.defaultStepHyperparameters f
    )
    s0
```
