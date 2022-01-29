# optimal-hs-accelerate-pbil

Run PBIL derivative-free (metaheuristic) optimization
backed by [Accelerate](https://github.com/AccelerateHS/accelerate).

Optimize until convergence like:
```haskell
import qualified Data.Array.Accelerate         as A
import qualified Math.Optimization.Accelerate.DerivativeFree.PBIL.Default
                                               as PBILD

optimize = do
  s0 <- PBILD.initialState n
  pure $ PBILD.finalize $ A.awhile
    (A.map A.not . PBILD.isConverged)
    (PBILD.step n f)
    s0
```
where `n` is the number of bits in a binary problem vector
and `f` is an objective function.
