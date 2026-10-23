{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
import Test.Tasty.Bench ( defaultMain, Benchmark, bgroup, bench, nf, env )
import GHC.Generics
import Control.DeepSeq

import IC.Colour
import LSystems
import Examples

main :: IO ()
main = defaultMain [benchmarks]

benchmarks :: Benchmark
benchmarks = bgroup "lsystems"
  [ bench "trace1" (nf (\cs -> trace1 cs (angle bush) Blue) cmds)
  , bench "trace2" (nf (\cs -> trace2 cs (angle bush) Blue) cmds)
  ]
  where
    !cmds = force (expandLSystem bush 5)

-- With the trace1 algorithm (recusion), the time taken to process expandLSystem bush 5 was
-- 247 microseconds +- 21 microseconds
-- Meanwhile, with the trace2 algorithm (explicit stack), the time taken to process expandLSystem bush 5 was
-- 201 microseconds +- 10 microseconds
-- This means trace2 was approximately 19% faster than trace1, and a possible explanation is that trace2 
-- uses an explicit stack to manage branches, whereas trace1 relies on recursive calls and list concatenation (++),
-- which can introduce extra overhead.
-- Therefore, trace2 appears more efficient for this input!

-- 'ere be Dragons
-- This stuff is used to allow for the strict evaluation of Commands and Colours
-- Don't worry about how this works :)
deriving instance Generic Command
deriving instance NFData Command
deriving instance Generic Colour
deriving instance NFData Colour
