
module Observable (
    module OModule
  ) where

import Observable.Core as OModule
-- import Observable.Distribution as OModule (
--     beta
--   , binomial
--   , standard
--   , normal
--   , student
--   , gamma
--   , invGamma
--   , uniform
--   , dirichlet
--   , symmetricDirichlet
--   , categorical
--   , discreteUniform
--   , isoGauss
--   , poisson
--   , exponential
--   )
import Observable.Interpreter as OModule
import Observable.Utils as OModule (
    parameters
  , observations
  )

