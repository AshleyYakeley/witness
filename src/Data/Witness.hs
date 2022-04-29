module Data.Witness
    ( module I
    , module Data.Witness
    ) where

import Data.IndNat as I
import Data.Proxy as I
import Data.Type.Apply as I
import Data.Type.Equality as I
import Data.Type.Heterogeneous as I
import Data.Type.With as I
import Data.Witness.General.Constraint as I
import Data.Witness.General.Finite as I
import Data.Witness.General.Representative as I
import Data.Witness.General.WitnessValue as I
import Data.Witness.Specific.All as I
import Data.Witness.Specific.Any as I
import Data.Witness.Specific.ApplyStack as I
import Data.Witness.Specific.Concat as I
import Data.Witness.Specific.Cons as I
import Data.Witness.Specific.Either as I
import Data.Witness.Specific.IndNat as I
import Data.Witness.Specific.List.Element as I
import Data.Witness.Specific.List.List as I
import Data.Witness.Specific.List.Product as I
import Data.Witness.Specific.List.Sum as I
import Data.Witness.Specific.Natural as I
import Data.Witness.Specific.Pair as I
import Data.Witness.Specific.Saturate as I
import Data.Witness.Specific.Single as I
import Data.Witness.Specific.Submap as I
import Data.Witness.Specific.Symbol as I
import Data.Witness.Specific.WitnessDict as I
import Data.Witness.Specific.WitnessFDict as I
import Prelude

-- | See whether two represented and witnessed types are the same.
matchIs ::
       forall w a b. (TestEquality w, Is w a, Is w b)
    => Proxy w
    -> Maybe (a :~: b)
matchIs _ = testEquality r r
  where
    r :: forall t. (Is w t)
      => w t
    r = representative
