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
import Data.Witness.All as I
import Data.Witness.Any as I
import Data.Witness.ApplyStack as I
import Data.Witness.Concat as I
import Data.Witness.Cons as I
import Data.Witness.Constraint as I
import Data.Witness.Either as I
import Data.Witness.Finite as I
import Data.Witness.HList as I
import Data.Witness.IndNat as I
import Data.Witness.List as I
import Data.Witness.ListElement as I
import Data.Witness.ListSum as I
import Data.Witness.Natural as I
import Data.Witness.Pair as I
import Data.Witness.Representative as I
import Data.Witness.Saturate as I
import Data.Witness.Single as I
import Data.Witness.Submap as I
import Data.Witness.Symbol as I
import Data.Witness.WitnessDict as I
import Data.Witness.WitnessFDict as I
import Data.Witness.WitnessValue as I
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
