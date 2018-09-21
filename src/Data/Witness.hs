module Data.Witness
    ( module Data.Proxy
    , module Data.Type.Equality
    , module Data.Nat
    , module Data.Witness.Kind
    , module Data.Witness.Any
    , module Data.Witness.All
    , module Data.Witness.Constraint
    , module Data.Witness.Symbol
    , module Data.Witness.Finite
    , module Data.Witness.Single
    , module Data.Witness.Cons
    , module Data.Witness.Either
    , module Data.Witness.Submap
    , module Data.Witness.WitnessDict
    , module Data.Witness.WitnessFDict
    , module Data.Witness.Nat
    , module Data.Witness.ListElement
    , module Data.Witness.List
    , module Data.Witness.Representative
    , module Data.Witness
    ) where

import Data.Nat
import Data.Proxy
import Data.Type.Equality
import Data.Witness.All
import Data.Witness.Any
import Data.Witness.Cons
import Data.Witness.Constraint
import Data.Witness.Either
import Data.Witness.Finite
import Data.Witness.Kind
import Data.Witness.List
import Data.Witness.ListElement
import Data.Witness.Nat
import Data.Witness.Representative
import Data.Witness.Single
import Data.Witness.Submap
import Data.Witness.Symbol
import Data.Witness.WitnessDict
import Data.Witness.WitnessFDict
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
