module Data.Type.Apply where

import Control.Category
import Data.Kind
import Data.Type.Equality

applyRefl ::
       forall k1 k2 (fa :: k1 -> k2) (fb :: k1 -> k2) (a :: k1) (b :: k1). fa :~: fb -> a :~: b -> (fa a) :~: (fb b)
applyRefl Refl Refl = Refl

reflId ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Category cat
    => a :~: b -> cat a b
reflId Refl = id
