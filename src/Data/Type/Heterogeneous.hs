module Data.Type.Heterogeneous where

import Data.Kind
import Data.Type.Equality
import Prelude

-- | somewhat awkwardly named
homoHetEq :: forall (k :: Type) (a :: k) (b :: k). a :~~: b -> a :~: b
homoHetEq HRefl = Refl

class TestHetEquality (w :: forall k. k -> Type) where
    testHetEquality :: forall (ka :: Type) (a :: ka) (kb :: Type) (b :: kb). w a -> w b -> Maybe (a :~~: b)
