{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Type.Heterogeneous where

import Data.Kind
import Data.Type.Equality
import Prelude

withHRefl :: forall ka (a :: ka) kb (b :: kb) (r :: Type). a :~~: b -> ((a ~~ b) => r) -> r
withHRefl HRefl r = r

-- | somewhat awkwardly named
homoHetEq :: forall (k :: Type) (a :: k) (b :: k). a :~~: b -> a :~: b
homoHetEq HRefl = Refl

class TestHetEquality (w :: forall k. k -> Type) where
    testHetEquality :: forall (ka :: Type) (a :: ka) (kb :: Type) (b :: kb). w a -> w b -> Maybe (a :~~: b)

-- | Equivalent to (:~~:), but can be made an instance of 'TestHetEquality'
type HetEqual :: forall ka. ka -> forall kb. kb -> Type
data HetEqual a b where
    HetRefl :: forall k (a :: k). HetEqual a a

instance forall k (a :: k). TestHetEquality (HetEqual a) where
    testHetEquality HetRefl HetRefl = Just HRefl
