module Data.Type.Heterogeneous where

import Import

withHRefl :: forall ka (a :: ka) kb (b :: kb) (r :: Type). a :~~: b -> ((a ~~ b) => r) -> r
withHRefl HRefl r = r

-- | somewhat awkwardly named
hetHomoEq :: forall (k :: Type) (a :: k) (b :: k). a :~~: b -> a :~: b
hetHomoEq HRefl = Refl

class TestHetEquality (w :: forall k. k -> Type) where
    testHetEquality :: forall (ka :: Type) (a :: ka) (kb :: Type) (b :: kb). w a -> w b -> Maybe (a :~~: b)

-- | Equivalent to (:~~:), but can be made an instance of 'TestHetEquality'
type HetEqual :: forall ka. ka -> forall kb. kb -> Type
data HetEqual a b where
    HetRefl :: forall k (a :: k). HetEqual a a

instance forall k (a :: k). TestHetEquality (HetEqual a) where
    testHetEquality HetRefl HetRefl = Just HRefl

type HetConstraintWitness :: forall k1. (k1 -> Constraint) -> forall k2. k2 -> Type
data HetConstraintWitness c t where
    MkHetConstraintWitness
        :: forall k (c :: k -> Constraint) (t :: k). c t
        => HetConstraintWitness c t

type HetConstraint :: forall k1. (k1 -> Constraint) -> forall k2. k2 -> Constraint
class HetConstraint c t where
    hetConstraint :: HetConstraintWitness c t

instance forall k (c :: k -> Constraint) (t :: k). c t => HetConstraint c t where
    hetConstraint = MkHetConstraintWitness
