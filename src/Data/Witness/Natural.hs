module Data.Witness.Natural
    ( KnownNat
    , type (<=)
    , type (+)
    , type (*)
    , type (^)
    , type (-)
    , CmpNat
    , Div
    , Mod
    , Log2
    , TNatural
    , NaturalType(..)
    , zeroNaturalType
    , succNaturalType
    , addNaturalType
    , multiplyNaturalType
    , NaturalListElement
    , NaturalListElementType(..)
    , naturalCountListType
    ) where

import Data.Witness.Constraint
import Data.Witness.List
import Data.Witness.Representative
import Data.Witness.WitnessValue
import GHC.TypeLits as GHC
import Import
import Numeric.Natural
import Unsafe.Coerce

type TNatural = GHC.Nat

type NaturalType :: TNatural -> Type
data NaturalType bn where
    MkNaturalType :: KnownNat bn => NaturalType bn

instance WitnessValue NaturalType where
    type WitnessValueType NaturalType = Natural
    witnessToValue :: forall t. NaturalType t -> Natural
    witnessToValue MkNaturalType = fromInteger $ natVal (Proxy :: Proxy t)
    valueToWitness i cont =
        case someNatVal $ toInteger i of
            Just (SomeNat p) -> let
                psw :: forall (t :: TNatural). KnownNat t
                    => Proxy t
                    -> NaturalType t
                psw _ = MkNaturalType
                in cont $ psw p
            Nothing -> error "negative Natural"

instance TestEquality NaturalType where
    testEquality (MkNaturalType :: NaturalType a) (MkNaturalType :: NaturalType b) = sameNat (Proxy @a) (Proxy @b)

instance Representative NaturalType where
    getRepWitness MkNaturalType = Dict

instance KnownNat bn => Is NaturalType bn where
    representative = MkNaturalType

instance Show (NaturalType bn) where
    show = show . witnessToValue

instance AllWitnessConstraint Show NaturalType where
    allWitnessConstraint = Dict

unsafeNaturalType :: forall n. Natural -> NaturalType n
unsafeNaturalType i = valueToWitness i $ \(nt :: NaturalType t) -> unsafeCoerce @(NaturalType t) @(NaturalType n) nt

zeroNaturalType :: NaturalType 0
zeroNaturalType = MkNaturalType

succNaturalType :: NaturalType a -> NaturalType (a + 1)
succNaturalType a = unsafeNaturalType $ succ $ witnessToValue a

addNaturalType :: NaturalType a -> NaturalType b -> NaturalType (a + b)
addNaturalType a b = unsafeNaturalType $ witnessToValue a + witnessToValue b

multiplyNaturalType :: NaturalType a -> NaturalType b -> NaturalType (a * b)
multiplyNaturalType a b = unsafeNaturalType $ witnessToValue a * witnessToValue b

type NaturalListElement :: TNatural -> forall k. [k] -> k
type family NaturalListElement n tt where
    NaturalListElement 0 (t : tt) = t
    NaturalListElement n (t : tt) = NaturalListElement (n - 1) tt

type NaturalListElementType :: forall k. [k] -> k -> Type
data NaturalListElementType tt t where
    MkNaturalListElementType :: NaturalType n -> NaturalListElementType tt (NaturalListElement n tt)

instance TestEquality (NaturalListElementType tt) where
    testEquality (MkNaturalListElementType na) (MkNaturalListElementType nb) = do
        Refl <- testEquality na nb
        return Refl

unsafeNaturalCountListTypeN :: forall w lt lt'. Natural -> ListType w lt -> ListType (NaturalListElementType lt') lt
unsafeNaturalCountListTypeN _ NilListType = NilListType
unsafeNaturalCountListTypeN n (ConsListType _ lt) =
    ConsListType
        (valueToWitness n $
         unsafeCoerce @(NaturalListElementType lt' _) @(NaturalListElementType lt' _) . MkNaturalListElementType) $
    unsafeNaturalCountListTypeN (succ n) lt

naturalCountListType :: forall w lt. ListType w lt -> ListType (NaturalListElementType lt) lt
naturalCountListType = unsafeNaturalCountListTypeN 0
