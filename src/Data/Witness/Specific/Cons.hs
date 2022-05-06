module Data.Witness.Specific.Cons where

import Data.Witness.General.Finite
import Data.Witness.General.Representative
import Data.Witness.General.WitnessConstraint
import Data.Witness.Specific.All
import Data.Witness.Specific.List.Element
import Data.Witness.Specific.List.List
import Import

type EmptyType :: forall k. k -> Type
newtype EmptyType t =
    MkEmptyType Void
    deriving (Eq, Countable, Searchable, Empty)

instance Finite (EmptyType t) where
    allValues = []
    assemble _ = pure never

instance TestEquality EmptyType where
    testEquality = never

instance Representative EmptyType where
    getRepWitness = never

instance FiniteWitness EmptyType where
    assembleWitnessFor _ = pure emptyAllFor

instance WitnessConstraint c EmptyType where
    witnessConstraint = never

emptyAllOf :: AllOf EmptyType
emptyAllOf = MkAllOf never

emptyAllFor :: AllFor f EmptyType
emptyAllFor = MkAllFor never

type ConsType :: forall k. k -> (k -> Type) -> k -> Type
data ConsType a r t where
    FirstType :: ConsType t r t
    RestType :: r t -> ConsType a r t

instance TestEquality r => TestEquality (ConsType a r) where
    testEquality FirstType FirstType = return Refl
    testEquality (RestType r1) (RestType r2) = do
        Refl <- testEquality r1 r2
        return Refl
    testEquality _ _ = Nothing

instance FiniteWitness r => FiniteWitness (ConsType a r) where
    assembleWitnessFor getsel =
        (\f (MkAllFor r) ->
             MkAllFor $ \wt ->
                 case wt of
                     FirstType -> f
                     RestType rt -> r rt) <$>
        getsel FirstType <*>
        assembleWitnessFor (getsel . RestType)

instance (c a, WitnessConstraint c r) => WitnessConstraint c (ConsType a r) where
    witnessConstraint FirstType = Dict
    witnessConstraint (RestType rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

consAllOf :: a -> AllOf r -> AllOf (ConsType a r)
consAllOf a (MkAllOf tup) =
    MkAllOf $ \esel ->
        case esel of
            FirstType -> a
            RestType sel -> tup sel

type IsFiniteConsWitness :: forall k. (k -> Type) -> Constraint
class Is (ListType Proxy) (FiniteConsWitness sel) => IsFiniteConsWitness (sel :: k -> Type) where
    type FiniteConsWitness sel :: [k]
    toFiniteConsElement :: forall t. sel t -> ListElementType (FiniteConsWitness sel) t
    fromFiniteConsElement :: forall t. ListElementType (FiniteConsWitness sel) t -> sel t

instance Is (ListType Proxy) edits => IsFiniteConsWitness (ListElementType edits) where
    type FiniteConsWitness (ListElementType edits) = edits
    toFiniteConsElement = id
    fromFiniteConsElement = id

instance IsFiniteConsWitness EmptyType where
    type FiniteConsWitness EmptyType = '[]
    toFiniteConsElement wit = never wit
    fromFiniteConsElement lt = never lt

instance IsFiniteConsWitness lt => IsFiniteConsWitness (ConsType a lt) where
    type FiniteConsWitness (ConsType a lt) = a : (FiniteConsWitness lt)
    toFiniteConsElement FirstType = FirstElementType
    toFiniteConsElement (RestType sel) = RestElementType $ toFiniteConsElement sel
    fromFiniteConsElement FirstElementType = FirstType
    fromFiniteConsElement (RestElementType lt) = RestType $ fromFiniteConsElement lt
