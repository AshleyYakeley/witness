module Data.Witness.Cons where

import Data.Constraint
import Data.Countable
import Data.Empty
import Data.Kind
import Data.Proxy
import Data.Searchable
import Data.Type.Equality
import Data.Void
import Data.Witness.All
import Data.Witness.Constraint
import Data.Witness.Finite
import Data.Witness.List
import Data.Witness.ListElement
import Data.Witness.Representative
import Prelude

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
    assembleWitnessF _ = pure emptyAllF

instance WitnessConstraint c EmptyType where
    witnessConstraint = never

emptyAll :: AllValue EmptyType
emptyAll = MkAllValue never

emptyAllF :: AllF EmptyType f
emptyAllF = MkAllF never

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
    assembleWitnessF getsel =
        (\f (MkAllF r) ->
             MkAllF $ \wt ->
                 case wt of
                     FirstType -> f
                     RestType rt -> r rt) <$>
        getsel FirstType <*>
        assembleWitnessF (getsel . RestType)

instance (c a, WitnessConstraint c r) => WitnessConstraint c (ConsType a r) where
    witnessConstraint FirstType = Dict
    witnessConstraint (RestType rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

consAll :: a -> AllValue r -> AllValue (ConsType a r)
consAll a (MkAllValue tup) =
    MkAllValue $ \esel ->
        case esel of
            FirstType -> a
            RestType sel -> tup sel

type IsFiniteConsWitness :: forall k. (k -> Type) -> Constraint
class Is (ListType Proxy) (FiniteConsWitness sel) => IsFiniteConsWitness (sel :: k -> Type) where
    type FiniteConsWitness sel :: [k]
    toLTW :: forall t. sel t -> ListElementType (FiniteConsWitness sel) t
    fromLTW :: forall t. ListElementType (FiniteConsWitness sel) t -> sel t

instance Is (ListType Proxy) edits => IsFiniteConsWitness (ListElementType edits) where
    type FiniteConsWitness (ListElementType edits) = edits
    toLTW = id
    fromLTW = id

instance IsFiniteConsWitness EmptyType where
    type FiniteConsWitness EmptyType = '[]
    toLTW wit = never wit
    fromLTW lt = never lt

instance IsFiniteConsWitness lt => IsFiniteConsWitness (ConsType a lt) where
    type FiniteConsWitness (ConsType a lt) = a : (FiniteConsWitness lt)
    toLTW FirstType = FirstElementType
    toLTW (RestType sel) = RestElementType $ toLTW sel
    fromLTW FirstElementType = FirstType
    fromLTW (RestElementType lt) = RestType $ fromLTW lt
