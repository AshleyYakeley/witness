module Data.Witness.Cons where

import Data.Constraint
import Data.Countable
import Data.Empty
import Data.Searchable
import Data.Type.Equality
import Data.Witness.All
import Data.Witness.Constraint
import Data.Witness.Finite
import Data.Witness.List
import Data.Witness.ListElement

newtype EmptyWitness t =
    MkEmptyWitness None
    deriving (Eq, Countable, Searchable, Empty)

instance Finite (EmptyWitness t) where
    allValues = []
    assemble _ = pure never

instance TestEquality EmptyWitness where
    testEquality = never

instance FiniteWitness EmptyWitness where
    assembleWitnessF _ = pure emptyAllF

instance WitnessConstraint c EmptyWitness where
    witnessConstraint = never

emptyAll :: All EmptyWitness
emptyAll = MkAll never

emptyAllF :: AllF EmptyWitness f
emptyAllF = MkAllF never

data ConsWitness a r t where
    FirstWitness :: ConsWitness t r t
    RestWitness :: r t -> ConsWitness a r t

instance TestEquality r => TestEquality (ConsWitness a r) where
    testEquality FirstWitness FirstWitness = return Refl
    testEquality (RestWitness r1) (RestWitness r2) = do
        Refl <- testEquality r1 r2
        return Refl
    testEquality _ _ = Nothing

instance FiniteWitness r => FiniteWitness (ConsWitness a r) where
    assembleWitnessF getsel =
        (\f (MkAllF r) ->
             MkAllF $ \wt ->
                 case wt of
                     FirstWitness -> f
                     RestWitness rt -> r rt) <$>
        getsel FirstWitness <*>
        assembleWitnessF (getsel . RestWitness)

instance (c a, WitnessConstraint c r) => WitnessConstraint c (ConsWitness a r) where
    witnessConstraint FirstWitness = Dict
    witnessConstraint (RestWitness rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

consAll :: a -> All r -> All (ConsWitness a r)
consAll a (MkAll tup) =
    MkAll $ \esel ->
        case esel of
            FirstWitness -> a
            RestWitness sel -> tup sel

class KnownList (FiniteConsWitness sel) =>
      IsFiniteConsWitness (sel :: k -> *) where
    type FiniteConsWitness sel :: [k]
    toLTW :: forall t. sel t -> ListThingWitness (FiniteConsWitness sel) t
    fromLTW :: forall t. ListThingWitness (FiniteConsWitness sel) t -> sel t

instance KnownList edits => IsFiniteConsWitness (ListThingWitness edits) where
    type FiniteConsWitness (ListThingWitness edits) = edits
    toLTW = id
    fromLTW = id

instance IsFiniteConsWitness EmptyWitness where
    type FiniteConsWitness EmptyWitness = '[]
    toLTW wit = never wit
    fromLTW lt = never lt

instance IsFiniteConsWitness lt => IsFiniteConsWitness (ConsWitness a lt) where
    type FiniteConsWitness (ConsWitness a lt) = a : (FiniteConsWitness lt)
    toLTW FirstWitness = FirstListThingWitness
    toLTW (RestWitness sel) = RestListThingWitness $ toLTW sel
    fromLTW FirstListThingWitness = FirstWitness
    fromLTW (RestListThingWitness lt) = RestWitness $ fromLTW lt