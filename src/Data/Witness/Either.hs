module Data.Witness.Either where

import Data.Constraint
import Data.Type.Equality
import Data.Witness.All
import Data.Witness.Constraint
import Data.Witness.Finite
import Prelude

data EitherWitness (colsel1 :: k -> *) (colsel2 :: k -> *) (t :: k)
    = LeftWitness (colsel1 t)
    | RightWitness (colsel2 t)

instance (TestEquality colsel1, TestEquality colsel2) => TestEquality (EitherWitness colsel1 colsel2) where
    testEquality (LeftWitness s1) (LeftWitness s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality (RightWitness s1) (RightWitness s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality _ _ = Nothing

instance (FiniteWitness p, FiniteWitness q) => FiniteWitness (EitherWitness p q) where
    assembleWitnessF getsel =
        (\(MkAllF p) (MkAllF q) ->
             MkAllF $ \wt ->
                 case wt of
                     LeftWitness rt -> p rt
                     RightWitness rt -> q rt) <$>
        assembleWitnessF (getsel . LeftWitness) <*>
        assembleWitnessF (getsel . RightWitness)

instance (WitnessConstraint c p, WitnessConstraint c q) => WitnessConstraint c (EitherWitness p q) where
    witnessConstraint (LeftWitness rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict
    witnessConstraint (RightWitness rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

instance (Show (p t), Show (q t)) => Show (EitherWitness p q t) where
    show (LeftWitness rt) = show rt
    show (RightWitness rt) = show rt

instance (AllWitnessConstraint Show p, AllWitnessConstraint Show q) => AllWitnessConstraint Show (EitherWitness p q) where
    allWitnessConstraint :: forall t. Dict (Show (EitherWitness p q t))
    allWitnessConstraint =
        case allWitnessConstraint @_ @_ @Show @p @t of
            Dict ->
                case allWitnessConstraint @_ @_ @Show @q @t of
                    Dict -> Dict

eitherAll :: AllValue sel1 -> AllValue sel2 -> AllValue (EitherWitness sel1 sel2)
eitherAll (MkAllValue tup1) (MkAllValue tup2) =
    MkAllValue $ \esel ->
        case esel of
            LeftWitness sel -> tup1 sel
            RightWitness sel -> tup2 sel

eitherAllF :: AllF sel1 f -> AllF sel2 f -> AllF (EitherWitness sel1 sel2) f
eitherAllF (MkAllF tup1) (MkAllF tup2) =
    MkAllF $ \esel ->
        case esel of
            LeftWitness sel -> tup1 sel
            RightWitness sel -> tup2 sel
