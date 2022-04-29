module Data.Witness.Specific.Either where

import Data.Witness.General.Constraint
import Data.Witness.General.Finite
import Data.Witness.Specific.All
import Import

type EitherType :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type)
data EitherType colsel1 colsel2 t
    = LeftType (colsel1 t)
    | RightType (colsel2 t)

instance (TestEquality colsel1, TestEquality colsel2) => TestEquality (EitherType colsel1 colsel2) where
    testEquality (LeftType s1) (LeftType s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality (RightType s1) (RightType s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality _ _ = Nothing

instance (FiniteWitness p, FiniteWitness q) => FiniteWitness (EitherType p q) where
    assembleWitnessF getsel =
        (\(MkAllF p) (MkAllF q) ->
             MkAllF $ \wt ->
                 case wt of
                     LeftType rt -> p rt
                     RightType rt -> q rt) <$>
        assembleWitnessF (getsel . LeftType) <*>
        assembleWitnessF (getsel . RightType)

instance (WitnessConstraint c p, WitnessConstraint c q) => WitnessConstraint c (EitherType p q) where
    witnessConstraint (LeftType rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict
    witnessConstraint (RightType rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

instance (Show (p t), Show (q t)) => Show (EitherType p q t) where
    show (LeftType rt) = show rt
    show (RightType rt) = show rt

instance (AllWitnessConstraint Show p, AllWitnessConstraint Show q) => AllWitnessConstraint Show (EitherType p q) where
    allWitnessConstraint :: forall t. Dict (Show (EitherType p q t))
    allWitnessConstraint =
        case allWitnessConstraint @_ @_ @Show @p @t of
            Dict ->
                case allWitnessConstraint @_ @_ @Show @q @t of
                    Dict -> Dict

eitherAll :: AllValue sel1 -> AllValue sel2 -> AllValue (EitherType sel1 sel2)
eitherAll (MkAllValue tup1) (MkAllValue tup2) =
    MkAllValue $ \esel ->
        case esel of
            LeftType sel -> tup1 sel
            RightType sel -> tup2 sel

eitherAllF :: AllF sel1 f -> AllF sel2 f -> AllF (EitherType sel1 sel2) f
eitherAllF (MkAllF tup1) (MkAllF tup2) =
    MkAllF $ \esel ->
        case esel of
            LeftType sel -> tup1 sel
            RightType sel -> tup2 sel
