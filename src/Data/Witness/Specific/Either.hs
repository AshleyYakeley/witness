module Data.Witness.Specific.Either where

import Data.Witness.General.AllConstraint
import Data.Witness.General.Finite
import Data.Witness.General.WitnessConstraint
import Data.Witness.Specific.All
import Import

type EitherType :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type)
data EitherType w1 w2 t
    = LeftType (w1 t)
    | RightType (w2 t)

instance (TestEquality w1, TestEquality w2) => TestEquality (EitherType w1 w2) where
    testEquality (LeftType s1) (LeftType s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality (RightType s1) (RightType s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality _ _ = Nothing

instance (FiniteWitness p, FiniteWitness q) => FiniteWitness (EitherType p q) where
    assembleWitnessFor getsel =
        (\(MkAllFor p) (MkAllFor q) ->
             MkAllFor $ \wt ->
                 case wt of
                     LeftType rt -> p rt
                     RightType rt -> q rt) <$>
        assembleWitnessFor (getsel . LeftType) <*>
        assembleWitnessFor (getsel . RightType)

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

instance (AllConstraint Show p, AllConstraint Show q) => AllConstraint Show (EitherType p q) where
    allConstraint :: forall t. Dict (Show (EitherType p q t))
    allConstraint =
        case allConstraint @_ @_ @Show @p @t of
            Dict ->
                case allConstraint @_ @_ @Show @q @t of
                    Dict -> Dict

eitherAllOf :: AllOf sel1 -> AllOf sel2 -> AllOf (EitherType sel1 sel2)
eitherAllOf (MkAllOf tup1) (MkAllOf tup2) =
    MkAllOf $ \esel ->
        case esel of
            LeftType sel -> tup1 sel
            RightType sel -> tup2 sel

eitherAllFor :: AllFor sel1 f -> AllFor sel2 f -> AllFor (EitherType sel1 sel2) f
eitherAllFor (MkAllFor tup1) (MkAllFor tup2) =
    MkAllFor $ \esel ->
        case esel of
            LeftType sel -> tup1 sel
            RightType sel -> tup2 sel
