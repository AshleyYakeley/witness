module Data.Witness.Kind where

import Data.Constraint
import Data.Kind
import Data.Witness.Constraint
import Data.Witness.Representative
import GHC.TypeLits
import Prelude
import Unsafe.Coerce

type KindWitness :: forall k -> k -> Type
type family KindWitness k

type InKind (a :: k) = Is (KindWitness k) a

inKind ::
       forall k (a :: k). InKind a
    => KindWitness k a
inKind = representative

-- NoWitness
type NoWitness :: forall k. k -> Type
data NoWitness t =
    MkNoWitness

instance Representative NoWitness where
    getRepWitness MkNoWitness = Dict

instance Is NoWitness a where
    representative = MkNoWitness

-- Type
type instance KindWitness Type = NoWitness

-- Constraint
type instance KindWitness Constraint = NoWitness

-- Nat
type instance KindWitness Nat = NoWitness

-- kp -> kq
type FunctionKindWitness :: forall kp kq. (kp -> Type) -> (kq -> Type) -> ((kp -> kq) -> Type)
newtype FunctionKindWitness (wp :: kp -> Type) wq t =
    MkFunctionKindWitness (forall (p :: kp). wq (t p))

applyFunctionKindWitness ::
       forall kp kq (wp :: kp -> Type) (wq :: kq -> Type) (t :: kp -> kq) (p :: kp) (proxy :: kp -> Type).
       (Representative wq)
    => FunctionKindWitness wp wq t
    -> proxy p
    -> Dict (Is wq (t p))
applyFunctionKindWitness (MkFunctionKindWitness f) _ = withRepresentative (f @p) Dict

type instance KindWitness (kp -> kq) =
     FunctionKindWitness (KindWitness kp) (KindWitness kq)

newtype DT1 (cq :: kq -> Constraint) (f :: kp -> kq) r =
    MkDT1 ((forall (p :: kp). cq (f p)) => r)

newtype DT2 (cq :: kq -> Constraint) (f :: kp -> kq) r =
    MkDT2 ((forall (p :: kp). Dict (cq (f p))) -> r)

dictThing ::
       forall kp kq (cq :: kq -> Constraint) (f :: kp -> kq) r.
       ((forall (p :: kp). cq (f p)) => r)
    -> ((forall (p :: kp). Dict (cq (f p))) -> r)
dictThing f =
    case unsafeCoerce @(DT1 cq f r) @(DT2 cq f r) $ MkDT1 f of
        MkDT2 f' -> f'

functionKindWitness ::
       forall kp kq (wp :: kp -> Type) (wq :: kq -> Type) (f :: kp -> kq) r. (Representative wq)
    => FunctionKindWitness wp wq f
    -> ((forall (p :: kp). Is wq (f p)) => r)
    -> r
functionKindWitness (MkFunctionKindWitness f) call = dictThing @kp @kq @(Is wq) call $ getRepWitness f

instance forall kp kq (wp :: kp -> Type) (wq :: kq -> Type). Representative wq =>
             Representative (FunctionKindWitness wp wq) where
    getRepWitness :: forall (p :: kp -> kq). FunctionKindWitness wp wq p -> Dict (Is (FunctionKindWitness wp wq) p)
    getRepWitness fw = functionKindWitness fw Dict

instance (Representative wq, forall p. Is wq (f p)) =>
             Is (FunctionKindWitness (wp :: kp -> Type) (wq :: kq -> Type)) (f :: kp -> kq) where
    representative = MkFunctionKindWitness representative

deriveIsFunctionKindWitness ::
       forall kp kq (wp :: kp -> Type) (wq :: kq -> Type) (f :: kp -> kq). (Representative wq, forall p. Is wq (f p))
    => Dict (Is (FunctionKindWitness wp wq) f)
deriveIsFunctionKindWitness = Dict

-- (kp,kq)
type PairWitness :: forall kp kq. (kp -> Type) -> (kq -> Type) -> ((kp, kq) -> Type)
data PairWitness wp wq t where
    MkPairWitness :: (Is wp p, Is wq q) => PairWitness wp wq '( p, q)

instance (Representative wp, Representative wq) => Representative (PairWitness wp wq) where
    getRepWitness MkPairWitness = Dict

instance (Is wp p, Is wq q) => Is (PairWitness wp wq) '( p, q) where
    representative = MkPairWitness

type instance KindWitness (kp, kq) =
     PairWitness (KindWitness kp) (KindWitness kq)

type AnyInKind :: forall k. (k -> Type) -> Type
data AnyInKind wit =
    forall t. InKind t => MkAnyInKind (wit t)

instance AllWitnessConstraint Show w => Show (AnyInKind w) where
    show (MkAnyInKind wa) = showAllWitness wa

-- AllInKind
type AllInKind :: Type -> Constraint
class AllInKind k where
    withAllInKind ::
           forall kw r. (kw ~ KindWitness k)
        => ((forall (a :: k). Is kw a) => r)
        -> r

allInKind ::
       forall k (a :: k). AllInKind k
    => Dict (InKind a)
allInKind = let
    helper ::
           forall kw. kw ~ KindWitness k
        => Dict (Is kw a)
    helper = withAllInKind @k Dict
    in helper

instance AllInKind Type where
    withAllInKind r = r

instance AllInKind Constraint where
    withAllInKind r = r

instance AllInKind kq => AllInKind (kp -> kq) where
    withAllInKind r = withAllInKind @kq r
