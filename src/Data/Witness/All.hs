module Data.Witness.All where

import Data.Functor.Identity
import Data.Type.Equality
import Data.Witness.Any

newtype AllF (w :: k -> *) (f :: k -> *) = MkAllF
    { getAllF :: forall (t :: k). w t -> f t
    }

newtype All (w :: * -> *) = MkAll
    { getAll :: forall t. w t -> t
    }

allFToAll :: AllF w Identity -> All w
allFToAll (MkAllF wtit) = MkAll $ \wt -> runIdentity $ wtit wt

allToAllF :: All w -> AllF w Identity
allToAllF (MkAll wtt) = MkAllF $ \wt -> Identity $ wtt wt

type family UnAll (aw :: *) :: * -> * where
    UnAll (All w) = w

splitWitnessList :: TestEquality w => [Any w] -> AllF w []
splitWitnessList [] = MkAllF $ \_ -> []
splitWitnessList ((MkAny wt t):rr) =
    MkAllF $ \wt' ->
        case testEquality wt wt' of
            Just Refl -> t : (getAllF (splitWitnessList rr) wt')
            Nothing -> getAllF (splitWitnessList rr) wt'
