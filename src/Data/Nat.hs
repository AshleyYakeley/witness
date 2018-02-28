module Data.Nat where

data Nat
    = Zero
    | Succ Nat

addNat :: Nat -> Nat -> Nat
addNat Zero b = b
addNat (Succ a) b = Succ $ addNat a b

-- | subtractFromNat a b = b - a
subtractFromNat :: Nat -> Nat -> Maybe Nat
subtractFromNat Zero b = Just b
subtractFromNat (Succ a) (Succ b) = subtractFromNat a b
subtractFromNat (Succ _) Zero = Nothing

multiplyNat :: Nat -> Nat -> Nat
multiplyNat Zero _ = Zero
multiplyNat (Succ a) b = addNat (multiplyNat a b) b
