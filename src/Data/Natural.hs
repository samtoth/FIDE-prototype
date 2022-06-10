{-# LANGUAGE DeriveDataTypeable #-}
module Data.Natural where

import Data.Data
import Prettyprinter (Pretty(..))

data Nat = Zero | Suc Nat
  deriving (Typeable, Data)


instance Num Nat where
  a + Zero = a
  a + Suc s = Suc a + s

  Zero - a = Zero
  a - Zero = a
  Suc a - Suc b = a - b

  a * Zero = Zero
  Zero * b = Zero
  Suc a * b = (a * b) + b

  abs = id

  signum Zero = Zero
  signum (Suc a) = Suc Zero

  fromInteger int = if (int <= 0) then Zero else Suc (fromInteger (int - 1))  
 
instance Enum Nat where
  fromEnum Zero = 0
  fromEnum (Suc n) = 1 + (fromEnum n)

  toEnum = fromInteger . (toEnum.fromEnum ∷ Int → Integer)

instance Eq Nat where
  Zero == Zero = True
  Suc a == Suc b = a == b
  n == m = False

instance Ord Nat where
  Zero <= n = True
  Suc n <= Zero = False
  Suc n <= Suc m = n <= m

instance Show Nat where
  show = show . fromEnum

instance Pretty Nat where
  pretty = pretty . show

inf ∷ Nat
inf = Suc inf

take ∷ Nat → [a] → [a]
take (Suc n) (x:xs) = x : Data.Natural.take n xs
take (Suc n) [] = []
take Zero (x:xs) = []
