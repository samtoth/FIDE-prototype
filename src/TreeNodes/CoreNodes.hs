{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module CoreNodes where

import CodeTree
import Data.Natural
import FIDECore

type Explicit = Bool

data FideCore desugar where
  Ty  ∷ FideCore desugar
  App ∷ (CodeTree desugar a, CodeTree desugar b) ⇒ a → b → FideCore desugar
  Lam ∷ (CodeTree desugar a) ⇒ String → a → FideCore desugar
  Var ∷ Nat → FideCore desugar
  Prod ∷ (CodeTree desugar a, CodeTree desugar b) ⇒ Explicit → String  → a → b → FideCore desugar
  


instance CodeTree AllFC (FideCore AllFC) where
  desugar Ty = ATy
  desugar (App a b) = AApp (desugar a) (desugar b)
  desugar (Lam _ a) = ALam (desugar a)
  desugar (Var n) = AVar n
  desugar (Prod e _ t1 t2) = AProd e (desugar t1) (desugar t2) 

  name Ty = "Ty"
  name (App a b) = "(" <> name a <> " ∘ " <> name b <> ")"
  name (Lam n a) = "λ" <> n <> ". " <>  name a
  name (Var n)   = "# " <> show n
  name (Prod e ident t n) = case e of
    True →  "Π" <> ident <> ": " <> name t <> ". " <> name n
    False → "∀" <> ident <> ": " <> name t <> ". " <> name n

  children Ty = []
  children (App a b) = [MkSome a, MkSome b]
  children (Lam n a) = [MkSome a]
  children (Var _) = []
  children (Prod _ _ rty xty) = [MkSome rty, MkSome xty]



instance Show (FideCore desugar) where
  show = name
