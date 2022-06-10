{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module CodeTree where

class CodeTree desugared a | a → desugared where
  desugar :: a → desugared
  name ∷ a → String
  children ∷ a → [SomeCodeTree desugared]


data SomeCodeTree desugared = ∀ f. CodeTree desugared f ⇒ MkSome f

instance CodeTree desugared (SomeCodeTree desugared) where
  desugar (MkSome t) = desugar t
  name (MkSome t) = name t
  children (MkSome t) = children t


data NTree a = MkTree a [NTree a]

data GenericTree desugared = MkGT {
  gt_name ∷ String,
  gt_desugar ∷ desugared
  }

toGeneric ∷ CodeTree desugared c ⇒ c → NTree (GenericTree desugared)
toGeneric c = MkTree (MkGT (name c) (desugar c)) $ (fmap toGeneric . children) c


instance CodeTree desugared (NTree (GenericTree desugared)) where
  name (MkTree gt _)= gt_name gt
  desugar (MkTree gt _) = gt_desugar gt
  children (MkTree _ cs)  = MkSome <$>  cs
