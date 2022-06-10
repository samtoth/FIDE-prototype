{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fdefer-typed-holes #-}

module FIDECore (
  AllFC(..), WellTyped(),
  mkWellTyped,
  tryToNat,
  natToFC, normalise,
  normaliseUnsafe,
  typeCheck,
  reductions,
  format,
  formatL
  ) where

import CodeTree
import Data.Natural
import Data.Bifunctor (first)
import Data.Monoid (All(..), Any(..))
-- import Control.Monad (liftM2)

import Data.Text.Lazy (unpack)

import Data.Function (on)
import Data.Data

import Prettyprinter
import Prettyprinter.Render.Terminal

-- import Data.Fix (Fix(Fix))

import Optics hiding (both)
-- import Optics.TH

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable

import Data.Maybe

(≡) ∷ Eq a ⇒ a → a → Bool
(≡) = (==)
(∘) ∷ (b → c) → (a → b) → a → c 
(∘) = (.)
(…) ∷ (c → d) → (a → b → c) → a → b → d
(…) = (.) ∘ (.)
(∈) ∷ Eq a ⇒ a → [a] → Bool
(∈) = elem
both ∷ (a → b) → (a, a) → (b, b)
both f (a, b) = (f a , f b)
infixr 9 …
infixr 8 ∘ 

data AllFC where
  -- standard types and terms of CoC
  ATy :: Nat -> AllFC
  ASet ∷ AllFC
  AApp ∷ AllFC → AllFC → AllFC
  AVar ∷ Nat → AllFC
  ALam ∷ AllFC → AllFC
  AProd ∷ AllFC → AllFC → AllFC

  -- eliminated versions of λ (Λ) and Π (∀)
  AeLam ∷ AllFC → AllFC
  AeProd ∷ AllFC → AllFC → AllFC

  -- ι type and respective proof terms
  AIota ∷ AllFC → AllFC → AllFC
  ASum ∷ AllFC → AllFC → AllFC
  AProj1 ∷ AllFC → AllFC
  AProj2 ∷ AllFC → AllFC

  -- β η equality type and respective proof terms
  AEq ∷ AllFC → AllFC → AllFC
  ABeta ∷ AllFC
  ASigma ∷ AllFC → AllFC
  ARho ∷ AllFC → AllFC → AllFC

  ATop ∷ AllFC

  

makeBaseFunctor ''AllFC
makeLenses ''AllFCF
makePrisms ''AllFCF

deriving instance Typeable AllFC
deriving instance Typeable a ⇒ Typeable (AllFCF a)
deriving instance Data AllFC
deriving instance Data a ⇒ Data (AllFCF a)


-- tmap = embed ∘ over traversed (MkSome) ∘ project
one = AeProd ASet $ AProd (AVar 0) (AVar 1)


colorDepth ∷ Nat → Color
colorDepth = ((cycle [Red, Blue, Green, Yellow, Magenta, Cyan]) !!) ∘ fromEnum


format ∷ AllFC → Doc AnsiStyle
format = cataWithDepth $ \depth → \case
  (ALamF bod) → annotate (color $ colorDepth depth) "λ." <+> annotate ((color $ colorDepth depth) <> underlined) bod
  (AAppF a b) → parens $ a <+> "∘" <+> b
  (AeLamF bod) → annotate (color $ colorDepth depth) "Λ." <+> bod
  (AProdF a b) → annotate (color $ colorDepth depth) "Π(_ :" <+> a <> ")." <+> b
  (AVarF n) → annotate (color $ colorDepth (depth - n - 1)) ("#" <+> pretty n)
  x → (pretty . nameF $ x) <+> hsep (toListOf folded $ x) 

  
formatL ∷ [AllFC] → Doc AnsiStyle
formatL = group . encloseSep (flatAlt "[ " "[")
                          (flatAlt " ]" "]")
                          ", " . fmap format

nameF ∷ AllFCF a → String
nameF ASetF = "Set"
nameF (ATyF n) = "Ty " <> show n
nameF (AAppF _ _) = "App"
nameF (ALamF _) = "λ."
nameF (AProdF _ _) = "Π"
nameF (AVarF n) = "# " <> show n

nameF (AeLamF _) = "Λ."
nameF (AeProdF _ _) = "∀"

nameF (AIotaF _ _) = "ι"
nameF (ASumF _ _) = "Sum" -- "[" <> name a <> ", " <> name b <> "]"
nameF (AProj1F _) = "Proj1" -- "[" <> name a <> "]¹"
nameF (AProj2F _) = "Proj2" -- "[" <> name a <> "]²"

nameF (AEqF _ _) = "~" --name a <> " ~ " <> name b
nameF ABetaF = "β"
nameF (ASigmaF _) = "σ" --"σ[" <> name a <> "]"
nameF (ARhoF _ _) = "ρ" --"ρ[" <> name a <> " / " <> name b <> "]"

nameF ATopF = "U"



instance CodeTree AllFC AllFC where
  desugar = id

  name = nameF . project

  children = fmap MkSome . toListOf traversed . project

instance (Data a, Recursive a, Base a ~ AllFCF) ⇒ Eq (AllFCF a) where
  x == y = getAll (
      (All $ ((≡) `on` toConstr) x y)
        <>
      (foldMap (f) ∘ uncurry zip ∘ both (toListOf folded) $ (x, y))
    )
    where
      f ∷ (a, a) → All
      f (a', b') = let (a, b) ∷ (AllFCF a, AllFCF a) = (project a', project b')
                   in All $ case (a, b) of
                            (ASetF, ASetF) → True
                            (ATopF, ATopF) → True
                            (ABetaF, ABetaF) → True
                            (ATyF x, ATyF y)  → x ≡ y
                            (AVarF x, AVarF y) → x ≡ y
                            _ → a ≡ b

instance Eq AllFC where
  (==) = (≡) `on` project
          
{-
class HasFC a where 
  _fc ∷ a → AllFC
  _setFc ∷ a → SomeFC → a


data SomeFC = ∀fc. HasFC fc ⇒ MkSomeFC fc

instance Show SomeFC where
  show (MkSomeFC fc) = show ∘ _fc $ fc

instance HasFC SomeFC where
  _fc (MkSomeFC a) = _fc a
  _setFc (MkSomeFC a) = MkSomeFC ∘ _setFc a

type instance Base SomeFC = AllFCF 

instance Corecursive SomeFC where
  embed = MkSomeFC

instance Recursive SomeFC where
  project (MkSomeFC a) = let fc = project $ _fc a in fmap MkSomeFC fc

instance HasFC AllFC where
  _fc = id
  _setFc = _fc … const id  

instance HasFC (AllFCF (SomeFC)) where
  _fc fcsome = case embed fcsome of
                 MkSomeFC a → _fc a 
  _setFc _ a = project a
  
data WithAnn prop = AnnFC { _inner ∷ SomeFC, _prop ∷ prop }

deriving instance Show prop ⇒ Show (WithAnn prop)

fc ∷ HasFC fc ⇒ Lens fc fc AllFC SomeFC
fc = lens _fc _setFc

instance HasFC (WithAnn prop) where
  _fc AnnFC { _inner, _prop } = _fc  _inner
  _setFc s x = s {_inner = _setFc (_inner s) x}

type WithDepth = WithAnn Nat

annotateDepth ∷ HasFC f ⇒ f → WithDepth
annotateDepth = go 0 ∘ _fc
  where
    go ∷ Nat → AllFC → WithDepth
    go d fc = let transformed = embed $ case project fc of
                    ALamF bod → ALamF ∘ MkSomeFC $ go (Suc d) bod
                    AProdF a b → AProdF (MkSomeFC $ go d a) (MkSomeFC $ go (Suc d) b)
                    AeLamF bod → AeLamF ∘ MkSomeFC $ go (Suc d) bod
                    AeProdF a b → AeProdF (MkSomeFC $ go d a) (MkSomeFC $ go (Suc d) b)
                    AIotaF a b → AIotaF (MkSomeFC $ go d a) (MkSomeFC $ go (Suc d) b)
                    x → fmap (MkSomeFC ∘ go d) x
                    
              in AnnFC { _inner = MkSomeFC transformed, _prop = d }
-}

anaWithDepth ∷ (Corecursive a, Base a ~ AllFCF) ⇒ (Nat → a → AllFCF a) → a → AllFCF a
anaWithDepth f = go 0
  where go d fc = let new = f d fc
                  in case new of
                       ALamF bod → ALamF ∘ embed $ go (Suc d) bod
                       AProdF a b → AProdF (embed $ go d a) (embed $ go (Suc d) b) 
                       AeLamF bod → AeLamF ∘ embed $ go (Suc d) bod
                       AeProdF a b → AeProdF (embed $ go d a) (embed $ go (Suc d) b)
                       AIotaF a b → AIotaF (embed $ go d a) (embed $ go (Suc d) b)
                       x → fmap (embed ∘ go d) x

cataWithDepth ∷ (Nat → Base AllFC a → a) → AllFC → a
cataWithDepth f = go 0 ∘ project
  where
    go d = \case
      ALamF a → f d ∘ ALamF ∘  go (Suc d) ∘ project $ a
      AProdF a b → (f d … AProdF) (go d ∘ project $ a) (go (Suc d) ∘ project $ b)
      AeLamF a → f d ∘ AeLamF ∘ go (Suc d) ∘ project $ a
      AeProdF a b → (f d … AeProdF) (go d ∘ project $ a) (go (Suc d) ∘ project $ b)
      AIotaF a b → (f d … AIotaF) (go d ∘ project $ a) (go (Suc d) ∘ project $ b)
      x → f d $ fmap (go d . project) x
      
  

foldMapWithDepth ∷ (Monoid m) ⇒ (Nat → AllFC → m) → AllFC → m
foldMapWithDepth f = go 0
  where
    --go ∷ Nat → AllFC → m
    go d fc = f d fc <> case project fc of
                          ALamF bod → go (Suc d) bod
                          AProdF a b →  go d a <> go (Suc d) b 
                          AeLamF bod → go (Suc d) bod
                          AeProdF a b → go d a <> go (Suc d) b
                          AIotaF a b → go d a <> go (Suc d) b
                          x → foldMap (go d) x
  
substitute ∷ AllFC -- ^ subject
           → Nat   -- ^ index
           → AllFC -- ^ Replacee
           → AllFC
substitute subject idx replace = embed $ anaWithDepth go subject
  where
    go depth = \case
      AVar n → if (n ≡ (idx + depth)) then project (shift (toEnum . fromEnum $ depth) 0 replace) else AVarF n
      x → project x
      

{- shift code assisted by https://github.com/Gabriella439/Haskell-Morte-Library/issues/1 --}
shift ∷ Integer -- ^ shift amount
      → Nat -- ^ cutoff
      → AllFC -- ^ Term
      → AllFC
shift s c  = genericShift s (\n depth →  n >= depth + c )

genericShift ∷ Integer
             → (Nat → Nat → Bool)
             → AllFC
             → AllFC
genericShift shift f = embed ∘ anaWithDepth go
  where
    go depth = \case
      AVar n → project $ if f n depth
        then AVar ∘ toEnum $ (fromEnum n) + (fromEnum shift)
        else AVar n
      x → project x


isNormal ∷ AllFC → Bool
isNormal x = getAll $ f x <> go x  
  where
    go = foldMapOf folded f . project
    f = \case
              (AApp (ALam _) _) → All False
              (AApp (AeLam _) _) → All False
              (ALam (AApp x (AVar 0))) → All ∘ not $ isFree 0 x
              x → go x


reduceOnce ∷ AllFC → AllFC
reduceOnce (AApp (ALam bod) ag)
  = shift (-1) 0 $ substitute bod 0 (shift 1 0 ag)
reduceOnce (AApp fn ag)
  | isNormal fn = AApp fn (reduceOnce ag)
  | otherwise = AApp (reduceOnce fn) ag
reduceOnce t@(ALam (AApp x (AVar 0)))
  | isFree 0 x = x
  | otherwise = t
reduceOnce (ALam bod)
  = ALam $ reduceOnce bod
reduceOnce (AProd xty rty)
  | isNormal xty = AProd xty $ reduceOnce rty
  | otherwise = AProd (reduceOnce xty) rty
reduceOnce t = t

reductions ∷ AllFC → [AllFC]
reductions = iterate reduceOnce
             
normaliseUnsafe ∷ Nat → AllFC  → Maybe AllFC
normaliseUnsafe gas = listToMaybe . dropWhile (not . isNormal) . Data.Natural.take gas . reductions
{- generate stream of iterated reductions, and then take the first such reduction that is in normal form -}


instance Show AllFC where
  show = unpack ∘ renderLazy ∘ layoutPretty defaultLayoutOptions ∘ format

natToFC ∷ Nat → AllFC
natToFC n = ALam  $ ALam $ go n
  where
    go Zero = AVar 0
    go (Suc n) = AApp (AVar 1) $ go n

tryToNat ∷ AllFC → Maybe Nat 
tryToNat t = do
  let (ALam (ALam b)) = t
  go 0 b
  where
    go ∷ Nat → AllFC → Maybe Nat
    go i tt = do
      case tt of
        AVar 0 → Just i
        AApp a b → do
          let (AVar 1) = a
          go (Suc i) b
        _ → Nothing


type Context = [AllFC]

data TypeCheckFailure where
  Unspecified :: TypeCheckFailure
  ExpectedTypeNotReceived ∷ AllFC -- ^ expected
                          → AllFC -- ^ inferred
                          → TypeCheckFailure
  UnInferrable :: TypeCheckFailure
  VarOutOfBounds :: TypeCheckFailure
  ExpectedSort :: TypeCheckFailure
  NotBetaEtaEq ∷ AllFC → AllFC → TypeCheckFailure
  ElimsDiffer ∷ AllFC → AllFC → TypeCheckFailure
  ElimError ∷ ElimError → TypeCheckFailure
  ExpectedIotaType ∷ TypeCheckFailure
  deriving (Eq, Show)

typeCheck ∷ AllFC -- ^ Scrutinee
          → AllFC -- ^ Expected type
          → Context
          → Either TypeCheckFailure ()
typeCheck _ ATop _ = Right ()
typeCheck scrut' expect' ctx = do
  let expect = reduceOnce expect'
  let scrut = reduceOnce scrut'
  case scrut of
    ALam bod → case expect of
      AProd xty rty → typeCheck bod rty (xty ⊳ ctx) 
      _ → Left Unspecified -- Term should be of function type
    AeLam bod → case expect of
      AeProd xty rty → typeCheck bod rty (xty ⊳ ctx)
      _ → Left Unspecified -- Ditto
    ASum a b → case expect of
      AIota aty bty → do
        typeCheck a aty ctx
        typeCheck b bty (aty ⊳ ctx)
        let same = ((==) `on` (normaliseUnsafe 200 ∘ fromElimed . elim)) a b
        if same then
          Right ()
        else
          Left $ ElimsDiffer a b
      _ → Left $ ExpectedIotaType
    ABeta → case expect of
      AEq a b → do
          if ((≡) `on` (normaliseUnsafe 200 ∘ fromElimed ∘ elim)) a b then
              Right ()
          else
              Left $ NotBetaEtaEq a b
      _ → Left Unspecified -- Expected Equality type
    term → do
      infered ← typeInfer term ctx
      if infered == expect then
        Right ()
      else
        Left $ ExpectedTypeNotReceived expect infered

ctxLookup :: Context -> Nat -> Either TypeCheckFailure AllFC
ctxLookup (_ : ts) (Suc n) = ctxLookup ts n
ctxLookup (t:_) Zero = Right t
ctxLookup _ _ = Left VarOutOfBounds

(⊳) :: AllFC → Context → Context
t ⊳ ctx = shift 1 0 <$> (t : ctx)

typeInfer ∷ AllFC -- ^ Scrutinee
          → Context
          → Either TypeCheckFailure AllFC -- ^ Either information about the failure or the type that has been infered
typeInfer scrut ctx = case scrut of
  ATop → Right ATop -- Although this might be inconcistent - in practice I dont think it matters
  AVar n -> ctxLookup ctx n
  ASet -> Right . ATy $ 0
  ATy n -> Right . ATy . Suc $ n
  AApp fn arg -> do
    funct <- reduceOnce <$> typeInfer fn ctx -- Maybe should be normalise
    case funct of
      AProd xty rty -> do
        argt <- reduceOnce <$> typeInfer fn ctx -- Ditto
        if argt == xty then
          Right rty
        else
          Left Unspecified -- Argument did not match function application
      _ -> Left Unspecified -- Non function application
  AProd xty rty -> do
     xtyty <- typeInfer xty  ctx
     if isSort xtyty
     then do
       rtyty <- typeInfer rty (xtyty ⊳ ctx)
       if isSort rtyty then
          Right rtyty
       else
          Left ExpectedSort
     else
       Left ExpectedSort
  _ -> Left UnInferrable


isSort :: AllFC -> Bool
isSort ASet = True
isSort (ATy _) = True
isSort _ = False


newtype WellTyped = MkWT (AllFC, AllFC)
  deriving (Eq, Show)


mkWellTyped ∷ AllFC → AllFC → Either TypeCheckFailure WellTyped
mkWellTyped t ty = typeCheck t ty [] >> (Right . MkWT $ (t, ty))

-- Fairly sure that normalisation is type preserving in CoC and CDLE
-- seems like a pretty hefty assumption - I should probably confirm
-- It is safe to give infinite gas - becuase all well typed terms terminate
normalise ∷ WellTyped → WellTyped
normalise (MkWT (t, ty)) = MkWT . first fromJust $ (normaliseUnsafe inf t, ty)

newtype Elimed = MkElimed { fromElimed ∷ AllFC }
  deriving (Show, Eq)

data ElimError = E
  deriving (Eq, Show)

isFree ∷ Nat → AllFC → Bool
isFree n = not ∘ getAny ∘ foldMapWithDepth (\depth → \case
                                                 AVar var → Any (var ≡ (n + depth)) 
                                                 x → Any False
                                             )

elim ∷ AllFC → Elimed
elim = MkElimed . go 0 []
  where
        go depth vars = \case
          x@(AApp a (AVar n)) → if n ∈ vars then
                              a
                            else
                              x
          AeProd a b → do
            genericShift (-1) (\var d → (var > d)) $ go (Suc depth) ((0 + depth):vars) b
          AeLam bod → genericShift (-1) (\var d → (var > d)) $ go (Suc depth) ((0 + depth):vars) bod
          AIota a _ → go depth vars a
          ASum a _ → go depth vars a
          AProd a b → AProd (go depth vars a) (go (Suc depth) vars b)
          ALam bod → ALam (go (Suc depth) vars bod)
          x → embed $ (go depth vars) <$> project x


