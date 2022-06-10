{-# LANGUAGE TypeApplications, GeneralizedNewtypeDeriving, LambdaCase #-}

module FideCoreSpec where

import SpecHelper
import Test.QuickCheck
import Test.Hspec.QuickCheck
import FIDECore
import Data.Natural
import CodeTree()

import Control.Monad

newtype TestNat = TN {unTN :: Nat}
  deriving (Show, Eq, Num)

instance Arbitrary TestNat where
  arbitrary = fromInteger <$> arbitrary @Integer

newtype FinNat = FN {unFN ∷ (Nat, Nat)}
  deriving (Show, Eq)

instance Arbitrary FinNat where
  arbitrary = do
    n ← Suc . unTN <$> arbitrary @TestNat 
    m ← sized $ \size → pure $ finNat n size
    return $ FN(n, m)
      where
        finNat 0 = const Zero
        finNat n = \case
          0 → Zero
          m → Suc (finNat (n - 1) (m - 1)) 
        

aNat ∷ Gen (Nat)
aNat = unTN <$> arbitrary

newtype TestAllFC = TFC AllFC
  deriving (Show, Eq)

instance Arbitrary TestAllFC where
  arbitrary = sized $ \x -> TFC <$> afc' x
    where
      afc' :: Int → Gen (AllFC)
      afc' 0 = oneof [pure ASet, liftM ATy aNat, liftM AVar aNat, pure ATop, pure ABeta]
      afc' n | n>0 = 
	oneof [pure ASet, liftM ATy aNat, liftM AVar aNat, pure ATop, pure ABeta,
	       liftM2 AApp halfTree halfTree,
               liftM ALam $ afc' (n - 1),
               liftM AeLam $ afc' (n -1),
               liftM2 AProd halfTree halfTree,
               liftM2 AeProd halfTree halfTree,
               liftM2 ASum halfTree halfTree,
               liftM2 AIota halfTree halfTree,
               liftM AProj1 $ afc' (n - 1),
               liftM AProj2 $ afc' (n - 1),
               liftM2 AEq halfTree halfTree,
               liftM ASigma $ afc' (n - 1),
               liftM2 ARho halfTree halfTree
               ]
  	where halfTree = afc' (n `div` 2)


  
fcNatIdent :: Spec
fcNatIdent =  prop "toNat is inverse of fromNat" $
    \(TN x) -> (tryToNat . natToFC $ x) `shouldBe` Just x 

fcBetaReduce :: Spec
fcBetaReduce = prop "reduce (λa.a) ∘ b ≡ b" $
    \(TFC n) -> (normaliseUnsafe' $ AApp (ALam $ AVar 0) n) `shouldBe` normaliseUnsafe' n

plus
  = ALam $ ALam $ ALam $ ALam $
    AApp (AApp m f) (AApp (AApp n f) x)
       where m = AVar 3
             n = AVar 2
             f = AVar 1
             x = AVar 0

plus' m n = AApp (AApp plus m) n

fcPlus :: Spec
fcPlus = prop "plus ∘ (fromNat a) ∘ (fromNat b) ≡ fromNat (a + b) " $
         \(TN a, TN b) -> (normaliseUnsafe' (plus' (natToFC a) (natToFC b))) `shouldBe` (normaliseUnsafe' $ natToFC (a + b))

fcPlusCommute :: Spec
fcPlusCommute = prop "plus commutes" $
                \(TN a, TN b) -> normaliseUnsafe' (plus' (natToFC $ a) (natToFC $ b)) `shouldBe` normaliseUnsafe' (plus' (natToFC $ b) (natToFC $ a))

fcNestedLams :: Spec
fcNestedLams = prop "reduction correctly renames nested λ's and vars referencing them" $
  \(TN size, TN size2) ->
    normaliseUnsafe' (AApp (ALam $ nestedLam size2 (AVar (Suc size2))) (nestedLam size (AVar 0)))
               `shouldBe` Just (nestedLam size2 (nestedLam size $ AVar 0))
  where
    nestedLam Zero t = ALam $ t
    nestedLam (Suc n) t = ALam $ nestedLam n t

natTranslation ∷ Spec
natTranslation = describe "can translate from Numbers to Core" $ do
  fcNatIdent


-- Normalise with an arbitrary large amount of gas
normaliseUnsafe' = normaliseUnsafe 10000
-- N.B. normaliseUnsafe is acceptable here - all of the properties are guaranteed to be termanating. I have used the unsafe varient to generate a wider variety of property tests to erase the need of generating arbritary welltyped terms. 
normalisation ∷ Spec
normalisation = describe "normalise" $ do
  fcBetaReduce
  fcNestedLams
  context "with the church definition of plus" $ do
    fcPlus
    fcPlusCommute

typeChecking ∷ Spec
typeChecking = describe "Type Check" $ do
  describe "Correctly types Sorts" $ do
    it "(Set : Type 0)" $
        typeCheck ASet (ATy 0) [] `shouldBe` Right ()
    prop "(Type n : Type (Suc n))" $
       \(TN n) → typeCheck (ATy n) (ATy (Suc n)) [] `shouldBe` Right ()

  describe "types Function Application" $
      it "((λn. n) id) : idT" $
        let x   = ALam $ ALam $ AVar 0
            xty = AProd ASet $ AProd (AVar 0) (AVar 1) 
        in typeCheck (AApp (ALam $ AVar 0) x) xty [] `shouldBe` Right ()

  describe "types Product types" $ do
    it "types ΛA.λx.x : (ΠA:*. Π_:A. A)" $
      typeCheck (ALam $ ALam $ AVar 0) (AProd ASet $ AProd (AVar 0) $ AVar 1) [] `shouldBe` Right ()
    context "∀ (n:Nat) (m:Fin n)." $ 
      prop "(λ.λ... n ... λ.AVar 0) : (ΠSet.ΠSet ... `n` ... Π(AVar m). (AVar m)" $
        \(FN (n, m)) → let term = \case
                             Suc n' → ALam $ term n'
                             Zero   → ALam $ AVar 0
                           ty  = \case
                             (Suc n') → AProd ASet $ ty n' 
                             Zero     → AProd (AVar m) $ AVar (Suc m)
                   in typeCheck (term n) (ty n) [] `shouldBe` Right ()
    describe "types βη-equal terms" $ do
      prop "trivially: ∀(a:U). β : {a ≡ a}" $ 
        \(TFC term) → typeCheck ABeta (AEq term term) [] `shouldBe` Right ()
      it "inhabits η reducable equalities" $
        let plus1 = AApp plus (natToFC 1)
        in typeCheck ABeta (AEq (ALam $ AApp plus1 (AVar 0)) (plus1)) [] `shouldBe` Right () 
           
spec :: Spec
spec = describe "AllFC" $ do
  natTranslation
  normalisation
  typeChecking
  
    
main :: IO ()
main = hspec  spec
         






{-
data WellTyped = MkWT {wt_term ∷ AllFC, wt_ty ∷ AllFC}
  deriving (Show)
instance Arbitrary WellTyped where
  arbitrary = let aTy = do
                     anat ← aNat
                     return (MkWT (ATy anat) $ ATy (Suc anat))
               in oneof [pure $ (MkWT ASet (ATy 0)), aTy]

newtype ASort = MkAS { srt ∷ AllFC }
instance Arbitrary ASort where
  arbitrary = MkAS <$> oneof [pure ASet, liftM ATy aNat]

newtype AType = MkAT { ty ∷ AllFC }
instance Arbitrary AType where
  arbitrary = sized $ \size → MkAT <$> ((generateTermof size []) =<<  (srt <$> arbitrary @ASort))

type Context = [AllFC]


generateTermof ∷ Int → Context → AllFC → Gen AllFC
generateTermof 0 ctx t
  = case t of
      ASet → oneof (liftM3 AProd arbitrary (pure ASet) (pure $ AVar 0) : (pure <$> (vars ++ subtypes t)))
    where
      vars = map (\(n, _) → AVar n )
           . filter (\(_, b) → b == t)
           . zip (iterate Suc Zero)
           $ ctx
      subtypes (ATy n ) = ASet : (fmap ATy . take (fromEnum n) $ iterate Suc Zero)
      subtypes _ = []
generateTermof n ctx t
  = let halfTerm c t' = generateTermof (n `div` 2) c t'
    in case t of
         ASet → let prod = do
                         e ← arbitrary @Bool
                         xty ← halfTerm ctx (ATy Zero)  -- (Ty 0 to allow Set to be a possibly bound term)
                         rty ← halfTerm (xty : ctx) ASet
                         return $ AProd e xty rty
                    app = do
                         ty ← halfTerm ctx ASet 
                         e ← arbitrary @Bool
                         bod ← halfTerm ctx (AProd e ty t) 
                         arg ← halfTerm ctx ty 
                         return $ AApp bod arg
                        
                    in
                      oneof $ prod : app : vars
  where
    vars = fmap (\(n, _) → pure $ AVar n )
         . filter (\(_, b) → b == t)
         . zip (iterate Suc Zero)
         $ ctx
    subtypes (ATy n ) = ASet : (fmap ATy . take (fromEnum n) $ iterate Suc Zero)
    subtypes _ = []
  -}  
