module CarteSpec where

import Test.Hspec
import Test.QuickCheck

import Carte

-- test generique
{-
genTrucFree :: Gen Bool
genTrucFree = do
    lim <- choose (1, 100)  -- la limite initiale
    return $ if 1 == lim
             then Porte Ouvert NS
             else Porte Fermee NS

trucSpec = do
  describe "Titre test" $ do
    it "ce que ça fait" $
      property prop_genTruc_inv

-- passe d'invariants à propriétés
prop_genTruc_inv :: Property
prop_genTruc_inv = forAll genTrucFree $ truc_inv
--                        Gen Truc      Bool
--  invariant : coord in bound
--  prop :      all coords in bound

-}




--allCoordsInBounds_inv :: Carte -> Bool

genCarte :: Gen Carte
genCarte = pure (read "XXX\nX X\nXXX\n")

carteSpec = do
  describe "Validité des cartes chargées" $ do
    it "vérifie que toutes les coordonnées de la carte correspondent à une case" $
      property prop_genCarte_inv

prop_genCarte_inv :: Property
prop_genCarte_inv = forAll genCarte $ allCoordsInBounds_inv
