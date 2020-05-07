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
genCarte = pure (read   "XXXXX\n\
                        \X S X\n\
                        \X   X\n\
                        \XE  X\n\
                        \XXXXX"  )



carteCoordInBoundsSpec = do
  describe "carteCoordInBounds : " $ do
    it "verifie que toutes les coordonnees de la carte correspondent a une case" $
      property prop_carteCoordInBounds_inv

prop_carteCoordInBounds_inv :: Property
prop_carteCoordInBounds_inv = forAll genCarte $ allCoordsInBounds_inv




carteAllCaseExistsSpec = do
  describe "AllCoordInCarteCarte : " $ do
    it "verifie que toutes les coordonnees entre la hauteur et la largeur donnent vers une case dans la carte" $
      property prop_AllCoordInCarteCarte_inv

prop_AllCoordInCarteCarte_inv :: Property
prop_AllCoordInCarteCarte_inv = forAll genCarte $ allCoordInCarte_inv



carteEntranceExitSpec = do
  describe "entranceExit : " $ do
    it "verifie que toutes la carte contient une seule entree et une seule sortie" $
      property prop_AllCoordInCarteCarte_inv

prop_entranceExitCarte_inv :: Property
prop_entranceExitCarte_inv = forAll genCarte $ entranceExit_inv



carteSurroundedByWallsSpec = do
  describe "surroundedByWalls : " $ do
    it "verifie que les bordures de la carte sont des murs" $
      property prop_surroundedByWallsCarte_inv

prop_surroundedByWallsCarte_inv :: Property
prop_surroundedByWallsCarte_inv = forAll genCarte $ surroundedByWalls_inv



