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
                        \XE  X\n\
                        \X   X\n\
                        \XS  X\n\
                        \XXXXX"  )

genCoord :: Gen Coord
genCoord = do
  x <- choose (0, 50)
  y <- choose (0, 50)
  pure (Coord x y)


----------

carteAllCoordsInBoundsSpec = do
  describe "Carte ---------> allCoordsInBounds_inv : " $ do
    it "verifie que toutes les coordonnees de la carte correspondent a une case" $
      property prop_carteCoordInBounds_inv

prop_carteCoordInBounds_inv :: Property
prop_carteCoordInBounds_inv = forAll genCarte $ prop_allCoordsInBounds_inv

----------

carteAllCaseExistsSpec = do
  describe "Carte ---------> allCoordInCarte_inv : " $ do
    it "verifie que toutes les coordonnees entre la hauteur et la largeur donnent vers une case dans la carte" $
      property prop_AllCoordInCarteCarte_inv

prop_AllCoordInCarteCarte_inv :: Property
prop_AllCoordInCarteCarte_inv = forAll genCarte $ prop_allCoordInCarte_inv

----------

carteEntranceExitSpec = do
  describe "Carte ---------> entranceExit_inv : " $ do
    it "verifie que toutes la carte contient une seule entree et une seule sortie" $
      property prop_AllCoordInCarteCarte_inv

prop_entranceExitCarte_inv :: Property
prop_entranceExitCarte_inv = forAll genCarte $ prop_entranceExit_inv

----------

carteSurroundedByWallsSpec = do
  describe "Carte ---------> surroundedByWalls_inv : " $ do
    it "verifie que les bordures de la carte sont des murs" $
      property prop_surroundedByWallsCarte_inv

prop_surroundedByWallsCarte_inv :: Property
prop_surroundedByWallsCarte_inv = forAll genCarte $ prop_surroundedByWalls_inv

----------

carteDoorsSurroundedByWallsSpec = do
  describe "Carte ---------> doorsSurroundedByWalls : " $ do
    it "verifie que les portes sont encadrées par des murs" $
      property prop_doorsSurroundedByWallsCarte_inv

prop_doorsSurroundedByWallsCarte_inv :: Property
prop_doorsSurroundedByWallsCarte_inv = forAll genCarte $ prop_doorsFramedByWalls_inv

----------

carteCoordSpec = do
  describe "Carte ---------> Coord" $ do
    it "verifie que les coordonnées sont valides" $
      property prop_CoordCarte_inv

prop_CoordCarte_inv :: Property
prop_CoordCarte_inv = forAll genCoord $ prop_Coord_inv

----------

carteCarteSpec = do
  describe "Carte ---------> ALL CARTE INVARIANTS" $ do
    it "verifie que les cartes sont valides" $
      property prop_carteCarte_inv

prop_carteCarte_inv :: Property
prop_carteCarte_inv = forAll genCarte $ prop_Carte_inv

