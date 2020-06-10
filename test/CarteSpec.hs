module CarteSpec where

import Test.Hspec
import Test.QuickCheck

import Carte



genCarte :: Gen Carte
genCarte = pure (read   "XXXXXXXXXXXXXXX\n\
                        \XE   /        X\n\
                        \XX XXXXXX-X-X-X\n\
                        \X  X   XX X X X\n\
                        \X XX^XXXX X X X\n\
                        \X XX X    X X X\n\
                        \X      XXXX X X\n\
                        \XXXXXXXX    o X\n\
                        \X     X  X XX-X\n\
                        \XXXXX-XXXXXX  X\n\
                        \X     |  X    X\n\
                        \X-XXXXX     XXX\n\
                        \X  SXXX    XX X\n\
                        \X   o | XX    X\n\
                        \XXXXXXXXXXXXXXX")

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


