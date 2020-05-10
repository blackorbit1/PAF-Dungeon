module EnvSpec where

import Test.Hspec
import Test.QuickCheck

import Carte
import Environnement


genCarte :: Gen Carte
genCarte = pure (read   "XXXXX\n\
                        \XE  X\n\
                        \X   X\n\
                        \XS  X\n\
                        \XXXXX"  )


genEnv :: Gen Envi
genEnv = pure (setEntity (entiteFromChar 'M' 1) (Coord 2 1) (setEntity (entiteFromChar 'J' 0) (Coord 1 1) (createEnvi (read   "XXXXX\n\
                        \XE  X\n\
                        \X   X\n\
                        \XS  X\n\
                        \XXXXX"  ) "")))


environnementAllCoordsPositiveSpec = do
  describe "Environnement ---------> allCoordsPositive" $ do
    it "verifie que toutes les coordonnées des entitees sont positives" $
      property prop_environnementAllCoordsPositive_inv

prop_environnementAllCoordsPositive_inv :: Property
prop_environnementAllCoordsPositive_inv = forAll genEnv $ prop_allCoordsPositive_inv

-----------

environnementOneUncrossableMobPerCaseSpec = do
  describe "Environnement ---------> oneUncrossableMobPerCase" $ do
    it "verifie qu'il y a au maximum une entité non traversable sur une case" $
      property prop_environnementOneUncrossableMobPerCase_inv

prop_environnementOneUncrossableMobPerCase_inv :: Property
prop_environnementOneUncrossableMobPerCase_inv = forAll genEnv $ prop_oneUncrossableMobPerCase_inv

-----------

environnementPositiveStatsSpec = do
  describe "Environnement ---------> positiveStats" $ do
    it "verifie que les stats (pv et niveau d'accréditation pour aller sur les cases) sont positives" $
      property prop_environnementPositiveStats_inv

prop_environnementPositiveStats_inv :: Property
prop_environnementPositiveStats_inv = forAll genEnv $ prop_positiveStats_inv

-----------

environnementUniqueIdsSpec = do
  describe "Environnement ---------> uniqueIds" $ do
    it "verifie que les identifiant de toutes les entites de l'environnement sont unique" $
      property prop_environnementUniqueIds_inv

prop_environnementUniqueIds_inv :: Property
prop_environnementUniqueIds_inv = forAll genEnv $ prop_uniqueIds_inv

----------

environnementEnviSpec = do
  describe "Environnement ---------> ALL ENVI INVARIANTS" $ do
    it "verifie que les environnements sont valides" $
      property prop_environnementEnvi_inv

prop_environnementEnvi_inv :: Property
prop_environnementEnvi_inv = forAll genEnv $ prop_Envi_inv

----------

{-
environnementEntiteSpec = do
  describe "Environnement ---------> entite" $ do
    it "verifie que toutes les entitees sont valides" $
      property prop_environnementEntite_inv

prop_environnementEntite_inv :: Property
prop_environnementEntite_inv = forAll genEnv $ prop_Entite_inv
-}





