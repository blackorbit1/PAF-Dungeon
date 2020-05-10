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
genEnv = pure (setEntity (entiteFromChar 'M' 0) (Coord 2 1) (setEntity (entiteFromChar 'J' 0) (Coord 1 1) (createEnvi (read   "XXXXX\n\
                        \XE  X\n\
                        \X   X\n\
                        \XS  X\n\
                        \XXXXX"  ) "")))


environnementAllCoordsPositive = do
  describe "Environnement ---------> allCoordsPositive" $ do
    it "verifie que toutes les coordonnées des entitees sont positives" $
      property prop_environnementAllCoordsPositive_inv

prop_environnementAllCoordsPositive_inv :: Property
prop_environnementAllCoordsPositive_inv = forAll genEnv $ allCoordsPositive_inv

-----------

environnementOneUncrossableMobPerCase = do
  describe "Environnement ---------> oneUncrossableMobPerCase" $ do
    it "verifie qu'il y a au maximum une entité non traversable sur une case" $
      property prop_environnementOneUncrossableMobPerCase_inv

prop_environnementOneUncrossableMobPerCase_inv :: Property
prop_environnementOneUncrossableMobPerCase_inv = forAll genEnv $ oneUncrossableMobPerCase_inv

-----------

environnementPositiveStats = do
  describe "Environnement ---------> positiveStats" $ do
    it "verifie que les stats (pv et niveau d'accréditation pour aller sur les cases) sont positives" $
      property prop_environnementPositiveStats_inv

prop_environnementPositiveStats_inv :: Property
prop_environnementPositiveStats_inv = forAll genEnv $ positiveStats_inv


environnementUniqueIds = do
  describe "Environnement ---------> uniqueIds" $ do
    it "verifie que les identifiant de toutes les entites de l'environnement sont unique" $
      property prop_environnementUniqueIds_inv

prop_environnementUniqueIds_inv :: Property
prop_environnementUniqueIds_inv = forAll genEnv $ uniqueIds_inv




