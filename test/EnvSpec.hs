module EnvSpec where

import Test.Hspec
import Test.QuickCheck

import Carte
import Environnement


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


genEnv :: Gen Envi
genEnv = pure (createEnvi (read   "XXXXXXXXXXXXXXX\n\
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
                                  \XXXXXXXXXXXXXXX")  "               \n\
                                                      \ J             \n\
                                                      \               \n\
                                                      \     MC        \n\
                                                      \               \n\
                                                      \         M     \n\  
                                                      \             M \n\
                                                      \         M     \n\
                                                      \ M             \n\
                                                      \               \n\
                                                      \               \n\
                                                      \               \n\
                                                      \               \n\
                                                      \ M           M ")


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





