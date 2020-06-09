module MoteurSpec where

import Test.Hspec
import Test.QuickCheck

import Carte
import Environnement
import Modele
import Moteur


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

genMoteur :: Gen Etat
genMoteur = do
    let carteTxt =  "XXXXX\n\
                    \XE  X\n\
                    \X   X\n\
                    \XS  X\n\
                    \XXXXX"

    let mobTxt = ""

    let carte = createCarte carteTxt
    let env = createEnvi carte mobTxt

    pure (Tour 0 (initModele carte env) "")

moteurMoteurSpec = do
  describe "Modele ---------> ALL MOTEUR INVARIANTS" $ do
    it "verifie que tous les invariants du moteur sont respectés" $
      property prop_moteurMoteur_inv

prop_moteurMoteur_inv :: Property
prop_moteurMoteur_inv = forAll genMoteur $ prop_Etat_inv
