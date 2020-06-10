module MoteurSpec where

import Test.Hspec
import Test.QuickCheck

import Carte
import Environnement
import Modele
import Moteur



genMoteur :: Gen Etat
genMoteur = do
    let carteTxt =      "XXXXXXXXXXXXXXX\n\
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
                        \XXXXXXXXXXXXXXX"


    let mobTxt =  "               \n\
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
                  \ M           M "

    let carte = createCarte carteTxt
    let env = createEnvi carte mobTxt

    pure (Tour 0 (initModele carte env) "")

moteurMoteurSpec = do
  describe "Moteur ---------> ALL MOTEUR INVARIANTS" $ do
    it "verifie que tous les invariants du moteur sont respectés" $
      property prop_moteurMoteur_inv

prop_moteurMoteur_inv :: Property
prop_moteurMoteur_inv = forAll genMoteur $ prop_Etat_inv
