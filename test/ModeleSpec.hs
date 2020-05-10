module ModeleSpec where

import Test.Hspec
import Test.QuickCheck

import Carte
import Environnement
import Modele


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

genModele :: Gen Modele
genModele = do
    let carteTxt =  "XXXXX\n\
                    \XE  X\n\
                    \X   X\n\
                    \XS  X\n\
                    \XXXXX"

    let mobTxt = ""

    let carte = createCarte carteTxt
    let env = createEnvi carte mobTxt
    
    pure (initModele carte env)

modeleModeleSpec = do
  describe "Modele ---------> ALL MODELE INVARIANTS" $ do
    it "verifie que tous les invariants du modele sont respectés" $
      property prop_modeleModele_inv

prop_modeleModele_inv :: Property
prop_modeleModele_inv = forAll genModele $ prop_Modele_inv
