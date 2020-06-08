module Moteur where

import Carte (Carte, Coord)
import qualified Carte as C

import Environnement (Entite, Envi)
import qualified Environnement as E

import Keyboard (Keyboard)
import qualified Keyboard as K

import Modele (Modele)
import qualified Modele as M

import qualified Data.Map.Strict as Map

--import StdGen



data Etat =   Perdu 
            | Gagne
            | Tour {  num_tour :: Int
                , modele :: Modele.Modele
                , journal_tour :: String}


etat_tour :: RealFrac a => Etat -> Keyboard -> a -> Etat
etat_tour state kbd deltaT = case state of
    Tour _ _ _ -> let m = M.gameStep (modele state) kbd deltaT in
            if (E.getPlayerCoord (M.envi m)) == (C.getExitCoord (C.listFromCarte (M.carte m))) then Gagne
            else if (case (E.getPlayerEntity (M.envi m)) of
                    Just player -> ((E.pvie player) <= 0)
                    Nothing -> False
                    ) then Perdu
            else state {num_tour = (num_tour state) + 1 , modele = m , journal_tour = (M.logs m)}
    _ -> state

        
