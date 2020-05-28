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
            | Tour      { num_tour :: Int
                        , modele :: M.Modele
                        , journal_tour :: String }



etat_tour :: Etat -> Keyboard -> Etat
etat_tour state kbd = case state of
        Tour -> 
        
