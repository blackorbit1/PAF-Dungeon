module Moteur where

import Carte (Carte, Coord)
import qualified Carte as C

import Environnement (Entite, Envi)
import qualified Environnement as E

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Data.Map.Strict as M

--import StdGen



data Etat =   Perdu 
            | Gagne
            | Tour {  num_tour :: Int, carte_tour :: C.Carte
                    , envi_tour :: E.Envi 
                    --, gen_tour :: StdGen 
                    , obj_tour :: (M.Map Int E.Entite), journal_tour :: String}

