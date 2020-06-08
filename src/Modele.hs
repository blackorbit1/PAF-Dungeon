
module Modele where

import SDL

import Carte (Carte, Coord)
import qualified Carte as C

import Environnement (Entite, Envi)
import qualified Environnement as E

import Keyboard (Keyboard)
import qualified Keyboard as K

import System.Random
import qualified System.Random as R

-- H : Aller en haut
-- B : Aller en bas
-- D : Aller a droite
-- G : Aller a gauche
-- U : Utiliser ( contextuel )
-- A : Attaquer
-- R : Ne rien faire
data Ordre = Haut | Bas | Droite | Gauche | Uti | Atk | Rien deriving Show


data Modele = Modele  { carte :: Carte        -- carte actuelle
                      , envi :: Envi          -- environnement actuel
                      , gene :: (Int,StdGen)        -- generateur aleatoire l'int correspond à la prochaine seed du generateur
                      , logs :: String         -- journal du tour
                      , keyboard :: Keyboard  -- l’etat du clavier 
                      --, win :: Bool           -- gagné !
                      }


instance Show Modele where
    show = toString

class ToString a where
    toString :: a -> String

instance ToString Modele where
    toString modele = (show (carte modele)) ++ "\n\n" ++ (show (envi modele))



initModele :: Carte -> Envi -> Modele
initModele carte envi = Modele carte envi (1, mkStdGen 0) "" K.createKeyboard



bouge :: Modele -> Entite -> Coord -> Modele
bouge modele entity coord = modele { envi = (E.bougeById (E.idn entity) coord (envi modele) (carte modele)) }

prop_bouge_pre :: Modele -> Entite -> Coord -> Bool
prop_bouge_pre modele entity coord = (E.prop_bougeById_pre (E.idn entity) coord (envi modele) (carte modele))

prop_bouge_post :: Modele -> Entite -> Coord -> Bool
prop_bouge_post modele entity coord = (E.prop_bougeById_post (E.idn entity) coord (envi modele) (carte modele))




prop_Modele_inv :: Modele -> Bool
prop_Modele_inv modele = C.prop_Carte_inv (carte modele)
                      && E.prop_Envi_inv (envi modele)



prevoir :: Entite -> [(Int,Ordre)]
prevoir entity = case entity of
  E.Monstre _ _ _ _ -> [(1, Haut ),(1, Bas ),(1, Droite ),(1, Gauche ), (2, Rien )] --(0, Uti ), (4, Atk ),
  otherwise -> [(1, Haut ),(1, Bas ),(1, Droite ),(1, Gauche ),(1, Uti ), (0, Atk ), (1, Rien )]

transformPonderatedList :: [(Int, Ordre)] -> [Ordre]
transformPonderatedList ((0,ordre):xs) = transformPonderatedList xs
transformPonderatedList ((coef,ordre):xs) = ordre:(transformPonderatedList ((coef-1,ordre):xs))
transformPonderatedList [] = []

entityCoord :: Entite -> Modele -> Maybe Coord
entityCoord e m = (\result -> case result of 
    Just (co, _) -> Just co
    Nothing -> Nothing ) 
  (E.trouveId (E.idn e) (envi  m))

decider :: [(Int, Ordre)] -> Modele -> Entite -> Modele
decider list m entity = 
  case (entityCoord entity m) of
    Just c -> (
      let (ordre, modele) = pickOrder (transformPonderatedList list) m in
      case ordre of
        Haut    -> bouge (modele { logs = (logs modele) ++ (show entity) ++ " " ++ (show ordre) ++ "\n"}) entity (C.Coord (C.cx c) ((C.cy c) - 1))
        Bas     -> bouge (modele { logs = (logs modele) ++ (show entity) ++ " " ++ (show ordre) ++ "\n"}) entity (C.Coord (C.cx c) ((C.cy c) + 1))
        Droite  -> bouge (modele { logs = (logs modele) ++ (show entity) ++ " " ++ (show ordre) ++ "\n"}) entity (C.Coord ((C.cx c) + 1) (C.cy c))
        Gauche  -> bouge (modele { logs = (logs modele) ++ (show entity) ++ " " ++ (show ordre) ++ "\n"}) entity (C.Coord ((C.cx c) - 1) (C.cy c))
        Atk -> attack modele entity c
        Uti -> modele
        Rien -> modele
        --_ -> modele
      )
    Nothing -> m -- Si l'entitee qu'on traite ne correspond à aucune entié dans l'environnement, on ne change rien


{-
use :: Modele -> Entite -> Modele
use modele entity = case entity of
  Monstre -> 
-}

mml :: Maybe [a] -> Maybe [a] -> Maybe [a] -- merge maybe list
mml a b = case (a,b) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just b) -> Just b
  (Just a, Nothing) -> Just a 
  (Just a, Just b) -> Just (a <> b)

attackAux :: (Int, Modele) -> Entite -> (Int, Modele)
attackAux (damage, modele) entity@(E.Monstre idnn pviee _ _) = case (entityCoord entity modele) of 
  Just coord -> (damage, modele { envi = (E.setEntity (entity { E.pvie = pviee - damage }) coord (E.rmEntById idnn (envi modele))) } )
  Nothing -> (damage, modele)                      -- dans le cas où l'entitee ne correspond à aucunes coordonnées dans l'environnement
attackAux (damage, modele) _ = (damage, modele) -- dans le cas où c'est une entitee qui ne peut pas prendre de dommages


attack :: Modele -> Entite -> Coord -> Modele
attack modele entity c = (\(_, m) -> m) (foldl attackAux (2, modele) (case ( -- ici, l'attaque fait 2 de degats
  (E.getEntitiesAtCoord (C.Coord (C.cx c) ((C.cy c) + 1)) (envi modele) )
  `mml` (E.getEntitiesAtCoord (C.Coord (C.cx c) ((C.cy c) - 1)) (envi modele) )
  `mml` (E.getEntitiesAtCoord (C.Coord ((C.cx c) + 1) (C.cy c)) (envi modele) )
  `mml` (E.getEntitiesAtCoord (C.Coord ((C.cx c) - 1) (C.cy c)) (envi modele) )
  `mml` (E.getEntitiesAtCoord (C.Coord ((C.cx c) + 1) ((C.cy c) + 1)) (envi modele) )
  `mml` (E.getEntitiesAtCoord (C.Coord ((C.cx c) + 1) ((C.cy c) - 1)) (envi modele) )
  `mml` (E.getEntitiesAtCoord (C.Coord ((C.cx c) - 1) ((C.cy c) + 1)) (envi modele) )
  `mml` (E.getEntitiesAtCoord (C.Coord ((C.cx c) - 1) ((C.cy c) - 1)) (envi modele) )) of
    Just liste -> liste
    Nothing -> []
     ))




pickOrder :: [Ordre] -> Modele -> (Ordre, Modele)
pickOrder orders modele = (orders!!((R.randomRs (0,(length orders) - 1) (snd (gene modele)))!!((R.randomRs (1,99999) (snd (gene modele)))!!1)) -- On renvoie une ordre aleatoirement dans la liste (2eme element d'une liste liste d'entiers aléatoires)
                        , modele { gene = ( (fst (gene modele)) + 1, R.mkStdGen (fst (gene modele))) } ) -- on met à jour le generateur avec une nouvelle seed


handlePlayerActions :: Modele -> Entite -> Keyboard -> Coord -> Modele
handlePlayerActions modele player kbd c
    | (K.keypressed KeycodeZ kbd) = bouge modele player (C.Coord (C.cx c) ((C.cy c) - 1))
    | (K.keypressed KeycodeS kbd) = bouge modele player (C.Coord (C.cx c) ((C.cy c) + 1))
    | (K.keypressed KeycodeD kbd) = bouge modele player (C.Coord ((C.cx c) + 1) (C.cy c))
    | (K.keypressed KeycodeQ kbd) = bouge modele player (C.Coord ((C.cx c) - 1) (C.cy c))
    -- | (K.keypressed KeycodeQ kbd) = attack modele player c
    --actions à ajouter ici
    | otherwise = modele


gameStepAux :: Modele -> Keyboard -> [Entite] -> Modele
gameStepAux modele kbd (entity:entities) = case E.trouveId (E.idn entity) (envi modele) of
  Just (c, E.Joueur _ _ _ _) ->  gameStepAux (handlePlayerActions modele entity kbd c) kbd entities
  Just (c, E.Monstre _ _ _ _) -> gameStepAux (decider (prevoir entity) modele entity) kbd entities
  _ -> gameStepAux modele kbd entities
gameStepAux modele kbd [] = modele

gameStep :: RealFrac a => Modele -> Keyboard -> a -> Modele
gameStep modele kbd deltaTime = gameStepAux modele kbd (E.listEntities (envi modele))
