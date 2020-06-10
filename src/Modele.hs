
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



---------------------STRUCTURES----------------------


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


---------------------INSTANCES----------------------


instance Show Modele where
    show = toString

class ToString a where
    toString :: a -> String

instance ToString Modele where
    toString modele = (show (carte modele)) ++ "\n\n" ++ (show (envi modele))



initModele :: Carte -> Envi -> Modele
initModele carte envi = Modele carte envi (1, mkStdGen 0) "" K.createKeyboard


---------------------UTILITAIRES----------------------

-- Fonction utilitaire permettant de concatener des Maybe [a]
mml :: Maybe [a] -> Maybe [a] -> Maybe [a] -- merge maybe list
mml a b = case (a,b) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just b) -> Just b
  (Just a, Nothing) -> Just a 
  (Just a, Just b) -> Just (a <> b)



---------------------OPERATIONS----------------------

-- Permet de déplacer une entitée à des coordonnées données dans le modele
bouge :: Modele -> Entite -> Coord -> Modele
bouge modele entity coord = modele { envi = (E.bougeById (E.idn entity) coord (envi modele) (carte modele)) }

prop_bouge_pre :: Modele -> Entite -> Coord -> Bool
prop_bouge_pre modele entity coord = (E.prop_bougeById_pre (E.idn entity) coord (envi modele) (carte modele))

prop_bouge_post :: Modele -> Entite -> Coord -> Bool
prop_bouge_post modele entity coord = (E.prop_bougeById_post (E.idn entity) coord (envi modele) (carte modele))



-- Renvoie une liste associant chaque action qu'une entité données peut faire en fonction de l'environnement dans laquelle elle se trouve
prevoir :: Entite -> Envi -> [(Int,Ordre)]
prevoir entity env = case entity of
  E.Monstre _ _ _ _ _ -> if (
      case (E.getPlayerCoord env, E.entityCoord entity env) of
        (Just coP, Just coM) -> ((abs ((C.cx coP) - (C.cx coM))) <= 2) && ((abs ((C.cy coP) - (C.cy coM))) <= 2)
        (_, _) -> False )
    then [(1, Haut ),(1, Bas ),(1, Droite ),(1, Gauche ), (2, Rien ), (0, Uti ), (8, Atk )]
    else [(1, Haut ),(1, Bas ),(1, Droite ),(1, Gauche ), (2, Rien ), (0, Uti ), (0, Atk )]
  otherwise -> [(1, Haut ),(1, Bas ),(1, Droite ),(1, Gauche ),(1, Uti ), (0, Atk ), (1, Rien )]

prop_prevoir_pre :: Entite -> Envi -> Bool
prop_prevoir_pre en env = (E.prop_Entite_inv en) && (E.prop_Envi_inv env)
-- la fonction n'apporte aucune modification qu'il faudrait vérifier à l'aide d'un post


-- permet de transformer une liste de paires associant chaque action à une probabilité en une simple liste des actions possibles 
-- ou chaque action se trouve n fois dans la liste pour n sa probabilité
transformPonderatedList :: [(Int, Ordre)] -> [Ordre]
transformPonderatedList ((0,ordre):xs) = transformPonderatedList xs
transformPonderatedList ((coef,ordre):xs) = ordre:(transformPonderatedList ((coef-1,ordre):xs))
transformPonderatedList [] = []

-- Permet, à partir d'une liste d'ordres possibles et d'un modele contenant un générateur de nombres aléatoire, de récupérer un ordre le plus aléatoirement possible
pickOrder :: [Ordre] -> Modele -> (Ordre, Modele)
pickOrder orders modele = (orders!!((R.randomRs (0,(length orders) - 1) (snd (gene modele)))!!((R.randomRs (1,99999) (snd (gene modele)))!!1)) -- On renvoie une ordre aleatoirement dans la liste (2eme element d'une liste liste d'entiers aléatoires)
                        , modele { gene = ( (fst (gene modele)) + 1, R.mkStdGen (fst (gene modele))) } ) -- on met à jour le generateur avec une nouvelle seed

-- Décide quels actions une entité doit faire en fonction d'une liste de couples associants actions possible et probabilité de cette action
decider :: [(Int, Ordre)] -> Modele -> Entite -> Modele
decider list m entity = 
  case (E.entityCoord entity (envi m)) of
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



-- Retire un certain nombre de points de vie à une entitée
-- cette fonction est une fonction auxiliaire de attack
applyDamage :: Int -> Modele -> Entite -> Int -> Int -> (Int, Modele)
applyDamage damage modele entity idnn pviee = case (E.entityCoord entity (envi modele)) of 
  Just coord ->  (damage, modele{ envi = (E.setEntity (E.removePvie entity damage) coord (E.rmEntById idnn (envi modele))) } )
  Nothing -> (damage, modele)                      -- dans le cas où l'entitee ne correspond à aucunes coordonnées dans l'environnement

-- cette fonction est une fonction auxiliaire de attack
attackAux :: (Int, Modele) -> Entite -> (Int, Modele)
attackAux (damage, modele) entity@(E.Monstre idnn pviee _ _ _) = applyDamage damage modele entity idnn pviee
attackAux (damage, modele) entity@(E.Joueur idnn pviee _ _ _ _) = applyDamage damage modele entity idnn pviee
attackAux (damage, modele) _ = (damage, modele) -- dans le cas où c'est une entitee qui ne peut pas prendre de dommages

-- Attaque toutes les cases 8-adjacentes
attack :: Modele -> Entite -> Coord -> Modele
attack modele entity c = (\(_, m@(Modele _ env _ _ _) ) -> m { envi = (E.setAttackingTrue entity env c) } ) (foldl attackAux (50, modele) (case ( -- ici, l'attaque fait 2 de degats
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




-- Ouvre toutes les portes 4-adjacentes
playerUse :: Modele -> Entite -> Coord -> Modele
playerUse modele player c = if (E.hasKey player) then
  let carte1 = C.tryOpenDoor (C.Coord (C.cx c) ((C.cy c) - 1)) (carte modele) in
  let carte2 = C.tryOpenDoor (C.Coord (C.cx c) ((C.cy c) + 1)) carte1 in
  let carte3 = C.tryOpenDoor (C.Coord ((C.cx c) + 1) (C.cy c)) carte2 in
  let carte4 = C.tryOpenDoor (C.Coord ((C.cx c) - 1) (C.cy c)) carte3 in
  modele { carte = carte4 }
  else modele

-- Applique une action sur le joueur en fonction de la touche sur laquelle il appuie 
handlePlayerActions :: Modele -> Entite -> Keyboard -> Coord -> Modele
handlePlayerActions modele player kbd c
    | (K.keypressed KeycodeZ kbd) = bouge modele player (C.Coord (C.cx c) ((C.cy c) - 1))
    | (K.keypressed KeycodeS kbd) = bouge modele player (C.Coord (C.cx c) ((C.cy c) + 1))
    | (K.keypressed KeycodeD kbd) = bouge modele player (C.Coord ((C.cx c) + 1) (C.cy c))
    | (K.keypressed KeycodeQ kbd) = bouge modele player (C.Coord ((C.cx c) - 1) (C.cy c))
    | (K.keypressed KeycodeSpace kbd) = attack modele player c
    | (K.keypressed KeycodeF kbd) = playerUse modele player c
    --actions à ajouter ici
    | otherwise = modele

-- ouvre un coffre et donne une clé au joueur 
handleChest :: Coord -> Entite -> Modele -> Modele
handleChest coord chest modele = case E.getPlayerCoord (envi modele) of
  Just c -> if coord == c then modele { envi = (E.openChest (E.idn chest) (E.giveKeyToPlayer (envi modele))) } else modele
  Nothing -> modele


-- fonction récursive permettant à chaque entitée d'accomplir une tache
gameStepAux :: Modele -> Keyboard -> [Entite] -> Modele
gameStepAux modele kbd (entity:entities) = case E.trouveId (E.idn entity) (envi modele) of
  Just (c, E.Joueur _ _ _ _ _ _) -> gameStepAux (handlePlayerActions modele entity kbd c) kbd entities
  Just (c, E.Monstre _ _ _ _ _) -> gameStepAux (decider (prevoir entity (envi modele)) modele entity) kbd entities
  Just (c, E.Coffre _ _ False) -> gameStepAux (handleChest c entity modele) kbd entities
  _ -> gameStepAux modele kbd entities
gameStepAux modele kbd [] = modele

gameStep :: RealFrac a => Modele -> Keyboard -> a -> Modele
gameStep modele kbd deltaTime = gameStepAux modele kbd (E.listEntities (envi modele))

prop_gameStep_pre :: RealFrac a => Modele -> Keyboard -> a -> Bool
prop_gameStep_pre modele kbd deltaTime = prop_Modele_inv modele


---------------------INVARIANTS----------------------

-- invariant du type modele
prop_Modele_inv :: Modele -> Bool
prop_Modele_inv modele = C.prop_Carte_inv (carte modele)
                      && E.prop_Envi_inv (envi modele)

