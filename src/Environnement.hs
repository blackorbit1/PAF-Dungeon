module Environnement where

import qualified Data.Map.Strict as M

import Carte

import Data.Ord
import Data.List

data Envi = Envi { contenu_envi :: M.Map Coord [Entite] }

data Entite = Monstre {idn :: Int , pvie :: Int, clearanceLevel :: Int, isFranchissable :: Bool}
            | Joueur {idn :: Int , pvie :: Int, clearanceLevel :: Int, isFranchissable :: Bool}
    deriving (Eq, Show)

createEnvi :: Carte -> String -> Envi
createEnvi carte texte = foldl 
                        (\ env@(Envi {contenu_envi = ce}) (coord, _ ) -> env {contenu_envi = (M.insert coord [] ce) })
                        (Envi M.empty) (listFromCarte carte)


{-
franchissableEnv :: Coord -> Envi -> Bool -- On a choisi de mettre le resultat en Maybe dans le cas où on demanderait
franchissableEnv co env = case (M.lookup co (contenu_envi env)) >>= (\tableau -> (foldl (\acc entite -> acc && (isFranchissable entite)) True tableau ))   of
        Just True -> True
        _ -> False
-}

-- Renvoie le type du mob correspondant à un caractère
entiteFromChar :: Char -> Int -> Entite
entiteFromChar caractere idn = case caractere of
    'J' -> Joueur idn 300 20 False
    'M' -> Monstre idn 100 30 False
    


listFromEnv :: Envi -> [(Coord, [Entite])]
listFromEnv env = (sortBy (comparing fst) (M.assocs (contenu_envi env) ))

entityFromId :: [Entite]-> Int -> Maybe Entite
entityFromId (entity:entities) wanted_idn = if (idn entity) == wanted_idn then Just entity else (entityFromId entities wanted_idn)
entityFromId [] _ = Nothing


trouveId_aux :: Int -> [(Coord, [Entite])] -> Maybe (Coord, Entite)
trouveId_aux idn ((coord, entities):xs) = case (entityFromId entities idn) of
        Just entity -> Just (coord, entity)
        Nothing -> trouveId_aux idn xs
trouveId_aux _ [] = Nothing

trouveId :: Int -> Envi -> Maybe (Coord, Entite)
trouveId idn env = trouveId_aux idn (listFromEnv env)

getEntitiesAtCoord :: Coord -> Envi -> Maybe [Entite]
getEntitiesAtCoord coord env = M.lookup coord (contenu_envi env)


setEntity :: Entite -> Coord -> Envi -> Envi 
setEntity entite coord env = case getEntitiesAtCoord coord env of
        Just new_entities -> Envi (M.insert coord (entite:(new_entities)) (contenu_envi env))
        Nothing -> env -- ce cas permet de respecter la signature de la fonction mais qui ne devrait pas se presenter grace à l'usage des pre-conditions








