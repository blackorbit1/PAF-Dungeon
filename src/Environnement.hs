module Environnement where

import qualified Data.Map.Strict as M
import Carte

import Data.Ord
import Data.List




---------------------STRUCTURES----------------------


data Envi = Envi { contenu_envi :: M.Map Coord [Entite] }

data Entite = Monstre {idn :: Int , pvie :: Int, clearanceLevel :: Int, isFranchissable :: Bool}
            | Joueur {idn :: Int , pvie :: Int, clearanceLevel :: Int, isFranchissable :: Bool}
    deriving (Eq, Show)




---------------------INSTANCES----------------------

instance Show Envi where
    show = toString

toStringAux :: [Entite] -> String
toStringAux (x:xs) = strFromEntite x
toStringAux [] = "-"

instance ToString Envi where
        toString env = fst (foldl (\(strAcc, y) (coord, entities) -> ((strAcc ++ (if ((cy coord) > y) then "|\n|" else "") ++ (toStringAux entities)), (cy coord)) ) ("",0) (listFromEnv env)) 

createEnvi :: Carte -> String -> Envi
createEnvi carte texte = foldl 
                        (\ env@(Envi {contenu_envi = ce}) (coord, _ ) -> env {contenu_envi = (M.insert coord [] ce) })
                        (Envi M.empty) (listFromCarte carte)



---------------------UTILITAIRES----------------------


franchissableEnv :: Coord -> Envi -> Bool
franchissableEnv coord env = case (getEntitiesAtCoord coord env) of
        Just entities -> foldl (\boolAcc entity -> (boolAcc && (isFranchissable entity))) True entities
        Nothing -> False

-- Cree une Entite a partir d'un caractère
entiteFromChar :: Char -> Int -> Entite
entiteFromChar caractere idn = case caractere of
    'J' -> Joueur idn 300 10 False
    'M' -> Monstre idn 100 20 False

-- Renvoie le caractère correspondant à l'Entite
strFromEntite :: Entite -> String
strFromEntite ca = case ca of
    Joueur _ _ _ _ -> "J"
    Monstre _ _ _ _ -> "M"

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



---------------------OPERATIONS----------------------


setEntity :: Entite -> Coord -> Envi -> Envi 
setEntity entite coord env = case getEntitiesAtCoord coord env of
        Just new_entities -> Envi (M.insert coord (entite:(new_entities)) (contenu_envi env))
        Nothing -> env  -- cette fonction ne permet pas de placer des entités en dehors des coordonnées de la map
                        -- mais il est toujours possible de les placer à des endroits où ils ne devraient pas pouvoir aller

rmEntById :: Int -> Envi -> Envi
rmEntById idn env = case trouveId idn env of
        Just (coord, en) ->  Envi(M.insert coord (delete en (case (getEntitiesAtCoord coord env) of
                        Just entities -> entities
                        Nothing -> [] -- impossible, trouveId a rendu Just, il y a forcement une liste [Entite] a la clé coord
                )) (contenu_envi env))
        Nothing -> env


bougeById :: Int -> Coord -> Envi -> Carte -> Envi
bougeById idn coord env carte = case ((trouveId idn env), (getCase coord carte)) of
        ((Just (_, entite)), (Just ca)) -> if (isTraversable ca (clearanceLevel entite)) && (franchissableEnv coord env) then
                (setEntity entite coord (rmEntById idn env)) else env
        _ -> env --si trouveId ou getCase renvoient Nothing



---------------------INVARIANTS----------------------


--    qu'il n'y ait pas coordonnées négatives
--    qu'il n'y ait pas 2 mobs non traversables sur la meme case
--    que tous les id soient uniques
--    que les PV et clearanceLevel ne soient pas négatifs
-- X  qu'il n'y ait pas de liste d'entités en dehors des limites de la carte
-- X  vérifier que les mobs ont le droit d'être sur la case sur laquelle ils sont
-- No qu'il n'y ait qu'un seul joueur

positiveCoord :: Coord -> Bool
positiveCoord co = ((cx co) >= 0) && ((cy co) >= 0) 

allCoordsPositive_inv :: Envi -> Bool
allCoordsPositive_inv env = foldl (\boolAcc (co,_) -> boolAcc && (positiveCoord co)) True (listFromEnv env)


oneUncrossableMobPerCase_inv :: Envi -> Bool
oneUncrossableMobPerCase_inv env = foldl (\boolAcc (co,_) -> boolAcc && (case (getEntitiesAtCoord co env) of
        Just entities -> if (foldl (\uncrossableAcc entity ->  uncrossableAcc + (if (isFranchissable entity) then 0 else 1)) 0 entities) <= 1 then True else False
        Nothing -> True
        )) True (listFromEnv env)

positiveStats_inv :: Envi -> Bool
positiveStats_inv env = foldl (\boolAcc (co,_) -> boolAcc && (case (getEntitiesAtCoord co env) of
        Just entities -> foldl (\boolAcc2 entity ->  boolAcc2 && (((pvie entity) >= 0) && ((clearanceLevel entity) >= 0))) True entities
        Nothing -> True
        )) True (listFromEnv env)



hasUniqueId :: Entite -> Envi -> Bool
hasUniqueId entity env = foldl (\boolAcc (co,_) -> boolAcc && (case (getEntitiesAtCoord co env) of
        Just entities -> foldl (\boolAcc2 entity2 -> boolAcc2 && if entity /= entity2 then (idn entity) /= (idn entity2) else True) True entities
        Nothing -> True
        )) True (listFromEnv env)


quadraticUniqueIds_inv ::  Envi -> Bool
quadraticUniqueIds_inv env = foldl (\boolAcc (co,_) -> boolAcc && (case (getEntitiesAtCoord co env) of
        Just entities -> foldl (\boolAcc2 entity ->  boolAcc2 && hasUniqueId entity env) True entities
        Nothing -> True
        )) True (listFromEnv env)


uniqueIds_inv ::  Envi -> Bool
uniqueIds_inv env = (\(result, _) -> result) (foldl (\(boolAcc, seenIdn) (co,_) -> (case (getEntitiesAtCoord co env) of
                Just entities -> (foldl (\(boolAcc2, seenIdn2) entity ->  ((boolAcc2 && ((idn entity) `notElem` seenIdn2)), (idn entity):seenIdn2)) (boolAcc, seenIdn) entities)
                Nothing -> (boolAcc, seenIdn)
                )
        ) (True , []) (listFromEnv env)) 



---------------------PRECONDITION----------------------


-> Carte -

---------------------POSTCONDITION----------------------

