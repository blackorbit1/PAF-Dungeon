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



insertEntitiesAux :: (Int, Int, Int, Envi) -> Char -> (Int, Int, Int, Envi)
insertEntitiesAux (x, y, idn, env) '\n' = (0, y+1, idn, env)
insertEntitiesAux (x, y, idn, env) ' ' = (x+1, y, idn, env)
insertEntitiesAux (x, y, idn, env) char = (x+1, y, (idn + 1), (setEntity (entiteFromChar char idn) (Coord x y) env))

insertEntities :: String -> Envi -> Envi
insertEntities texte env = (\(_, _, _, new_env) -> new_env) (foldl insertEntitiesAux (0, 0, 0, env) texte)


createEnvi :: Carte -> String -> Envi
createEnvi carte texte = insertEntities texte (foldl 
                        (\ env@(Envi {contenu_envi = ce}) (coord, _ ) -> env {contenu_envi = (M.insert coord [] ce) })
                        (Envi M.empty) (listFromCarte carte))

prop_createEnvi_pre :: Carte -> String -> Bool
prop_createEnvi_pre carte _ = prop_Carte_inv carte    -- faut-il verifier les invariant des types utilisés ?

prop_createEnvi_post :: Envi -> Bool
prop_createEnvi_post env = prop_Envi_inv env  -- meme question que pour la post condition de la carte



---------------------UTILITAIRES----------------------


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


listEntities :: Envi -> [Entite]
listEntities env = foldl (\entities (_,entlist) -> entities <> entlist) [] (listFromEnv env)


franchissableEnv :: Coord -> Envi -> Bool
franchissableEnv coord env = case (getEntitiesAtCoord coord env) of
        Just entities -> foldl (\boolAcc entity -> (boolAcc && (isFranchissable entity))) True entities
        Nothing -> False

prop_franchissableEnv_pre :: Coord -> Envi -> Bool
prop_franchissableEnv_pre coord env = prop_positiveCoord_inv coord
                               -- &&  prop_Envi_inv -- ? faut-il appeler les invariants des types utilisés dans les préconditions

prop_franchissableEnv_post :: Coord -> Envi -> Bool
prop_franchissableEnv_post coord env = undefined -- etant donne que cette fonction retourne une valeur, il n'y a pas de modification à évaluer après l'appel





entityFromId :: [Entite] -> Int -> Maybe Entite
entityFromId (entity:entities) wanted_idn = if (idn entity) == wanted_idn then Just entity else (entityFromId entities wanted_idn)
entityFromId [] _ = Nothing
-- cette fonction ne semble pas necessiter de pre condition particuliere (le maybe permettant de supporter des entrées incorrectes). De plus elle n'apporte aucune modification qu'il faudrait vérifier à l'aide d'un post

trouveId_aux :: Int -> [(Coord, [Entite])] -> Maybe (Coord, Entite)
trouveId_aux idn ((coord, entities):xs) = case (entityFromId entities idn) of
        Just entity -> Just (coord, entity)
        Nothing -> trouveId_aux idn xs
trouveId_aux _ [] = Nothing

trouveId :: Int -> Envi -> Maybe (Coord, Entite)
trouveId idn env = trouveId_aux idn (listFromEnv env)

prop_trouveId_pre :: Int -> Envi -> Bool
prop_trouveId_pre idn env = ( idn >= 0 )
                        && prop_uniqueIds_inv env -- ici il est particulierement important que les id soient uniques
-- la fonction n'apporte aucune modification qu'il faudrait vérifier à l'aide d'un post

getEntitiesAtCoord :: Coord -> Envi -> Maybe [Entite]
getEntitiesAtCoord coord env = M.lookup coord (contenu_envi env)
-- cette fonction ne semble pas necessiter de pre condition particuliere (le maybe permettant de supporter des entrées incorrectes). De plus elle n'apporte aucune modification qu'il faudrait vérifier à l'aide d'un post



---------------------OPERATIONS----------------------


setEntity :: Entite -> Coord -> Envi -> Envi 
setEntity entite coord env = case getEntitiesAtCoord coord env of
        Just new_entities -> Envi (M.insert coord (entite:(new_entities)) (contenu_envi env))
        Nothing -> env  -- cette fonction ne permet pas de placer des entités en dehors des coordonnées de la map
                        -- mais il est toujours possible de les placer à des endroits où ils ne devraient pas pouvoir aller

prop_setEntity_pre :: Entite -> Coord -> Envi -> Bool 
prop_setEntity_pre entity _ env =  prop_Envi_inv env
                                && prop_Entite_inv entity
                                && ((trouveId (idn entity) env) == Nothing ) -- l'id ne doit pas déjà exister dans l'environnement

prop_setEntity_post :: Entite -> Coord -> Envi -> Bool 
prop_setEntity_post entity coord env = ((trouveId (idn entity) env) == (Just (coord, entity))) -- on retrouve bien l'entite par id aux nouvelles coordonnees
                                    && (case (getEntitiesAtCoord coord env) of
                                                Just entities -> entity `elem` entities         --l'entite est bien dans la liste d'entites aux nouvelles coordonnes
                                                Nothing -> False )



rmEntById :: Int -> Envi -> Envi
rmEntById idn env = case trouveId idn env of
        Just (coord, en) ->  Envi(M.insert coord (delete en (case (getEntitiesAtCoord coord env) of
                        Just entities -> entities
                        Nothing -> [] -- impossible, trouveId a rendu Just, il y a forcement une liste [Entite] a la clé coord
                )) (contenu_envi env))
        Nothing -> env

prop_rmEntById_pre :: Int -> Envi -> Bool 
prop_rmEntById_pre idn env = (idn > 0) && ((trouveId idn env) /= Nothing )

prop_rmEntById_post :: Int -> Envi -> Bool 
prop_rmEntById_post idn env = (idn > 0) && ((trouveId idn env) == Nothing )


bougeById :: Int -> Coord -> Envi -> Carte -> Envi
bougeById idn coord env carte = case ((trouveId idn env), (getCase coord carte)) of
        ((Just (_, entite)), (Just ca)) -> if (isTraversable ca (clearanceLevel entite)) && (franchissableEnv coord env) then
                (setEntity entite coord (rmEntById idn env)) else env
        _ -> env -- si l'entite ou la case n'existe pas on renvoie l'env sans modification

prop_bougeById_pre :: Int -> Coord -> Envi -> Carte -> Bool
prop_bougeById_pre idn coord env carte =   (coordInCarte coord carte)
                                        && ((getEntitiesAtCoord coord env) /= Nothing )
                                        && (idn > 0)
                                        && ((trouveId idn env) /= Nothing )

prop_bougeById_post :: Int -> Coord -> Envi -> Carte -> Bool
prop_bougeById_post idn coord env carte =  (prop_uniqueIds_inv env) -- pour etre sur que l'entité n'a pas été dupliquée
                                        && ((trouveId idn env) /= Nothing ) -- l'entitee n'a pas été supprimee pendant son deplacement
                                        && (case ((getEntitiesAtCoord coord env), (trouveId idn env)) of
                                                ((Just entities), (Just (co, entity))) -> (entity `elem` entities) && (coord == co)
                                                otherwise -> False )

                                                
---------------------INVARIANTS----------------------


--    qu'il n'y ait pas coordonnées négatives
--    qu'il n'y ait pas 2 mobs non traversables sur la meme case
--    que tous les id soient uniques
--    que les PV et clearanceLevel ne soient pas négatifs
-- X  qu'il n'y ait pas de liste d'entités en dehors des limites de la carte
-- X  vérifier que les mobs ont le droit d'être sur la case sur laquelle ils sont
-- No qu'il n'y ait qu'un seul joueur

 

prop_allCoordsPositive_inv :: Envi -> Bool
prop_allCoordsPositive_inv env = foldl (\boolAcc (co,_) -> boolAcc && (prop_positiveCoord_inv co)) True (listFromEnv env)


prop_oneUncrossableMobPerCase_inv :: Envi -> Bool
prop_oneUncrossableMobPerCase_inv env = foldl (\boolAcc (co,_) -> boolAcc && (case (getEntitiesAtCoord co env) of
        Just entities -> if (foldl (\uncrossableAcc entity ->  uncrossableAcc + (if (isFranchissable entity) then 0 else 1)) 0 entities) <= 1 then True else False
        Nothing -> True
        )) True (listFromEnv env)

prop_positiveStats_inv :: Envi -> Bool
prop_positiveStats_inv env = foldl (\boolAcc (co,_) -> boolAcc && (case (getEntitiesAtCoord co env) of
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


prop_uniqueIds_inv ::  Envi -> Bool
prop_uniqueIds_inv env = (\(result, _) -> result) (foldl (\(boolAcc, seenIdn) (co,_) -> (case (getEntitiesAtCoord co env) of
                Just entities -> (foldl (\(boolAcc2, seenIdn2) entity ->  ((boolAcc2 && ((idn entity) `notElem` seenIdn2)), (idn entity):seenIdn2)) (boolAcc, seenIdn) entities)
                Nothing -> (boolAcc, seenIdn)
                )
        ) (True , []) (listFromEnv env)) 


prop_Envi_inv :: Envi -> Bool
prop_Envi_inv env = (prop_allCoordsPositive_inv env)
                 && (prop_oneUncrossableMobPerCase_inv env)
                 && (prop_positiveStats_inv env)
                 && (prop_uniqueIds_inv env)
                 && (foldl (\boolAcc entity -> boolAcc && (prop_Entite_inv entity)) True (listEntities env))


prop_Entite_inv :: Entite -> Bool
prop_Entite_inv entity =   ((idn entity) >= 0)
                        && ((pvie entity) >= 0)
                        && ((clearanceLevel entity) >= 0)

