module Carte where

import qualified Data.Map.Strict as M

import Data.Ord
import Data.List




---------------------STRUCTURES----------------------


data PDirection = NS | EO deriving (Eq, Show) -- direction d’une porte

data StatutP = Ouverte | Fermee deriving (Eq, Show) -- statut d’une porte

data Case = Normal -- une case vide
    | Porte PDirection StatutP -- une porte ouverte ou fermee
    | Piege 
    | Mur -- infranchissable (sauf pour les fantomes ...)
    | Entree -- debut du niveau
    | Sortie -- fin du niveau
    | Undefined -- case indefinie, (pour les caracteres inconnus a l'initialisation depuis le fichier carte)
    deriving (Eq, Show)

data Coord = Coord {cx :: Int , cy :: Int} deriving (Eq, Show)

data Carte = Carte {       
    cartel :: Int ,
    carteh :: Int , 
    carte_contenu :: (M.Map Coord Case) 
    }


---------------------INSTANCES----------------------


-- définition de la maniere d'ordonner des coordonnees
instance Ord Coord where
    compare c1 c2
        | (cy c1) < (cy c2) = LT
        | ((cy c1) == (cy c2)) && ((cx c1) < (cx c2)) = LT
        | ((cy c1) == (cy c2)) && ((cx c1) == (cx c2)) = EQ
        | otherwise = GT



instance Read Carte where
    readsPrec _ x = [((createCarte ({-reverse-} x)), "")]

-- fonction auxiliaire au foldl pour ajouter une case en fonction de son type et avec ses coordonnees x et y dans une carte donnee
createCarteAux :: Carte -> Char -> Carte
createCarteAux c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) '\n' = c {cartel = 0, carteh = ch + 1, carte_contenu = cc}
createCarteAux c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) caractere = c {cartel = cl + 1, carteh = ch, carte_contenu = M.insert (Coord (cl) (ch - 1)) (caseFromChar caractere) cc }


-- creer une carte à partir d'une chaine de caractere
createCarte :: String -> Carte
createCarte texte = foldl createCarteAux (Carte 0 1 M.empty) texte

prop_createCarte_pre :: String -> Bool
prop_createCarte_pre str = foldl (\boolAcc cara -> boolAcc && ((caseFromChar cara) /= Undefined) ) True str     --  Tous les caracteres de l'initialisation doivent etre reconnus

prop_createCarte_post :: Carte -> Bool
prop_createCarte_post carte = prop_Carte_inv carte  -- la carte cree doit etre valide  (impossible si elle correspond bien au txt)

instance Show Carte where
    show = toString

class ToString a where
    toString :: a -> String

toStringCarteAux :: Int -> (Coord, Case) -> String
--toStringCarteAux lar (co, ca) = if (cx co) == (max 0 (lar - 1)) then (strFromCase ca) ++ "\n" else (strFromCase ca)
toStringCarteAux cartel (co, ca) = if (cx co) == (max 0 (cartel - 1)) then (strFromCase ca) ++ "\n" else (strFromCase ca)


instance ToString Carte where
    toString c = "largeur = " ++ (show (cartel c)) ++
                "\nhauteur = " ++ (show (carteh c)) ++ "\n" ++
                foldl (\accstr cur -> accstr ++ (toStringCarteAux (cartel c) cur) ) "" (listFromCarte c)



---------------------UTILITAIRES----------------------

-- Renvoie le type de la case correspondant à un caractère
caseFromChar :: Char -> Case
caseFromChar caractere = case caractere of
    ' ' -> Normal
    '|' -> Porte EO Fermee
    '-' -> Porte NS Fermee
    '/' -> Porte EO Ouverte
    '^' -> Porte NS Ouverte
    'o' -> Piege
    'X' -> Mur
    'E' -> Entree
    'S' -> Sortie
    otherwise -> Undefined

-- Renvoie le caractère  correspondant à un type de case donné
strFromCase :: Case -> String
strFromCase ca = case ca of
    Normal -> " "
    Porte EO Fermee -> "|"
    Porte NS Fermee -> "-"
    Porte EO Ouverte -> "/"
    Porte NS Ouverte -> "^"
    Piege -> "o"
    Mur -> "X"
    Entree -> "E"
    Sortie -> "S"


getCoord :: (Coord, Case) -> Coord
getCoord (c, _) = c

-- transforme une carte en liste de couples coordonnees / case
listFromCarte :: Carte -> [(Coord,Case)]
listFromCarte carte = (sortBy (comparing fst) (M.assocs (carte_contenu carte) ))




---------------------OPERATIONS----------------------

-- recuperer une case à des coordonnees donnees
getCase :: Coord -> Carte -> Maybe Case
getCase coord carte = M.lookup coord (carte_contenu carte)

prop_getCase_pre :: Coord -> Carte -> Bool
prop_getCase_pre co carte = (coordInBounds co (cartel carte) (carteh carte))   --  la coordoonee est bien dans les bornes de la carte
                    && (coordInCarte co carte)      --  co doit bien etre une cle dans table associative de la carte

prop_getCase_post :: Coord -> Carte -> Bool
prop_getCase_post co carte = undefined -- etant donne que cette fonction retourne une valeur, il n'y a pas de modification à évaluer après l'appel




-- savoir si une entité peut travaerser une case en fonction de son niveau d'accréditation
isTraversable :: Case -> Int -> Bool  
isTraversable ca clearanceLevel =
    case ca of
        Normal -> clearanceLevel >= 1
        Entree -> clearanceLevel >= 1
        Sortie -> clearanceLevel >= 1
        Porte _ Ouverte ->  clearanceLevel >= 10
        Piege  ->           clearanceLevel >= 20
        Porte _ Fermee ->   clearanceLevel >= 30
        Mur -> clearanceLevel >= 40

prop_isTraversable_pre :: Case -> Int -> Bool
prop_isTraversable_pre ca clearanceLevel = clearanceLevel >= 0 -- "A FAIRE ! quand on aura les entites"

prop_isTraversable_post :: Case -> Int -> Bool
prop_isTraversable_post ca entite = undefined -- etant donne que cette fonction retourne une valeur, il n'y a pas de modification à évaluer après l'appel




-- modifier une case donnee dans une carte
editCase :: Coord -> Case -> Carte -> Carte
editCase coord ca carte = Carte (carteh carte) (cartel carte) (M.insert coord ca (carte_contenu carte))

{-
editcaseSure
    PRECONDITION
    editcase ----------\ est ce que le post condition doit prendre le résulatt d'edit case ou faire lui meme le edit case ? (avec du coup la meme signature qu'editcase)
    POSTCONDITION  <---/
-}

prop_editCase_pre :: Coord -> Case -> Carte -> Bool 
prop_editCase_pre co ca carte = (cx co) >= 0 && (cy co) >= 0
                        && (cx co) < (cartel carte)     --  x doit être strictement inférieur à la largeur de la carte
                        && (cy co) < (carteh carte)     --  y doit être strictement inférieur à la hauteur de la carte
                        && (coordInCarte co carte)      --  co doit bien etre une cle dans table associative de la carte

prop_editCase_post :: Coord -> Case -> Carte -> Bool 
prop_editCase_post co ca carte = (\new_carte -> (noChangesExceptAtCoord carte co new_carte) && ((Just ca) == (getCase co new_carte))) (editCase co ca carte)





-- ouvrir une porte (qu'elle soit déjà ouverte ou non)
openDoor :: Coord -> Carte -> Carte
openDoor coord carte = case getCase coord carte of 
    Just (Porte NS _) -> editCase coord (Porte NS Ouverte) carte
    Just (Porte EO _) -> editCase coord (Porte EO Ouverte) carte

prop_openDoor_pre :: Coord -> Carte -> Bool
prop_openDoor_pre coord carte = case (getCase coord carte) of
    Just (Porte _ _) -> True  -- la case au coordonnees demandees est bien une porte
    _ -> False              -- sinon , on renvoie False                
-- A noter : on ne met pas de précondition sur l'etat de la porte avant de l'ouvrir (ouvrir une porte ouverte n'a pas d'effet)

prop_openDoor_post :: Coord -> Carte -> Bool 
prop_openDoor_post co carte = (\new_carte -> (noChangesExceptAtCoord carte co new_carte) && (case (getCase co new_carte) of
    Just (Porte _ Ouverte) -> True
    _ -> False )) (openDoor co carte)






-- fermer une porte (qu'elle soit déjà fermee ou non)
closeDoor :: Coord -> Carte -> Carte
closeDoor coord carte = case getCase coord carte of 
    Just (Porte NS _) -> editCase coord (Porte NS Fermee) carte
    Just (Porte EO _) -> editCase coord (Porte EO Fermee) carte

prop_closeDoor_pre :: Coord -> Carte -> Bool
prop_closeDoor_pre coord carte = case (getCase coord carte) of
    Just (Porte _ _) -> True  -- la case au coordonnees demandees est bien une porte
    _ -> False              -- sinon , on renvoie False                  
-- A noter : on ne met pas de précondition sur l'etat de la porte avant de la fermer (fermer une porte fermee n'a pas d'effet)

prop_closeDoor_post :: Coord -> Carte -> Bool 
prop_closeDoor_post co carte = (\new_carte -> (noChangesExceptAtCoord carte co new_carte) && (case (getCase co new_carte) of
    Just (Porte _ Fermee) -> True
    _ -> False )) (closeDoor co carte)




---------------------INVARIANTS----------------------

prop_positiveCoord_inv :: Coord -> Bool
prop_positiveCoord_inv co = ((cx co) >= 0) && ((cy co) >= 0)

-- Vérifie qu'une coordonnée donee est entre les bornes données par la largeur et hauteur
coordInBounds :: Coord -> Int -> Int -> Bool
coordInBounds co larg haut = (prop_positiveCoord_inv co)
                          && ( ( (cx co) < larg ) 
                          && ( (cy co) < haut ) )

-- Vérifie que toutes les coordonnées des cases sont entre les bornes données par la largeur et hauteur de la carte
prop_allCoordsInBounds_inv :: Carte -> Bool
prop_allCoordsInBounds_inv carte = foldl (\boolAcc (co,_) -> boolAcc && coordInBounds co (cartel carte) (carteh carte) ) True (listFromCarte carte)

-------
-- Vérifie qu'une coordonnée donnée correspond bien à une case dans la carte
coordInCarte :: Coord -> Carte -> Bool
coordInCarte coord carte = case (getCase coord carte) of
                    Just _ -> True
                    Nothing -> False

-- Vérifie que toutes les coordonnées avec comme borne la largeur et hauteur correspondent bien à des cases dans la carte
prop_allCoordInCarte_inv :: Carte -> Bool
-- allCoordInCarte_inv carte | (cartel carte) != 0 && (carteh carte) != 0 
prop_allCoordInCarte_inv carte = foldl (\boolAcc (x,y) -> boolAcc && coordInCarte (Coord x y) carte) True ((\ i j -> (i, j)) <$> [0..((cartel carte) - 1)] <*> [0..((carteh carte) - 1)])

-------

-- renvoie "e" si la case est une entrée, "s" si c'est une sortie et rien sinon
getEntranceOrExit :: Case -> String
getEntranceOrExit ca 
    | ca == Entree = "e"
    | ca == Sortie = "s"
    | otherwise = ""

-- vérifie qu'une carte donnée ne contient 1 seule entrée et 1 seule sortie
prop_entranceExit_inv :: Carte -> Bool
prop_entranceExit_inv carte = case foldl (\strAcc (_,ca) -> strAcc ++ (getEntranceOrExit ca)) "" (listFromCarte carte) of
    "es" -> True
    "se" -> True
    _ -> False

-------

-- Pour une case donnée, si c'est une bordure, renvoie False si ce n'est pas un mur, True sinon
ifIsEdge_IsAWall :: (Coord, Case) -> Int -> Int -> Bool
ifIsEdge_IsAWall (co, ca) l h = case ((cx co), (cy co)) of
        (0, _) -> ca == Mur -- on est sur le bord et c'est un mur
        (_, 0) -> ca == Mur -- on est sur le bord et c'est un mur
        (x, y) -> not (((x == l) || (y == h)) && (ca /= Mur))  -- si on est sur le bord ET ce n'est pas un mur alors c'est faux
        
-- Vérifie qu'une carte donnée est entierement entouree par des murs
prop_surroundedByWalls_inv :: Carte -> Bool
prop_surroundedByWalls_inv carte = foldl (\boolAcc c -> boolAcc && ifIsEdge_IsAWall c ((cartel carte) - 1) ((carteh carte) - 1) ) True (listFromCarte carte)

-------

-- vérifie qu'une case donnée, si c'est une porte, est bien encadrée par des murs (renvoie True pour tout autre type de case)
caseFramedByWalls :: (Coord, Case) -> Carte -> Bool
caseFramedByWalls (co, ca) carte = case ca of
    Porte EO _ -> ((getCase (Coord (cx co) ((cy co) - 1)) carte) == Just Mur) 
               && ((getCase (Coord (cx co) ((cy co) + 1)) carte) == Just Mur) 
    Porte NS _ -> ((getCase (Coord ((cx co) - 1) (cy co)) carte) == Just Mur) 
               && ((getCase (Coord ((cx co) + 1) (cy co)) carte) == Just Mur) 
    _ -> True

-- Vérifie que toutes les portes d'une carte donnée sont encadrées par des murs
prop_doorsFramedByWalls_inv :: Carte -> Bool
prop_doorsFramedByWalls_inv carte = foldl (\boolAcc c -> boolAcc && caseFramedByWalls c carte ) True (listFromCarte carte)

{-
Idee pour savoir s'il y a un chemin entre l'entree et la sortie

mydfs :: Grafe -> [Neud] -> [Neud] -> [Neud]
mydfs graph visited [] = reverse visited
mydfs graph visited (x:xs) | elem x visited = mydfs graph visited xs
                           | otherwise = mydfs graph (x:visited) ((graph !! x) ++ xs)
-}

-------

prop_Carte_inv :: Carte -> Bool
prop_Carte_inv carte = (prop_allCoordsInBounds_inv carte)
                    && (prop_allCoordInCarte_inv carte)
                    && (prop_entranceExit_inv carte)
                    && (prop_surroundedByWalls_inv carte)
                    && (prop_doorsFramedByWalls_inv carte)
                    && (((cartel carte) >= 4) && ((cartel carte) >= 3)
                    || ((cartel carte) >= 3) && ((cartel carte) >= 4)) -- il faut que la carte soit au moins de 3x4 ou 4x3 pour respecter entranceExit_inv et surroundedByWalls_inv
{-
 Exemple de cartes de tailles minimales respectant tous les invariants :

        XXXX
        XESX
        XXXX

        XXX
        XEX
        XSX 
        XXX 
-}



prop_Coord_inv :: Coord -> Bool
prop_Coord_inv coord = prop_positiveCoord_inv coord



---------------------CONDITION-UTILS----------------------

--vérifie si deux carte sont identiques, sauf a la coordonnee donnee
noChangesExceptAtCoord :: Carte -> Coord ->  Carte -> Bool
noChangesExceptAtCoord carte coord post_fonction = foldl (\boolAcc ((co1, ca1) , (co2, ca2)) -> 
                                                boolAcc && (if ((co1 /= coord) && (ca1 == ca2)) then True else False ) )
                                                True (zip (listFromCarte carte) (listFromCarte post_fonction))




