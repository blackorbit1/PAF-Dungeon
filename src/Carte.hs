module Carte where
import qualified Data.Map.Strict as M
--import qualified Data.List as O
import Data.Ord
import Data.List

import Environnement
import qualified Environnement as E




---------------------STRUCTURES----------------------


data PDirection = NS | EO deriving (Eq, Show) -- direction d’une porte

data StatutP = Ouverte | Fermee deriving (Eq, Show) -- statut d’une porte

data Case = Normal -- une case vide
    | Porte PDirection StatutP -- une porte ouverte ou fermee
    | Piege 
    | Mur -- infranchissable (sauf pour les fantomes ...)
    | Entree -- debut du niveau
    | Sortie -- fin du niveau
    deriving (Eq, Show)

data Coord = Coord {cx :: Int , cy :: Int} deriving (Eq, Show)

data Carte = Carte {       
    cartel :: Int ,
    carteh :: Int , 
    carte_contenu :: (M.Map Coord Case) 
    }


---------------------INSTANCES----------------------



instance Ord Coord where
    --(<=) :: Coord -> Coord -> Ordering
    --c1 <= c2 = (cy c1) <= (cy c2) || (((cy c1) == (cy c2)) && ((cx c1) <= (cx c2)))
    compare c1 c2   | (cy c1) <= (cy c2) = LT
                    | otherwise = if (cx c1) <= (cx c2) then LT else GT


--foo x y | first_name x /= first_name y = EQ
--        | otherwise                   = (comparing last_name) x y



instance Read Carte where
    readsPrec _ x = [((createCarte ({-reverse-} x)), "")]

createCarteAux :: Carte -> Char -> Carte
createCarteAux c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) '\n' = c {cartel = 0, carteh = ch + 1, carte_contenu = cc}
createCarteAux c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) caractere = c {cartel = cl + 1, carteh = ch, carte_contenu = M.insert (Coord (cl) ch) (caseFromChar caractere) cc }


{-
a b c \n
d e f \n 
     
           a  b  c  \n  d
cartel -1  0  1  2  -1  0
carteh 0   0  0  0  1   1
cx         0  1  2      0
cy         0  0  0      1
-}


[(Coord {cx = 0, cy = 2},Mur),(Coord {cx = 1, cy = 1},Normal),(Coord {cx = 0, cy = 1},Mur),
(Coord {cx = 1, cy = 2},Mur),(Coord {cx = 2, cy = 0},Mur),(Coord {cx = 1, cy = 0},Mur),
(Coord {cx = 0, cy = 0},Mur),(Coord {cx = 2, cy = 2},Mur),(Coord {cx = 2, cy = 1},Mur)]

createCarte :: String -> Carte
createCarte texte = foldl createCarteAux (Carte 0 0 M.empty) texte



instance Show Carte where
    show = toString

class ToString a where
    toString :: a -> String

toStringCarteAux :: Int -> (Coord, Case) -> String
--toStringCarteAux lar (co, ca) = if (cx co) == (max 0 (lar - 1)) then (strFromCase ca) ++ "\n" else (strFromCase ca)
toStringCarteAux cartel (co, ca) = if (cx co) == (max 0 (cartel - 1)) then (strFromCase ca) ++ "\n" else (strFromCase ca)


instance ToString Carte where
    --toString c = foldl (\accstr cur -> accstr ++ (toStringCarteAux (cartel c) cur) ) "" (M.assocs (carte_contenu c) )
    --toString c = foldl (\accstr cur -> accstr ++ (toStringCarteAux (cartel c) cur) ) "" (sortBy (comparing getCoord) (M.assocs (carte_contenu c) ))
    --toString c = show (M.assocs (carte_contenu c))
    toString c = foldl (\accstr cur -> accstr ++ (toStringCarteAux (cartel c) cur) ) "" (listFromCarte c)



---------------------UTILITAIRES----------------------


caseFromChar :: Char -> Case
caseFromChar caractere = case caractere of
    ' ' -> Normal
    '|' -> Porte EO Fermee
    '_' -> Porte NS Fermee
    '/' -> Porte EO Ouverte
    '^' -> Porte NS Ouverte
    'o' -> Piege
    'X' -> Mur
    'E' -> Entree
    'S' -> Sortie


strFromCase :: Case -> String
strFromCase ca = case ca of
    Normal -> " "
    Porte EO Fermee -> "|"
    Porte NS Fermee -> "-"
    Porte EO Ouverte -> "/"
    Porte NS Ouverte -> "_"
    Piege -> "o"
    Mur -> "X"
    Entree -> "E"
    Sortie -> "S"


getCoord :: (Coord, Case) -> Coord
getCoord (c, _) = c

listFromCarte :: Carte -> [(Coord,Case)]
listFromCarte carte = (sortBy (comparing fst) (M.assocs (carte_contenu carte) ))

---------------------OPERATIONS----------------------


getCase :: Coord -> Carte -> Maybe Case
getCase coord carte = M.lookup coord (carte_contenu carte)

isTraversable :: Case -> Entite -> Bool  
isTraversable ca ent =
    case ca of
        Normal -> (clearanceLevel ent) >= 1
        Entree -> (clearanceLevel ent) >= 1
        Sortie -> (clearanceLevel ent) >= 1
        Porte _ Ouverte ->  (clearanceLevel ent) >= 10
        Piege  ->           (clearanceLevel ent) >= 20
        Porte _ Fermee ->   (clearanceLevel ent) >= 30
        Mur -> (clearanceLevel ent) >= 40
        
editCase :: Coord -> Case -> Carte -> Carte
editCase coord ca carte = Carte (carteh carte) (cartel carte) (M.insert coord ca (carte_contenu carte))

openDoor :: Coord -> Carte -> Carte
openDoor coord carte = case getCase coord carte of 
    Just (Porte NS _) -> editCase coord (Porte NS Ouverte) carte
    Just (Porte EO _) -> editCase coord (Porte EO Ouverte) carte

closeDoor :: Coord -> Carte -> Carte
closeDoor coord carte = case getCase coord carte of 
    Just (Porte NS _) -> editCase coord (Porte NS Fermee) carte
    Just (Porte EO _) -> editCase coord (Porte EO Fermee) carte



---------------------INVARIANTS----------------------

{-
prop_revrev :: Eq a => [a] -> [a] -> Bool
prop_revrev xs ys = reverse (xs <> ys) == reverse ys <> reverse xs
-}


coordInBounds :: Coord -> Int -> Int -> Bool
coordInBounds co larg haut = ( ( (cx co) < larg ) 
                         && ( (cy co) < haut ) )


allCoordsInBounds_inv :: Carte -> Bool
allCoordsInBounds_inv carte = foldl (\boolAcc (co,_) -> boolAcc && coordInBounds co (cartel carte) (carteh carte) ) True (M.assocs (carte_contenu carte) )

{-
caseExists :: Int -> Int -> Carte -> Bool
caseExists x y = case (M.lookup (Coord x y) (carte_contenu carte) ) of
                Just _ -> True
                Nothing -> False

allCaseExists_inv :: Carte -> Bool
caseExists_inv carte = 

-}
