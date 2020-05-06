module Carte where
import qualified Data.Map.Strict as M

import Environnement
import qualified Environnement as E


data PDirection = NS | EO deriving Eq -- direction d’une porte

data StatutP = Ouverte | Fermee deriving Eq -- statut d’une porte

data Case = Normal -- une case vide
    | Porte PDirection StatutP -- une porte ouverte ou fermee
    | Piege 
    | Mur -- infranchissable (sauf pour les fantomes ...)
    | Entree -- debut du niveau
    | Sortie -- fin du niveau
    deriving Eq

data Coord = Coord {cx :: Int , cy :: Int} deriving Eq

instance Ord Coord where
    c1 <= c2 = (cy c1) <= (cy c2) || (((cy c1) == (cy c2)) && ((cx c1) <= (cx c2)))


data Carte = Carte {       
    cartel :: Int ,
    carteh :: Int , 
    carte_contenu :: (M.Map Coord Case) 
    }

instance Read Carte where
    readsPrec _ x = [((createCarte (reverse x)), "")]


instance Show Carte where
    show = toString




class ToString a where
    toString :: a -> String

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

    
caseFromChar :: Char -> Case
caseFromChar caractere = case caractere of
    ' ' -> Normal
    '|' -> Porte EO Fermee
    '-' -> Porte NS Fermee
    '/' -> Porte EO Ouverte
    '_' -> Porte NS Ouverte
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

createCarteAux :: Char -> Carte -> Carte
createCarteAux '\n' c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) = c {cartel = -1, carteh = ch + 1, carte_contenu = cc}
createCarteAux caractere c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) = c {cartel = cl + 1, carteh = ch, carte_contenu = M.insert (Coord (cl + 1) ch) (caseFromChar caractere) cc }

createCarte :: String -> Carte
createCarte texte = foldr createCarteAux (Carte (-1) 0 M.empty) texte


toStringCarteAux :: Int -> (Coord, Case) -> String
toStringCarteAux lar (co, ca) = if (cx co) == (max 0 (lar - 1)) then (strFromCase ca) ++ "\n" else (strFromCase ca)




instance ToString Carte where
    toString c = foldl (\accstr cur -> accstr ++ (toStringCarteAux (cartel c) cur) ) "" (M.assocs (carte_contenu c) )






