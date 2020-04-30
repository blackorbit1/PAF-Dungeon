import qualified Data.Map.Strict as M

data PDirection = NS | EO deriving Eq -- direction d’une porte

data StatutP = Ouverte | Fermee deriving Eq -- statut d’une porte

data Case = Normal -- une case vide
    | Porte PDirection StatutP -- une porte ouverte ou fermee
    | Piege 
    | Mur -- infranchissable (sauf pour les fantomes ...)
    | Entree -- debut du niveau
    | Sortie -- fin du niveau
    deriving Eq

data Coord = C {cx :: Int , cy :: Int} deriving Eq

instance (Ord cx, Ord cy) => Ord (Coord cx cy) where
    c1 <= c2 = c1.cy <= c2.cy || ((c1.cy == c2.cy) && (c1.cx <= c2.cx))


data Carte = Carte {
    cartel :: Int , -- largeur
    carteh :: Int , -- hauteur
    carte_contenu :: (M.Map Coord Case) -- cases de la carte
    }

instance Read Carte where
    readsPrec _ x = [(myReadFile x), "")]


instance Show Carte where
    show = toString




class ToString a where
    toString :: a -> String






toStringCarteAux :: String -> Coord Case -> String
toStringCarteAux n (co ca) = n -- On consière que les retour chariots ne sont pas des caractères
toStringCarteAux n _ = n ++ '\n'


instance ToString Carte where
    toString (Carte c) = foldl (\x -> toStringCarteAux x ) "" c.carte_contenu



