
module Modele where

import SDL

import Carte (Carte, Coord)
import qualified Carte as C

import Environnement (Entite, Envi)
import qualified Environnement as E

import Keyboard (Keyboard)
import qualified Keyboard as K


data Modele = Modele  { carte :: Carte        -- carte actuelle
                      , envi :: Envi          -- environnement actuel
                      --, gene :: StdGen        -- generateur aleatoire
                      --, log :: String         -- journal du tour
                      --, keyboard :: Keyboard  -- l’etat du clavier 
                      --, win :: Bool           -- gagné !
                      }


instance Show Modele where
    show = toString

class ToString a where
    toString :: a -> String

instance ToString Modele where
    toString modele = (show (carte modele)) ++ "\n\n" ++ (show (envi modele))



initModele :: Carte -> Envi -> Modele
initModele carte envi = Modele carte envi



bouge :: Modele -> Entite -> Coord -> Modele
bouge modele entity coord = Modele (carte modele) (E.bougeById (E.idn entity) coord (envi modele) (carte modele))

prop_bouge_pre :: Modele -> Entite -> Coord -> Bool
prop_bouge_pre modele entity coord = (E.prop_bougeById_pre (E.idn entity) coord (envi modele) (carte modele))

prop_bouge_post :: Modele -> Entite -> Coord -> Bool
prop_bouge_post modele entity coord = (E.prop_bougeById_post (E.idn entity) coord (envi modele) (carte modele))




prop_Modele_inv :: Modele -> Bool
prop_Modele_inv modele = C.prop_Carte_inv (carte modele)
                      && E.prop_Envi_inv (envi modele)




data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int 
                           , virusX :: Int
                           , virusY :: Int
                           , win :: Bool}
  deriving (Eq, Show)

-- x = R.randomRIO(0, 540)
-- y = R.randomRIO(0, 380)
initGameState :: Int -> Int -> GameState
initGameState x y = GameState 0 0 5 x y False

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ sp _ _ _) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ sp _ _ _) | px < 540 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py sp _ _ _) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py sp _ _ _) | py < 380 = gs { persoY = py + sp }
                                | otherwise = gs

checkDead :: GameState -> GameState
checkDead gs@(GameState px py _ vx vy _) 
  | (sqrt (fromIntegral (((px - vx) * (px - vx)) + ((py - vy) * (py - vy)) ))) <= 100 = gs {win = True}
  | otherwise =  gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime 
  | K.keypressed KeycodeZ kbd = moveUp gstate
  | K.keypressed KeycodeQ kbd = moveLeft gstate
  | K.keypressed KeycodeS kbd = moveDown gstate
  | K.keypressed KeycodeD kbd = moveRight gstate
  | otherwise = gstate

{-
moveLeft :: Modele -> Modele
moveLeft gs@(GameState px _ sp _ _ _) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: Modele -> Modele
moveRight gs@(GameState px _ sp _ _ _) | px < 540 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: Modele -> Modele
moveUp gs@(GameState _ py sp _ _ _) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: Modele -> Modele
moveDown gs@(GameState _ py sp _ _ _) | py < 380 = gs { persoY = py + sp }
                                | otherwise = gs

checkDead :: Modele -> Modele
checkDead gs@(GameState px py _ vx vy _) 
  | (sqrt (fromIntegral (((px - vx) * (px - vx)) + ((py - vy) * (py - vy)) ))) <= 100 = gs {win = True}
  | otherwise =  gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime 
  | K.keypressed KeycodeZ kbd = moveUp gstate
  | K.keypressed KeycodeQ kbd = moveLeft gstate
  | K.keypressed KeycodeS kbd = moveDown gstate
  | K.keypressed KeycodeD kbd = moveRight gstate
  | otherwise = gstate




-}


