
module Behavior where

import SDL


import System.Random
import qualified System.Random as R


import Carte (Carte)
import qualified Carte as C

import Environnement
import qualified Environnement as E

import System.IO.Unsafe -- HAHA


import Keyboard (Keyboard)
import qualified Keyboard as K

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

data Modele = Modele  { carte :: C.Carte        -- carte actuelle
                    , envi :: E.Envi          -- environnement actuel
                    --, gene :: StdGen        -- generateur aleatoire
                    --, log :: String         -- journal du tour
                    --, keyboard :: Keyboard  -- l’etat du clavier 
                    --, win :: Bool           -- gagné !
                    }

initModele :: C.Carte -> Envi -> Modele
initModele carte envi = Modele carte envi

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
  | (sqrt (fromIntegral (((px - vx) * (px - vx)) + ((py - vy) * (py - vy)) ))) <= 100 = gs {win = True}
  | otherwise =  gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime 
  | K.keypressed KeycodeZ kbd = moveUp gstate
  | K.keypressed KeycodeQ kbd = moveLeft gstate
  | K.keypressed KeycodeS kbd = moveDown gstate
  | K.keypressed KeycodeD kbd = moveRight gstate
  | otherwise = gstate
