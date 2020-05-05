{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )


import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Behavior (GameState)
import qualified Behavior as M

import System.Random
import qualified System.Random as R


import Carte
import qualified Carte as C


loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 100 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadVirus :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVirus rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "virus") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "virus") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "virus") sprite smap
  return (tmap', smap')

loadDVirus :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadDVirus rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "Deadvirus") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Deadvirus") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "Deadvirus") sprite smap
  return (tmap', smap')


loadMur :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMur rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "mur") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "mur") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "mur") sprite smap
  return (tmap', smap')

loadSol :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSol rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "sol") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "sol") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "sol") sprite smap
  return (tmap', smap')



main :: IO ()
main = do
  initializeAll
  window <- createWindow "PafDungeon" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.jpg" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap smap
  -- chargement du virus
  (tmap', smap') <- loadVirus renderer "assets/virus.png" tmap' smap'
  (tmap', smap') <- loadDVirus renderer "assets/Deadvirus.png" tmap' smap'

  (tmap', smap') <- loadMur renderer "assets/mur.png" tmap' smap'
  (tmap', smap') <- loadSol renderer "assets/sol.png" tmap' smap'
  
  -- initialisation de l'état du jeu
  x <- R.randomRIO(0, 540)
  y <- R.randomRIO(0, 380)
  --let gameState = M.initGameState x y (read (TIO.readFile $ "texte.txt"))
  texte <- (readFile $ "map1.txt")
  let modele = B.initModele (read texte)
-- y = R.randomRIO(0, 380)
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap' smap' kbd modele

displayObjectAsSprite :: C.Case -> Int -> Int -> IO ()
displayObjectAsSprite objet x y = case objet of 
  C.Normal  -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "sol") smap) x y)
  _         -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "mur") smap) x y)

displayCarteAux :: () -> (k, a) -> IO ()
displayCarteAux _ (C.Coord co, ca) = displayObjectAsSprite ca (x co) (y co)

displayCarte :: C.Carte -> IO ()
displayCarte carte = foldr displayCarteAux () (assocs (carte_contenu carte))


gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> M.Modele -> IO ()
gameLoop frameRate renderer tmap smap kbd modele = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer

  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)


  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral (M.persoX gameState))
                                 (fromIntegral (M.persoY gameState)))
  --- display virus

  if (M.win gameState)
    then S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "Deadvirus") smap)
                                 (fromIntegral (M.virusX gameState))
                                 (fromIntegral (M.virusY gameState)))
    else S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "virus") smap)
                                 (fromIntegral (M.virusX gameState))
                                 (fromIntegral (M.virusY gameState)))


  displayCarte (carte modele)


  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep (M.checkDead gameState) kbd' deltaTime
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState')
