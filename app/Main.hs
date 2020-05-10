{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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


import Environnement
import qualified Environnement as E



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

loadPorEOFer :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorEOFer rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "porte_fermee_eo") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "porte_fermee_eo") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "porte_fermee_eo") sprite smap
  return (tmap', smap')

loadPorEOOuv :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorEOOuv rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "porte_ouverte_eo") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "porte_ouverte_eo") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "porte_ouverte_eo") sprite smap
  return (tmap', smap')

loadPorNSFer :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorNSFer rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "porte_fermee_ns") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "porte_fermee_ns") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "porte_fermee_ns") sprite smap
  return (tmap', smap')

loadPorNSOuv :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorNSOuv rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "porte_ouverte_ns") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "porte_ouverte_ns") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "porte_ouverte_ns") sprite smap
  return (tmap', smap')

loadEntrance :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadEntrance rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "entrance") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "entrance") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "entrance") sprite smap
  return (tmap', smap')

loadExit :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadExit rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "exit") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "exit") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "exit") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadVirus :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVirus rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "virus") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "virus") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "virus") sprite smap
  return (tmap', smap')

  



main :: IO ()
main = do
  initializeAll
  window <- createWindow "PafDungeon" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer


  (tmap', smap') <- loadMur renderer "assets/mur.png" TM.createTextureMap SM.createSpriteMap
  (tmap', smap') <- loadSol renderer "assets/sol.png" tmap' smap'
  (tmap', smap') <- loadPorEOFer renderer "assets/porte_fermee_eo.png" tmap' smap'
  (tmap', smap') <- loadPorNSFer renderer "assets/porte_fermee_ns.png" tmap' smap'
  (tmap', smap') <- loadPorEOOuv renderer "assets/porte_ouverte_eo.png" tmap' smap'
  (tmap', smap') <- loadPorNSOuv renderer "assets/porte_ouverte_ns.png" tmap' smap'
  (tmap', smap') <- loadEntrance renderer "assets/entrance.png" tmap' smap'
  (tmap', smap') <- loadExit renderer "assets/exit.png" tmap' smap'
    -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap' smap'
  -- chargement du virus
  (tmap', smap') <- loadVirus renderer "assets/virus.png" tmap' smap'
  
  -- initialisation de l'état du jeu
  x <- R.randomRIO(0, 540)
  y <- R.randomRIO(0, 380)
  let gameState = M.initGameState x y 
  strcarte <- (readFile $ "maps/map1.txt")
  strmobs <- (readFile $ "maps/mob1.txt")
  let modele = M.initModele (read strcarte) (E.setEntity (E.entiteFromChar 'M' 1) (Coord 2 1) (E.setEntity (E.entiteFromChar 'J' 0) (Coord 1 1) (E.createEnvi (read strcarte) strmobs)))
-- y = R.randomRIO(0, 380)
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop


  --putStrLn (show (listFromCarte (M.carte modele)))    --debug : affiche la map de la carte
  putStrLn (show (M.carte modele))
  putStrLn (show (E.prop_oneUncrossableMobPerCase_inv (M.envi modele)))    --debug : affiche la map de l'environnement
 
  --putStrLn ("doorsSurroundedByWalls_inv : " ++ (show ((read texte))))

  gameLoop 60 renderer tmap' smap' kbd gameState modele

{-
data RendererInfos = RendererInfos  { renderer :: Renderer
                                    , tmap :: TextureMap
                                    , smap :: SpriteMap }

initRendererInfos :: Renderer -> TextureMap -> SpriteMap -> RendererInfos
initRendererInfos renderer tmap smap = RendererInfos renderer tmap smap



displayObjectAsSprite :: RendererInfos -> C.Case -> CInt -> CInt -> RendererInfos
displayObjectAsSprite r objet x y = do
  startTime <- time
  perdu <- case objet of 
    C.Normal  -> S.displaySprite (renderer r) (tmap r) (S.moveTo (SM.fetchSprite (SpriteId "sol") (smap r)) x y)
    _         -> S.displaySprite (renderer r) (tmap r) (S.moveTo (SM.fetchSprite (SpriteId "mur") (smap r)) x y)
  return r

displayCarteAux :: RendererInfos -> (Coord, Case) -> RendererInfos
displayCarteAux renderer (co, ca) = displayObjectAsSprite renderer ca (fromIntegral (cx co)) (fromIntegral (cy co)) -- >>= ( \ _ -> renderer)

displayCarte :: RendererInfos -> C.Carte -> RendererInfos
displayCarte renderer carte = foldr displayCarteAux renderer (Map.assocs (carte_contenu carte))

-}


gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> M.Modele -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState modele = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer

  


  -- displayCarte (initRendererInfos renderer tmap smap) (carte modele)

  {-
  mapM_ (\(co, ca) -> (case ca of
          C.Normal  -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "sol") smap) (fromIntegral (50 * (cx co))) (fromIntegral (50 * (cy co))))
          _         -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "mur") smap) (fromIntegral (50 * (cx co))) (fromIntegral (50 * (cy co))))
          ))
    (Map.assocs (carte_contenu (M.carte modele))) -- est ce que c'est vraiment M.Carte ?
  -}
  
  mapM_ ( \ (co, ca) -> (do
    let x = (fromIntegral (50 * (cx co))) 
    let y = (fromIntegral (50 * (cy co)))
    case ca of
          C.Normal              -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "sol") smap) x y)
          C.Mur                 -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "mur") smap) x y)
          C.Porte NS Ouverte  -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "porte_ouverte_ns") smap) x y)
          C.Porte NS Fermee   -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "porte_fermee_ns") smap) x y)
          C.Porte EO Ouverte  -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "porte_ouverte_eo") smap) x y)
          C.Porte EO Fermee   -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "porte_fermee_eo") smap) x y)
          C.Entree              -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "entrance") smap) x y)
          C.Sortie              -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "exit") smap) x y)
          _                     -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "mur") smap) x y)
    ))
    (C.listFromCarte (M.carte modele)) 



  mapM_ ( \ (co, entites) -> (do
    let x = (fromIntegral (50 * (cx co))) 
    let y = (fromIntegral (50 * (cy co)))

    mapM_ ( \ entity -> (do
      case entity of
        E.Joueur _ _ _ _ -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap) x y)
        E.Monstre _ _ _ _ -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "virus") smap) x y)
        --_ -> S.displaySprite renderer tmap S.createEmptySprite
        )) entites

    )) (E.listFromEnv (M.envi modele)) 

  
  

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
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' modele)


