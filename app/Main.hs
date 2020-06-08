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

import Modele (Modele)
import qualified Modele as M

import System.Random
import qualified System.Random as R


import Carte
import qualified Carte as C

import Moteur
import qualified Moteur as Engine


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

loadPersoKey :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPersoKey rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "persoKey") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "persoKey") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "persoKey") sprite smap
  return (tmap', smap')

loadPersoAttacking :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPersoAttacking rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "persoAttacking") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "persoAttacking") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "persoAttacking") sprite smap
  return (tmap', smap')

loadMonstre :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMonstre rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "monstre") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "monstre") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "monstre") sprite smap
  return (tmap', smap')

loadMonstreAttacking :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMonstreAttacking rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "monstreAttacking") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "monstreAttacking") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "monstreAttacking") sprite smap
  return (tmap', smap')

loadWin :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadWin rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "gagne") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "gagne") (S.mkArea 0 0 500 500)
  let smap' = SM.addSprite (SpriteId "gagne") sprite smap
  return (tmap', smap')

loadLoose :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadLoose rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perdu") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perdu") (S.mkArea 0 0 500 500)
  let smap' = SM.addSprite (SpriteId "perdu") sprite smap
  return (tmap', smap')

loadChestOpened :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadChestOpened rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "chestOpened") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "chestOpened") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "chestOpened") sprite smap
  return (tmap', smap')

loadChestClosed :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadChestClosed rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "chestClosed") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "chestClosed") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "chestClosed") sprite smap
  return (tmap', smap')

loadShadow :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadShadow rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "shadow") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "shadow") (S.mkArea 0 0 1000 1000)
  let smap' = SM.addSprite (SpriteId "shadow") sprite smap
  return (tmap', smap')
  



main :: IO ()
main = do
  initializeAll
  window <- createWindow "PafDungeon" $ defaultWindow { windowInitialSize = V2 500 500 }
  renderer <- createRenderer window (-1) defaultRenderer


  (tmap', smap') <- loadMur renderer "assets/mur.png" TM.createTextureMap SM.createSpriteMap
  (tmap', smap') <- loadSol renderer "assets/sol.png" tmap' smap'
  (tmap', smap') <- loadPorEOFer renderer "assets/porte_fermee_eo.png" tmap' smap'
  (tmap', smap') <- loadPorNSFer renderer "assets/porte_fermee_ns.png" tmap' smap'
  (tmap', smap') <- loadPorEOOuv renderer "assets/porte_ouverte_eo.png" tmap' smap'
  (tmap', smap') <- loadPorNSOuv renderer "assets/porte_ouverte_ns.png" tmap' smap'
  (tmap', smap') <- loadEntrance renderer "assets/entrance.png" tmap' smap'
  (tmap', smap') <- loadExit renderer "assets/exit.png" tmap' smap'
  (tmap', smap') <- loadWin renderer "assets/win.png" tmap' smap'
  (tmap', smap') <- loadLoose renderer "assets/loose.png" tmap' smap'
  (tmap', smap') <- loadChestOpened renderer "assets/chest_opened.png" tmap' smap'
  (tmap', smap') <- loadChestClosed renderer "assets/chest_closed.png" tmap' smap'
    -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap' smap'
  (tmap', smap') <- loadPersoKey renderer "assets/perso_key.png" tmap' smap'
  (tmap', smap') <- loadPersoAttacking renderer "assets/persoAttacking.png" tmap' smap'
  -- chargement du virus
  (tmap', smap') <- loadMonstre renderer "assets/monstre.png" tmap' smap'
  (tmap', smap') <- loadMonstreAttacking renderer "assets/monstreAttacking.png" tmap' smap'

  (tmap', smap') <- loadShadow renderer "assets/brouillard.png" tmap' smap'
  
  -- initialisation de l'état du jeu
  --x <- R.randomRIO(0, 540)
  --y <- R.randomRIO(0, 380)
  --let gameState = M.initGameState x y 
  strcarte <- (readFile $ "maps/map1.txt")
  strmobs <- (readFile $ "maps/mob1.txt")


  let engineState = Tour 0 (M.initModele (read strcarte) (E.createEnvi (read strcarte) strmobs)) ""
-- y = R.randomRIO(0, 380)
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop


  --putStrLn (show (listFromCarte (M.carte modele)))    --debug : affiche la map de la carte
  putStrLn (show (M.carte (Engine.modele engineState)))
  putStrLn ("Invariants du Modele : Carte + Environnement : " ++ (show (M.prop_Modele_inv (Engine.modele engineState))) ++ "\n")
  --Juste pour voir si avant de commencer, le modele est sain


 
  --putStrLn ("doorsSurroundedByWalls_inv : " ++ (show ((read texte))))

  gameLoop 60 renderer tmap' smap' kbd engineState (read strcarte) (E.createEnvi (read strcarte) strmobs)

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


gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Etat -> Carte -> Envi -> IO ()
gameLoop frameRate renderer tmap smap kbd state carte env = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer
  case state of
    Engine.Gagne -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "gagne") smap) 0 0)
    Engine.Perdu -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perdu") smap) 0 0)
    _ -> (do
          putStrLn (show (E.listEntities (M.envi (Engine.modele state))))
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
            (C.listFromCarte (M.carte (Engine.modele state))) 


          mapM_ ( \ (co, entites) -> (do
            let x = (fromIntegral (50 * (cx co))) 
            let y = (fromIntegral (50 * (cy co)))

            mapM_ ( \ entity -> (do
              case entity of
                E.Joueur _ _ _ _ False False ->S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap) x y)
                E.Joueur _ _ _ _ False True ->S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "persoKey") smap) x y)
                E.Joueur _ _ _ _ True _ -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "persoAttacking") smap) x y)
                E.Joueur _ _ _ _ False True ->S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap) x y)
                E.Monstre _ _ _ _ False -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "monstre") smap) x y)
                E.Monstre _ _ _ _ True -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "monstreAttacking") smap) x y)
                E.Coffre _ _ True -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "chestOpened") smap) x y)
                E.Coffre _ _ False -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "chestClosed") smap) x y)
                --_ -> S.displaySprite renderer tmap S.createEmptySprite
                )) entites

            )) (E.listFromEnv (M.envi (Engine.modele state)))
          
            
          case E.getPlayerCoord (M.envi (Engine.modele state)) of
            --(Just c) -> ( ((fromIntegral ((cx c)) - 475)) , (fromIntegral ((cy c) - 475)) )
            --_ -> return ()


            (Just c) -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "shadow") smap) (fromIntegral ((50 * (cx c)) - 475)) (fromIntegral ((50 * (cy c)) - 475)) )
            _ -> return ()
          
          --putStrLn position_perso


          )
          
   
          
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ 100000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  --putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  --putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --putStrLn $ "Refresh time: " <> (show (1 / refreshTime)) <> " (ms)"
  --- update du game state
    
  let newState = case state of
        Engine.Gagne ->  (if (K.keypressed KeycodeR kbd') then Tour 0 (M.initModele carte env) "" else state)
        Engine.Perdu -> (if (K.keypressed KeycodeR kbd') then Tour 0 (M.initModele carte env) "" else state)
        _ -> Engine.etat_tour state kbd' deltaTime

  
    
  
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' newState carte env)


