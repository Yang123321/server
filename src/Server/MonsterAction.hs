{-# LANGUAGE RecordWildCards  #-}
module Server.MonsterAction where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM (TVar)
import Control.Concurrent.Async
import Control.Concurrent.STM.TArray (TArray)
import qualified Data.Map as M

import CreateMaze.Types

import Data.Time
import Control.Monad
import Control.Concurrent.STM.TArray
import Control.Concurrent.STM
import Data.Array.MArray
import Data.List
import System.Random
import qualified Data.List as L
import Server.Types
import Data.Maybe (isJust,fromJust)
import Control.Exception

newMonster :: String -> (Int,Int) -> IO Monster
newMonster name pos= do
  p <- newTVarIO pos
  h <- newTVarIO 1000
  mbs <- newTVarIO []
  return $ Monster { monsterName = name
                   , monsterPosition = p
                   , monsterState = MonsterState {
                         monsterHealth = h
                       , monsterBullets = mbs
                       }
                   }

createLotsOfMonster :: StdGen -> Server-> IO ()
createLotsOfMonster gen serv@Server{..} = do
  forM_ randPosList $ \(n,i,j) -> do
    v <- atomically $ readArray masterMaze (i,j)
    case v of
      Elem (Server.Types.Room _ ) NoBody -> do
        monster <- newMonster ("monster" ++ show n) (i,j)
        atomically $ writeArray masterMaze (i,j) $ changeBody v (M monster)
        atomically $ modifyTVar' monsterList (monster:)
      Elem (Server.Types.Floor _ ) NoBody -> do
        monster <- newMonster ("monster" ++ show n) (i,j)
        atomically $ writeArray masterMaze (i,j) $ changeBody v (M monster)
        atomically $ modifyTVar' monsterList (monster:)
      _ -> return ()

  where (g1,g2) = split gen
        randPosList = take 500 $ zip3 [1..] (randomRs (1,500) g1) (randomRs (1,500) g2) :: [(Int,Int,Int)]



type Dtime = Float

runner ::Server ->  Int -> UTCTime -> IO ()
runner serv@Server{..} counter tim = do
  tim' <- getCurrentTime
  let dt = fromRational $ toRational $ diffUTCTime tim' tim
  monsterList' <- readTVarIO monsterList

  forM_ monsterList' $ \m -> do
    test1Action1 serv m counter dt

  if length monsterList' < 400
    then do
    g <- randomIO
    createLotsOfMonster (mkStdGen g) serv
    else return ()

  let delayTime = floor $ 1000 / 60
  threadDelay $ delayTime * 1000

  runner serv (counter+1) tim'


monsterMove :: MasterMaze -> Monster -> (Int,Int) -> IO ()
monsterMove masterMaze monster@Monster{..} (di,dj) = do
  (i,j) <- readTVarIO monsterPosition
  let pos = (i+di,j+dj)
  v <- atomically $ readArray masterMaze pos
  case v of
    Elem (Server.Types.Room _ ) NoBody -> do
      atomically $ do
        writeTVar monsterPosition pos
        orig <- readArray masterMaze (i,j)
        writeArray masterMaze (i,j) $ changeBody orig NoBody
        writeArray masterMaze pos $ changeBody orig $ M monster
    Elem (Server.Types.Floor _ ) NoBody -> do
      atomically $ do
        writeTVar monsterPosition pos
        orig <- readArray masterMaze (i,j)
        writeArray masterMaze (i,j) $ changeBody orig NoBody
        writeArray masterMaze pos $ changeBody orig $ M monster
    _ -> return ()

randomMonsterMove :: MasterMaze -> Monster -> IO ()
randomMonsterMove masterMaze monster= do
    number <- randomRIO (1,4) :: IO Int
    case number of
      1 -> monsterMove masterMaze monster (0,1)
      2 -> monsterMove masterMaze monster (1,0)
      3 -> monsterMove masterMaze monster (0,-1)
      4 -> monsterMove masterMaze monster (-1,0)

monsterBulletCheckShotin :: MasterMaze -> (Client,Float,Float) -> IO (Maybe (Client,Float,Float))
monsterBulletCheckShotin masterMaze (c,si,sj) = do
  (i,j) <- readTVarIO $ clientPos c
  cv <- atomically $ readArray masterMaze (i,j)
  case cv of
    Elem _ NoBody -> return Nothing
    _ -> do
      v <- atomically $ readArray masterMaze (floor si,floor sj)
      case v of
        Elem _ (C (Client cname _ _ _ heal cs)) -> do
          atomically $ do
            h <- readTVar heal
            let t = h-10
            writeTVar heal t
            return Nothing
        Elem Null _  -> return Nothing
        Elem OtherStrangeThings _  -> return Nothing
        _ -> return $ Just (c,si,sj)

monsterBulletsUpdate :: MasterMaze -> [(Client,Float,Float)] -> Dtime -> IO [(Client,Float,Float)]
monsterBulletsUpdate masterMaze ls dt = do
  r <-  forM ls $ \(c,i,j) -> do
    res <- readTVarIO $ clientPos c -- :: IO (Either SomeException (Int,Int))
    case (Right res) of
      Left e -> do
        return Nothing
      Right (ci',cj') -> do
        let (ci,cj) = (fromIntegral ci' + 0.5, fromIntegral cj' + 0.5)
            (di,dj) = (ci-i,cj-j)
            distance = sqrt $ di ^ 2 + dj^2
            (diri,dirj) = (di/distance,dj/distance)
            (si,sj) = (i+7*diri*dt,j+7*dirj*dt) -- can change the bullet speed
        monsterBulletCheckShotin masterMaze (c,si,sj)
  return $ L.map fromJust $ L.filter (isJust) r



monsterShotFllowBullets :: MasterMaze -> Monster  -> Int -> Dtime -> IO ()
monsterShotFllowBullets masterMaze  monster@Monster{..} counter dt = do
  posos@(i,j) <- readTVarIO monsterPosition
  aroundUser <- foldM isUser [] (around (i,j))
  monsterlol <- readTVarIO $ monsterBullets monsterState

  let monsterBullets' = if counter `mod` 15 == 0  --- can change the bullet frequence
                        then Data.List.foldl' (add (i,j)) monsterlol aroundUser 
                        else monsterlol

  monsterBullets'' <- monsterBulletsUpdate masterMaze monsterBullets' dt
  atomically $ writeTVar (monsterBullets monsterState) monsterBullets''

  where around (i,j) = [(i,j) | i <- [i-5..i+5] , j<- [j-5..j+5]]
        add (i,j) ls c =(c,fromIntegral i + 0.5,fromIntegral j + 0.5) : ls
        isUser ls (i,j) = do
          if i<0 || j<0 || i>=mwDefault || j >= mhDefault
            then return ls
            else do
            v <- atomically $ readArray masterMaze (i,j)
            case v of
              Elem _ (C c) ->return $ c:ls
              _ -> return ls

monsterCheckAlive  :: Server -> Monster -> IO ()
monsterCheckAlive Server{..} monster@Monster{..} = do
  posos@(i,j) <- readTVarIO monsterPosition
  heal <- readTVarIO $ monsterHealth monsterState
  if heal > 0
    then return () :: IO ()
    else do
    atomically $ do
      modifyTVar' monsterList (L.delete monster)
      e <- readArray masterMaze posos
      writeArray masterMaze posos $ changeBody e NoBody



test1Action1 :: Server -> Monster  -> Int -> Dtime -> IO ()
test1Action1 serv@Server{..} monster@Monster{..} counter dt = do
  monsterShotFllowBullets masterMaze monster counter dt
  if counter `mod` 10 /= 0 -- can chage the random move frequence
    then return ()
    else randomMonsterMove masterMaze monster
  monsterCheckAlive serv monster


