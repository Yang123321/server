{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Server.Core where

import Server.Types
import Server.MonsterAction
import CreateMaze.Types (MazeI,mwDefault,mhDefault)
import Network.Socket
import Data.Binary

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception

import qualified Data.Map as M

import Control.Monad
import System.IO

import Text.Printf
import Data.Array.IArray ((!),bounds)
import Data.Array.MArray
-- import Data.Array.ST
import Control.Monad.ST

import Transmission.Command
import Data.Time
import qualified Data.List as L
import System.Random

newServer :: IO Server
newServer = do
  c <- newTVarIO M.empty
  mazeI <- decodeFile "data/test/random/-3225345198293634463.mazeI" :: IO MazeI
  let ((i,j),(i',j')) = bounds mazeI
  print "start init masterMzae "

  mMaze <- atomically $ do
    newArray ((i,j),(i',j')) (Elem Null NoBody)

  -- init maze
  forM_ [(pi,pj) | pi <- [i..i'],pj<-[j..j']] $ \p -> do
    if mazeI ! p == 2
      then atomically $ writeArray mMaze p (Elem (Room 2) NoBody)
      else atomically $ writeArray mMaze p (Elem Null NoBody)


  monster1 <- newMonster "monster1" (145,145)
  atomically $ writeArray mMaze (145,145) (Elem (Room 2) (M $ monster1))

  monster2 <- newMonster "monster2" (140,140)
  atomically $ writeArray mMaze (140,140) (Elem (Room 2) (M $ monster2))

  monster3 <- newMonster "monster3" (143,143)
  atomically $ writeArray mMaze (143,143) (Elem (Room 2) (M $ monster2))

  monsters <- newTVarIO [monster1,monster2,monster3]


  return Server {
    clients = c
    , masterMaze = mMaze
  , monsterList = monsters }

port :: Int
port = 45678

tmain :: IO ()
tmain = withSocketsDo $ do
  server@(Server _ m ms) <- newServer

  ms' <- readTVarIO ms
  tim <- getCurrentTime
  createLotsOfMonster (mkStdGen 100) server
  async $ runner server 0 tim

  print "newserver finish"
  addr <- resolve "45678"
  sock <- open addr
  forever $ do
    (sock',infoma) <- accept sock
    print infoma
    handle <- socketToHandle sock' ReadWriteMode
    forkFinally (deal handle server) (\a ->do
                                         hClose handle)
    return ()
 where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        let fd = fdSocket sock
        -- setCloseOnExecIfNeeded fd
        listen sock 10
        return sock

newClient :: Server -> ClientName -> Handle -> STM Client
newClient server@Server{..} name handle = do
  p <- newTVar (150,150)
  bs <- newTVar []
  h <- newTVar 1000
  speed <- newTVar 5
  c <- return Client { clientName   = name
                     , clientHandle = handle
                     , clientPos  = p
                     , clientBullets = bs
                     , clientHealth = h
                     , clientBulletSpeed = speed
                     }
  writeArray masterMaze (150,150) $ Elem (Room 2) (C c) -- make the new to a client
  return c

removeClien :: Server -> Client -> IO ()
removeClien server@Server{..} client@Client{..} = join $ atomically $ do
  clientmap <- readTVar clients
  let t = M.delete clientName clientmap
  writeTVar clients t

  p <- readTVar clientPos
  writeArray masterMaze p $ Elem (Room 2) (NoBody) 

  return $ t `seq` ()
  return $ do
    broadcast server (clientName ++ " leave")
    (print $ "disconnect of: " ++  clientName)

checkAndCline :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAndCline server@Server{..} name handle = join $ atomically $ do
  clientmap <- readTVar clients
  if M.member name clientmap
    then return $ return Nothing
    else do client <- newClient server name handle
            writeTVar clients $ M.insert name client clientmap
            return $ do
              return (Just client)

broadcast :: Server -> String -> IO ()
broadcast server@Server{..} st = do
  clientmap <- atomically $ readTVar clients
  M.traverseWithKey (sendMessage $ show  (Sbroadcast st)) clientmap
  return ()

tell :: Server -> Client -> String -> String -> String -> IO ()
tell server@Server{..} client@Client{..} sname tname st = do
  clientmap <- atomically $ readTVar clients
  case M.lookup tname clientmap  of
    Nothing -> return ()
    Just (Client _ h _ _ _ _) -> do
      hPutStrLn h $ show $ Sbroadcast $ sname ++ " > " ++ tname ++ " : " ++ st
      hPutStrLn clientHandle $ show $ Sbroadcast $ sname ++ " > " ++ tname ++ " : " ++ st


sendMessage ::String ->  ClientName -> Client -> IO ()
sendMessage s _ c@Client{..}  = do
      hPutStrLn clientHandle $ s


deal :: Handle -> Server -> IO ()
deal handle server@Server{..} = do
  hSetBuffering handle LineBuffering

  readName
  where
    readName = do
      name <- hGetLine handle
      case read name of
        CaskForLogin n -> mask $ \restore -> do
          ok <- checkAndCline server n handle
          case ok of
            Nothing -> do
              hPutStrLn handle $ show $ SloginFailed "name is used, you need change one!"
              readName

            Just client -> do
                hPutStrLn handle $ show $ SloginSuccess
                broadcast server (n ++ " joined")
                print $ n ++ " cliented!"
                restore (runClient server client)
                          `finally` (removeClien server client)
        _ -> readName

runClient :: Server-> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do

  -- race sendInfo receiveInfor
  tim <- getCurrentTime
  runConcurrently $ (,,)
    <$> Concurrently (bulletMove tim)
    <*> Concurrently sendInfo
    <*> Concurrently receiveInfor

  return ()

  where

    bulletMove tim = do
      tim' <- getCurrentTime
      let dt = fromRational $ toRational $ diffUTCTime tim' tim
      bullets <- readTVarIO clientBullets
      bulletSpeed <- readTVarIO clientBulletSpeed
      let bullets' = map (\(x,y,vx,vy) -> (x-dt*vy*bulletSpeed,y+dt*vx*bulletSpeed,vx,vy)) bullets

      bullets'' <- filterM checkBullet bullets'

      atomically $ writeTVar clientBullets bullets''

      let delayTime = floor $ 1000 / 100
      threadDelay $ delayTime * 1000

      bulletMove tim'

    checkBullet (x,y,vx,vy) = do
      let (i,j) = (floor x, floor y)
      v <- atomically $ readArray masterMaze (i,j)
      (i0,j0) <- readTVarIO clientPos
      if (i,j) == (i0,j0)
        then return True
        else do
        case v of
          Elem _ (C c@(Client cname _ _ _ heal cs)) -> do
            atomically $ do
              h <- readTVar heal
              speed <- readTVar clientBulletSpeed
              writeTVar heal $ h - (floor $ speed * fromIntegral 4)
            return False
          Elem _ (M (Monster name _ MonsterState{..})) -> do
            atomically $ do
              h <- readTVar monsterHealth
              speed <- readTVar clientBulletSpeed
              writeTVar monsterHealth $ h - (floor $ speed * fromIntegral 4)
            return False
          Elem Null _  -> return False
          Elem OtherStrangeThings _  -> return False
          _ -> return True

    sendInfo = forever $ do
      (i,j) <- readTVarIO clientPos
      let plist = [(x,y) | x <- [i-15..i+15],y <- [j-15..j+15]]

      pelist <- forM plist $ \p -> do
        atomically $ readArray masterMaze p
      let playerList = filter isTranser pelist

      playerList' <- forM playerList elem2tbody
      hPutStrLn clientHandle $ show $ SBodyList playerList'

      heal <- readTVarIO clientHealth
      if heal <=0
        then do
        -- removeClien serv client
        error "disconnect"
        else return ()

      let delayTime = 1000 `div` 20
      threadDelay $ delayTime * 1000



    isTranser :: Elem -> Bool
    isTranser (Elem g (C _)) = True
    isTranser (Elem g (M _)) = True
    isTranser _ = False

    elem2tbody :: Elem -> IO TBody
    elem2tbody (Elem g (C Client{..})) = do
      p <- readTVarIO clientPos
      s <- readTVarIO  clientHealth
      bs <- readTVarIO clientBullets
      return $ TP $ TClient clientName p s (map (\(a,b,c,d)-> (a,b)) bs)

    elem2tbody (Elem g (M (Monster n tvrp (MonsterState h bls)))) = do
      p <- readTVarIO tvrp
      ls <- readTVarIO bls
      h' <- readTVarIO h
      return $ TM (TMonster n h' p (map (\(a,b,c) -> (b,c)) ls))

    elem2tbody _ = error " do not do it, as Server.Core elem2tbody "


    receiveInfor = forever $ do
      msg <- hGetLine clientHandle
      -- print $ clientName ++ " : " ++ msg
      case read msg of
        Coperate Oright -> check (0, 1)
        Coperate Odown ->  check (1, 0)
        Coperate Oleft ->  check (0,-1)
        Coperate Oup ->    check (-1,0)
        Coperate (Shot d) -> addBullet d
        Coperate (BulletSpeedUp s)-> speedUp s
        Coperate (Command s)-> dealCommand s
        _            ->    return ()

    dealCommand st = do
      case words st of
        "tell":tname:ls -> tell serv client clientName tname $ concat $ L.intersperse " "  ls
        "trans":a:b:[] -> do
          let i = read a :: Int
              j = read b :: Int
          orin <- readTVarIO clientPos
          check' orin (i,j)

        ls -> broadcast serv  (clientName ++ " : " ++ st)


    speedUp s = do
      speed <- readTVarIO clientBulletSpeed
      atomically $ writeTVar clientBulletSpeed (speed + s)

    addBullet (dx,dy) = do
      (i,j)<- readTVarIO clientPos
  --                                         0.5 is the origion  of the strange bug
      atomically $ modifyTVar' clientBullets ((fromIntegral i + 0.5, fromIntegral j + 0.5, dx,dy) :)

    check (di,dj) = do
      (i,j) <- readTVarIO clientPos
      let (i',j') = (i+di,j+dj)
      if i'<15 || j' <15 || i' > (mwDefault -16) || j' > (mhDefault - 16)
        then return ()
        else do
        atomically $ do
          elem <- readArray masterMaze (i',j')
          case elem of
            Elem (Room num) NoBody -> do -- you can move to there
              writeTVar clientPos (i',j') -- update clientPos client
              orig <- readArray masterMaze (i,j)
              writeArray masterMaze (i,j) $ changeBody orig NoBody
              writeArray masterMaze (i',j') $ changeBody elem (C client)

            Elem (Floor num) NoBody -> do -- you can move to there
              writeTVar clientPos (i',j') -- update clientPos client
              orig <- readArray masterMaze (i,j)
              writeArray masterMaze (i,j) $ changeBody orig NoBody
              writeArray masterMaze (i',j') $ changeBody elem (C client)

            _ -> return ()


    check' (i,j) (i',j') = do
      if i'<15 || j' <15 || i' > (mwDefault -16) || j' > (mhDefault - 16)
        then return ()
        else do
        atomically $ do
          elem <- readArray masterMaze (i',j')
          case elem of
            Elem (Room num) NoBody -> do -- you can move to there
              writeTVar clientPos (i',j') -- update clientPos client
              orig <- readArray masterMaze (i,j)
              writeArray masterMaze (i,j) $ changeBody orig NoBody
              writeArray masterMaze (i',j') $ changeBody elem (C client)

            Elem (Floor num) NoBody -> do -- you can move to there
              writeTVar clientPos (i',j') -- update clientPos client
              orig <- readArray masterMaze (i,j)
              writeArray masterMaze (i,j) $ changeBody orig NoBody
              writeArray masterMaze (i',j') $ changeBody elem (C client)

            _ -> return ()
