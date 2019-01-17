{-# LANGUAGE RecordWildCards  #-}
module Server.Types where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM (TVar)
import Control.Concurrent.Async
import Control.Concurrent.STM.TArray (TArray)
import Data.Map as M

import CreateMaze.Types

import Data.Time
import Control.Monad
import Control.Concurrent.STM.TArray
import Control.Concurrent.STM
import Data.Array.MArray
import Data.List
import System.Random
import qualified Data.List as L


type ClientName = String


data Client = Client
  {
    clientName     :: ClientName
  , clientHandle   :: Handle
  , clientPos    :: TVar (Int,Int)
  , clientBullets :: TVar [(Float,Float,Float,Float)]
  , clientHealth :: TVar Int
  , clientBulletSpeed :: TVar Float
  }

type MasterMaze = TArray (Int,Int) Elem

data Server = Server
  {
    clients :: TVar (M.Map ClientName Client)
  , masterMaze :: MasterMaze
  , monsterList :: TVar [Monster]
  }


data Elem = Elem Ground Body

data Ground = Room Int
            | Floor Int
            | Null
            | OtherStrangeThings

data Body  = C Client
           | Build
           | M Monster
           | NoBody

changeBody :: Elem -> Body -> Elem
changeBody (Elem g b) b' = Elem g b'

changeGround :: Elem -> Ground -> Elem
changeGround (Elem g b) g' = Elem g' b


data MonsterState = MonsterState { monsterHealth :: TVar Int
                                 , monsterBullets :: TVar [(Client,Float,Float)]
                                 } deriving (Eq)

data Monster = Monster { monsterName :: String
                       , monsterPosition :: TVar (Int,Int)
                       , monsterState :: MonsterState
                       } deriving (Eq)

