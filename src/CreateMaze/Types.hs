{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
module CreateMaze.Types where

import Data.Array.ST
import Data.Array 
import Data.Word (Word8)
import Data.Array.Unboxed (UArray)
import Data.Map.Strict
import Data.Set

import Data.Binary
import GHC.Generics (Generic)

mwDefault = 531 :: Int
mhDefault = 531 :: Int

maxTest = 1000000 :: Int

creArrFromList :: [a] -> Array Int a
creArrFromList xs = listArray (0,length xs -1) xs


rwList = creArrFromList [9,11,13,15,17] :: Array Int Int
rhList = creArrFromList [9,11,13,15,17] :: Array Int Int



type Maze s = STUArray s (Int,Int) Int
type MazeI  = UArray  (Int,Int) Int

type Number = Int
data RoomAndFloor = Room (Int,Int,Int,Int) (Set (Int,Int))  --Room (i,j,i',j') ,set of ConnectPoint
                  | Floor (Set (Int,Int))  -- Floor, set of ConnectPoint
  deriving (Show,Read,Generic)

instance Binary RoomAndFloor

type RoomMap = Map Number RoomAndFloor
