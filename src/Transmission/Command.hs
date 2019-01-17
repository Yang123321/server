module Transmission.Command where

import Server.Types
import Data.Time


data ServerToClient = SloginSuccess
                    | SloginFailed String -- Failed reason
                    | SlogoutSuccess ClientName
                    | Sposition (Int,Int)

                    | SBodyList [TBody]

                    | Sbroadcast String

                    | StestPing UTCTime

                    deriving (Show,Read)

data TBody = TP TClient
           | TM TMonster
           deriving (Show,Read)
-----------------------name        position health  bullets
data TClient = TClient ClientName (Int,Int) Int   [(Float,Float)] deriving (Show,Read)
data TMonster = TMonster String Int (Int,Int) [(Float,Float)]  -- pos  blockPos don't care v
  deriving (Show,Read)
--server ---
-- receive  -- Client To Server  parse start as C
-- send     -- Server To Client  pack start as S
------

--client ---
-- receive  -- Server To Client parse start as S
-- send     -- Client To Server pack start as C
-----

data ClientToServer = CaskForLogin ClientName
                    | CaskForLogout ClientName

                    | Coperate Operate

                    | CtestPing UTCTime
                    deriving (Show,Read)


data Operate = Oright
             | Odown
             | Oleft
             | Oup
             | Shot (Float,Float) --direct
             | BulletSpeedUp Float
             | Command String
             deriving (Show,Read)


