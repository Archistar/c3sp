{-# LANGUAGE TemplateHaskell #-}

module ServerConfig
    ( ServerConfOut(..)
    , ServerOut(..)
    , toServerConfOut
    , addCost
    , sumCost
    , Server(..)
    , ServerConf(..)
    , Cost
    , CostP
    , exceedsLimit
    , noCost
    , UID
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Scientific

import Geolocation
import Util

type UID = Int

data Cost = Cost
    { costPerGBStorage           :: Scientific
    , costPerGBInboundTraffic    :: Scientific
    , costPerGBOutboundTraffic   :: Scientific
    , costPerGBOutboundBandwidth :: Scientific
    , costPerGETRequest          :: Scientific
    , costPerPUTRequest          :: Scientific
    , costPerPOSTRequest         :: Scientific
    , costPerLISTRequest         :: Scientific
    , costPerUser                :: Scientific
    } deriving (Show, Eq)

data CostP = CostP
    { costPerGBStorageP           :: Maybe Scientific
    , costPerGBInboundTrafficP    :: Maybe Scientific
    , costPerGBOutboundTrafficP   :: Maybe Scientific
    , costPerGBOutboundBandwidthP :: Maybe Scientific
    , costPerGETRequestP          :: Maybe Scientific
    , costPerPUTRequestP          :: Maybe Scientific
    , costPerPOSTRequestP         :: Maybe Scientific
    , costPerLISTRequestP         :: Maybe Scientific
    , costPerUserP                :: Maybe Scientific
    } deriving (Show, Eq)

data Server = Server
    { servId               :: UID
    , servName             :: String
    , servServerLocation   :: Geolocation
    , servAccessLocation   :: Geolocation
    , servAvailability     :: Scientific
    , servEncryptedStorage :: String
    , servCost             :: Cost
    } deriving (Show, Eq)

data ServerConf = ServerConf
    { scAvailability :: Scientific
    , scCostPerMonth :: Cost
    , scServers      :: [Server]
    , scK            :: Integer
    } deriving Show

data ServerOut = ServerOut
    { soName :: String
    } deriving (Show, Eq)

data ServerConfOut = ServerConfOut
    { scoServers      :: [ServerOut]
    } deriving Show

deriveJSON defaultOptions ''Cost
deriveJSON defaultOptions { fieldLabelModifier = init } ''CostP
deriveJSON defaultOptions { fieldLabelModifier = dropPrefix 4 } ''Server
deriveJSON defaultOptions { fieldLabelModifier = dropPrefix 2 } ''ServerConf
deriveJSON defaultOptions { fieldLabelModifier = dropPrefix 2 } ''ServerOut
deriveJSON defaultOptions { fieldLabelModifier = dropPrefix 3 } ''ServerConfOut

instance Eq ServerConf where
    (ServerConf a1 b1 c1 d1) == (ServerConf a2 b2 c2 d2)
        = a1 == a2 && b1 == b2 && d1 == d2 && noOrdEq c1 c2

fromList :: [Scientific] -> Cost
fromList [x1,x2,x3,x4,x5,x6,x7,x8,x9] = Cost x1 x2 x3 x4 x5 x6 x7 x8 x9
fromList _ = error "Exactly nine costs are needed to construct the Cost type."

fromListP :: [Maybe Scientific] -> CostP
fromListP [x1,x2,x3,x4,x5,x6,x7,x8,x9] = CostP x1 x2 x3 x4 x5 x6 x7 x8 x9
fromListP _ = error "Exactly nine costs are needed to construct the CostP type."

toList :: Cost -> [Scientific]
toList (Cost x1 x2 x3 x4 x5 x6 x7 x8 x9) = [x1,x2,x3,x4,x5,x6,x7,x8,x9]

toListP :: CostP -> [Maybe Scientific]
toListP (CostP x1 x2 x3 x4 x5 x6 x7 x8 x9) = [x1,x2,x3,x4,x5,x6,x7,x8,x9]

noCost :: Cost
noCost = fromList $ map (const 0) [1..9]

exceedsLimit :: CostP -> Cost -> Bool
exceedsLimit cp c = or $ zipWith mybL (toListP cp) (toList c)
    where mybL x y = maybe False (y >) x

addCost :: Cost -> Cost -> Cost
addCost c1 c2 = fromList $ zipWith (+) (toList c1) (toList c2)

sumCost :: [Cost] -> Cost
sumCost = foldl addCost noCost

toServerOut :: Server -> ServerOut
toServerOut = ServerOut . servName

toServerConfOut :: ServerConf -> ServerConfOut
toServerConfOut = ServerConfOut . map toServerOut . scServers
