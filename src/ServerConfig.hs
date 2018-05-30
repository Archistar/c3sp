{-# LANGUAGE TemplateHaskell #-}

module ServerConfig
    ( ServerConfOut(..)
    , ServerOut(..)
    , toServerConfOut
    , addCost
    , scaleCost
    , sumCost
    , Server(..)
    , ServerConf(..)
    , Cost(..)
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
    { costPerGBStorage         :: Scientific
    , costPerGBInboundTraffic  :: Scientific
    , costPerGBOutboundTraffic :: Scientific
    , costPerGetOperation      :: Scientific
    , costPerInsertOperation   :: Scientific
    , costPerDeleteOperation   :: Scientific
    , costPerListOperation     :: Scientific
    , costPerUser              :: Scientific
    } deriving (Show, Eq)

data CostP = CostP
    { costPerGBStorageP         :: Maybe Scientific
    , costPerGBInboundTrafficP  :: Maybe Scientific
    , costPerGBOutboundTrafficP :: Maybe Scientific
    , costPerGetOperationP      :: Maybe Scientific
    , costPerInsertOperationP   :: Maybe Scientific
    , costPerDeleteOperationP   :: Maybe Scientific
    , costPerListOperationP     :: Maybe Scientific
    , costPerUserP              :: Maybe Scientific
    } deriving (Show, Eq)

data Server = Server
    { servId                 :: UID
    , servName               :: String
    , servServerLocation     :: Geolocation
    , servEncryptedStorage   :: String
    , servAvailability       :: Scientific
    , servAccessLocation     :: Geolocation
    , servCost               :: Cost
    , servDelayedFirstByte   :: Bool
    , servMinStorageDuration :: Int
    , servNote               :: String
    } deriving (Show, Eq)

data ServerConf = ServerConf
    { scK                  :: Int
    , scN                  :: Int
    , scLoc                :: Geolocation
    , scAvailability       :: Scientific
    , scCostPerMonth       :: Cost
    , scDelayedFirstByte   :: Bool
    , scMinStorageDuration :: Int
    , scServers            :: [Server]
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
    (ServerConf a1 b1 c1 d1 e1 f1 g1 h1) == (ServerConf a2 b2 c2 d2 e2 f2 g2 h2)
        = a1 == a2 && b1 == b2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2
          && noOrdEq h1 h2

fromList :: [Scientific] -> Cost
fromList [x1,x2,x3,x4,x5,x6,x7,x8] = Cost x1 x2 x3 x4 x5 x6 x7 x8
fromList _ = error "Exactly nine costs are needed to construct the Cost type."

fromListP :: [Maybe Scientific] -> CostP
fromListP [x1,x2,x3,x4,x5,x6,x7,x8] = CostP x1 x2 x3 x4 x5 x6 x7 x8
fromListP _ = error "Exactly nine costs are needed to construct the CostP type."

toList :: Cost -> [Scientific]
toList (Cost x1 x2 x3 x4 x5 x6 x7 x8) = [x1,x2,x3,x4,x5,x6,x7,x8]

toListP :: CostP -> [Maybe Scientific]
toListP (CostP x1 x2 x3 x4 x5 x6 x7 x8) = [x1,x2,x3,x4,x5,x6,x7,x8]

noCost :: Cost
noCost = fromList $ map (const 0) [1..8]

exceedsLimit :: CostP -> Cost -> Bool
exceedsLimit cp c = or $ zipWith mybL (toListP cp) (toList c)
    where mybL x y = maybe False (y >) x

addCost :: Cost -> Cost -> Cost
addCost c1 c2 = fromList $ zipWith (+) (toList c1) (toList c2)

scaleCost :: Scientific -> Cost -> Cost
scaleCost a = fromList . map (*a) . toList

sumCost :: [Cost] -> Cost
sumCost = foldl addCost noCost

toServerOut :: Server -> ServerOut
toServerOut = ServerOut . servName

toServerConfOut :: ServerConf -> ServerConfOut
toServerConfOut = ServerConfOut . map toServerOut . scServers
