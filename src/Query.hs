{-# LANGUAGE TemplateHaskell #-}

module Query
    ( Query(..)
    , QueryOptions(..)
    , MultOption(..)
    , Order(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Scientific

import ServerConfig
import Geolocation
import Util

data Order = ByAvail | ByPerGBStorage deriving (Show, Eq)

data MultOption = MultOption
    { moptId :: UID
    , moptM  :: Int
    } deriving (Show, Eq)

data QueryOptions = QueryOptions
    { optMultOption  :: Maybe [MultOption]
    , optTrustedList :: Maybe [UID]
    } deriving (Show, Eq)

data Query = Query
    { qK             :: Maybe Int
    , qN             :: Maybe Int
    , qLoc           :: Maybe Geolocation
    , qAvail         :: Maybe Scientific
    , qCost          :: Maybe CostP
    , qDelay         :: Maybe Bool
    , qMinDur        :: Maybe Int
    , qOrder         :: Maybe Order
    , qOptions       :: Maybe QueryOptions
    , qMaxPerCompany :: Maybe Int
    } deriving (Show, Eq)

deriveJSON defaultOptions ''Order
deriveJSON defaultOptions { fieldLabelModifier = dropPrefix 4 } ''MultOption
deriveJSON defaultOptions { fieldLabelModifier = dropPrefix 3 } ''QueryOptions
deriveJSON defaultOptions { fieldLabelModifier = dropPrefix 1 } ''Query
