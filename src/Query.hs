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
    { qLK            :: Maybe Int
    , qUK            :: Maybe Int
    , qK             :: Maybe Int
    , qLN            :: Maybe Int
    , qUN            :: Maybe Int
    , qN             :: Maybe Int
    , qLimit         :: Maybe Int
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
