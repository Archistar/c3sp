module Geolocation(Lattice((<:), top, bot), Geolocation(..), geolocations, readGeolocation) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (pack, unpack)
import Data.Aeson.Types (typeMismatch)

class Eq a => Lattice a where
    bot :: a
    top :: a
    (<:) :: a -> a -> Bool

data Geolocation = BE | BR | DE | EU | HK | IE | JP | NL | US | World | Local deriving (Eq, Show)

geolocations = [BE, BR, DE, EU, HK, IE, JP, NL, US, World, Local]

readGeolocation :: String -> Either String Geolocation
readGeolocation "BE" = Right BE
readGeolocation "BR" = Right BR
readGeolocation "DE" = Right DE
readGeolocation "EU" = Right EU
readGeolocation "HK" = Right HK
readGeolocation "IE" = Right IE
readGeolocation "JP" = Right JP
readGeolocation "NL" = Right NL
readGeolocation "US" = Right US
readGeolocation "World" = Right World
readGeolocation "Local" = Right Local
readGeolocation s = Left $ "Given String \"" ++ s ++ "\" can not be interpreted as a Geolocation."

instance Lattice Geolocation where
    bot = Local
    top = World

    Local <: Local = True
    World <: World = True

    Local <: _ = True
    _ <: Local = False
    _ <: World = True
    World <: _ = False

    BE <: BE = True
    _ <: BE = False

    BR <: BR = True
    _ <: BR = False

    DE <: DE = True
    _ <: DE = False

    EU <: EU = True
    DE <: EU = True
    NL <: EU = True
    BE <: EU = True
    IE <: EU = True
    _ <: EU = False

    HK <: HK = True
    _ <: HK = False

    IE <: IE = True
    _ <: IE = False

    JP <: JP = True
    _ <: JP = False

    NL <: NL = True
    _ <: NL = False

    US <: US = True
    _ <: US = False

instance FromJSON Geolocation where
    parseJSON (Aeson.String n) = either fail pure $ readGeolocation $ unpack n
    parseJSON v = typeMismatch "Geolocation" v

instance ToJSON Geolocation where
    toJSON = Aeson.String . pack . show
