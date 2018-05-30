module Geolocation(Lattice((<:), top, bot), Geolocation(..), geolocations, readGeolocation) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (pack, unpack)
import Data.Aeson.Types (typeMismatch)

class Eq a => Lattice a where
    bot :: a
    top :: a
    (<:) :: a -> a -> Bool

data Geolocation = Toronto | Seoul | Frankfurt | London | CA | KR | GB | BE | BR | DE | EU | HK | IE | JP | NL | Virginia | US | Asia | World | Local deriving (Eq, Show)

geolocations = [Toronto, Seoul, Frankfurt, London, CA, KR, GB, BE, BR, DE, EU, HK, IE, JP, NL, Virginia, US, Asia, World, Local]

readGeolocation :: String -> Either String Geolocation
readGeolocation "Toronto" = Right Toronto
readGeolocation "Seoul" = Right Seoul
readGeolocation "Frankfurt" = Right Frankfurt
readGeolocation "London" = Right London
readGeolocation "CA" = Right CA
readGeolocation "KR" = Right KR
readGeolocation "GB" = Right GB
readGeolocation "BE" = Right BE
readGeolocation "BR" = Right BR
readGeolocation "DE" = Right DE
readGeolocation "EU" = Right EU
readGeolocation "HK" = Right HK
readGeolocation "IE" = Right IE
readGeolocation "JP" = Right JP
readGeolocation "NL" = Right NL
readGeolocation "Virginia" = Right Virginia
readGeolocation "US" = Right US
readGeolocation "Asia" = Right Asia
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
    Frankfurt <: DE = True
    _ <: DE = False

    GB <: GB = True
    London <: GB = True
    _ <: GB = False

    EU <: EU = True
    DE <: EU = True
    GB <: EU = True
    NL <: EU = True
    BE <: EU = True
    IE <: EU = True
    _ <: EU = False

    Asia <: Asia = True
    HK   <: Asia = True
    JP   <: Asia = True
    KR   <: Asia = True
    _    <: Asia = False

    HK <: HK = True
    _ <: HK = False

    KR <: KR = True
    Seoul <: KR = True
    _  <: KR = False

    IE <: IE = True
    _ <: IE = False

    JP <: JP = True
    _ <: JP = False

    NL <: NL = True
    _ <: NL = False

    CA <: CA = True
    Toronto <: CA = True
    _  <: CA = False

    US <: US = True
    Virginia <: US = True
    _ <: US = False

    Frankfurt <: Frankfurt = True
    _         <: Frankfurt = False

    London <: London = True
    _      <: London = False

    Virginia <: Virginia = True
    _        <: Virginia = False

    Seoul <: Seoul = True
    _     <: Seoul = False

    Toronto <: Toronto = True
    _       <: Toronto = False

instance FromJSON Geolocation where
    parseJSON (Aeson.String n) = either fail pure $ readGeolocation $ unpack n
    parseJSON v = typeMismatch "Geolocation" v

instance ToJSON Geolocation where
    toJSON = Aeson.String . pack . show
