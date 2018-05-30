module ConfigurationHandler(stdinValidateQuery, findConfigs, stdinFindConfigs) where

import Data.List (sort, sortBy, intersect, find)
import Data.Maybe (listToMaybe, fromMaybe, isJust, isNothing, fromJust)
import Data.Bool (bool)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as Ch8
import Data.Aeson (ToJSON, eitherDecode, encode)
import Control.Arrow (first, second)

import ServerConfig
import Query
import Geolocation
import Util

-- how many results should be returned
limit = 10

-- how should results be sorted
sortFn c1 c2 =
    let sc = costPerGBStorage . scCostPerMonth
    in case compare (sc c1) (sc c2) of
         EQ -> flip compare (scAvailability c1) (scAvailability c2)
         x  -> x

ioServers :: IO [Server]
ioServers = either (error . ("Invalid json file. (Storage Providers):" ++)) id . eitherDecode <$> Ch8.readFile "StorageProviders.json"

generateServerConfig :: Int -> Geolocation -> [Server] -> ServerConf
generateServerConfig k g servers@(_:_) = ServerConf k n g (atLeast k avails) cost delay duration servers
    where
        n = length servers
        avails = map servAvailability servers
        cost = scaleCost (1/ (fromIntegral k)) $ sumCost $ map servCost servers -- scale only really makes sense for storage cost
        delay = or $ map servDelayedFirstByte servers
        duration = maximum $ map servMinStorageDuration servers
generateServerConfig _ _ _ = error "At least 1 server is required to generate a server configuration."

isSafeLocation :: Geolocation -> Geolocation -> Bool
isSafeLocation = flip (<:)

validateQuery :: Ch8.ByteString -> [Server] -> Maybe String
validateQuery q servers
    = case eitherDecode q of
        Right (Query k n mloc minAvail maxCost delay minDur order opt)
            | fromMaybe False $ fmap (<= 0) k -> Just "k <= 0"
            | fromMaybe False (n >>= (flip fmap k . (<))) -> Just "n < k"
            | fromMaybe False $ fmap  (\x -> x < 0 || x > 1) minAvail -> Just "avail < 0 or > 1"
            | otherwise -> do
                mids <- opt >>= (fmap (map moptId) . optMultOption)
                let sids = map servId servers
                    nonExisting = filter (not . flip elem sids) mids
                fmap (\id -> "MultOption error: Server with id " ++ show id ++ " does not exist.") $ listToMaybe nonExisting
        Left errMsg -> Just errMsg

-- ignore sorting as this will be done by the sorting server
buildConfigs :: [Server] -> Query -> [ServerConf]
buildConfigs servers (Query k n mloc minAvail maxCost delay minDur _ opt) = sortedConfigs
    where
        servers' = maybeDo servers $ do
            mopts <- opt >>= optMultOption
            let singleMult s mopt = let
                    id = moptId mopt
                    m = moptM mopt
                    (mserver, s') = fromMaybe
                        (error ("MultOption error: Server with id " ++ show id ++ " does not exist."))
                        $ find' ((id ==) . servId) s
                    in replicate m mserver ++ s'
            pure $ flip (foldl singleMult) mopts
        pickServers :: Int -> Cost -> Int -> [Server] -> [((Bool, Int), [Server])]
        pickServers _ _ _ [] = []
        pickServers servCnt cost unsafeLocationCnt (x:xs)
          | maybe False (servCnt >=) n = []
          | otherwise = bool [] (bool [((isSafe, finalLCnt), [x])] (map (second (x:)) nextPicks) (nextPicks /= [])) isSuited ++ pickServers servCnt cost unsafeLocationCnt xs
                where
                    isSafe = isSafeLocation (fromMaybe top mloc) $ servServerLocation x
                    newUnsafeLocationCnt = unsafeLocationCnt + bool 1 0 isSafe
                    newCost = cost `addCost` servCost x
                    newServCnt = servCnt + 1
                    isSuited = maybe True (newUnsafeLocationCnt <) k && maybe True (not . flip exceedsLimit newCost) maxCost && (fromMaybe True delay || (not $ servDelayedFirstByte x)) && maybe True (servMinStorageDuration x <=) minDur
                    finalLCnt = fromMaybe (newUnsafeLocationCnt + 1) k
                    nextPicks :: [((Bool, Int), [Server])]
                    nextPicks = bool nextPicks' (map (first (first (const True))) nextPicks') isSafe
                    nextPicks' = pickServers newServCnt newCost newUnsafeLocationCnt xs

        filtered :: [(Int, [Server])]
        filtered = map (first snd) (filter
                                    (\((b, _), s)
                                      -> let le = fromIntegral $ length s
                                         in b && maybe True (le>=) k && maybe True (le ==) n)
                                    $ pickServers 0 noCost 0 servers')

        configs :: [ServerConf]
        configs = map (uncurry $ flip generateServerConfig $ fromMaybe top mloc) filtered

        filteredConfigs = maybe configs (\javail -> filter ((javail <=) . scAvailability) configs) minAvail

        sortedConfigs = take limit $ sortBy sortFn filteredConfigs

findConfigs :: FilePath -> IO (Either String [ServerConf])
findConfigs path = do
    servers <- ioServers
    file <- Ch8.readFile path
    let query = eitherDecode file
    pure $ buildConfigs servers <$> query

stdinFindConfigs :: IO (Either String [ServerConf])
stdinFindConfigs = do
    servers <- ioServers
    query <- fmap eitherDecode Ch8.getContents
    pure $ buildConfigs servers <$> query

stdinValidateQuery :: IO (Maybe String)
stdinValidateQuery = do
    servers <- ioServers
    query <- Ch8.getContents
    pure $ validateQuery query servers
