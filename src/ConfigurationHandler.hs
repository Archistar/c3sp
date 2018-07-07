module ConfigurationHandler(stdinValidateQuery, findConfigs, stdinFindConfigs) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as Ch8
import Data.Maybe
import Data.Aeson
import Data.List

import ServerConfig
import Query
import Geolocation
import Util

data AccLimit = AccLimit
  { accCost   :: Cost
  , accCc     :: Map String Int
  , accULoc   :: Int
  , accSafe   :: Bool
  , accSerCnt :: Int
  , accDelay  :: Bool
  , accDur    :: Int
  }

data BuildJob = BuildJob
  { bjAccLimit :: AccLimit
  , bjPicks    :: [Server]
  , bjPool     :: [Server]
  }

emptyAccLimit = AccLimit noCost M.empty 0 False 0 False 0

ioServers :: IO [Server]
ioServers = either (error . ("Invalid json file. (Storage Providers):" ++)) id . eitherDecode <$> Ch8.readFile "StorageProviders.json"

validateQuery :: Ch8.ByteString -> [Server] -> Maybe String
validateQuery q servers
    = case eitherDecode q of
        Right (Query lK uK k lN uN n limit mloc minAvail maxCost delay minDur order opt compMax)
            | fromMaybe False $ fmap (<= 0) k -> Just "k <= 0"
            | fromMaybe False (n >>= (flip fmap k . (<))) -> Just "n < k"
            | fromMaybe False $ fmap  (\x -> x < 0 || x > 1) minAvail -> Just "avail < 0 or > 1"
            | otherwise -> do
                mids <- opt >>= (fmap (map moptId) . optMultOption)
                let sids = map servId servers
                    nonExisting = filter (not . flip elem sids) mids
                fmap (\id -> "MultOption error: Server with id " ++ show id ++ " does not exist.") $ listToMaybe nonExisting
        Left errMsg -> Just errMsg

-- todo: special cases: no servers to pick from, negative limit
buildConfigs :: [Server] -> Query -> [ServerConf]
buildConfigs servers (Query lK uK k lN uN n limit mLoc avail cost delay dur mOrd opt comp) =
  let
    servers'      = maybeAdjust (fmap filterDuration dur)
                      $ maybeAdjust (fmap filterDelay delay)
                      $ maybeAdjust (fmap applyOptions opt) servers
    startBuildJob = BuildJob emptyAccLimit [] servers'

    -- query constraint functions
    inUpperK  = maybe (const True) (>=) uK
    isUpperK  = maybe (const True) (==) uK
    inLowerK  = maybe (const True) (<=) lK
    isK       = maybe (const True) (==) k
    inK       = maybe (const True) (>=) k
    outK      = maybe (const True) (<=) k
    inUpperN  = maybe (const True) (>=) uN
    isUpperN  = maybe (const True) (==) uN
    isUpperN' = maybe (const False) (==) uN
    inLowerN  = maybe (const True) (<=) lN
    isN       = maybe (const True) (==) n
    isN'      = maybe (const False) (==) n
    inN       = maybe (const True) (>=) n
    isLimit'  = maybe (const False) (==) limit . length
    loc       = fromMaybe top mLoc
    inAvail   = maybe (const True) (<=) avail
    inCost    = not . maybe (const False) (exceedsLimit) cost
    ord       = fromMaybe ByPerGBStorage mOrd
    inComp    = maybe (const True) (all . (>=)) comp . M.elems


    -- build list with given constraints
    pickServers :: [ServerConf] -> [BuildJob] -> [ServerConf]
    pickServers tl [] = tl
    pickServers tl (BuildJob al@(AccLimit costL compL locL safeL cntL delayL durL) picks pool:xs) =
      let (pick:pool') = pool

          -- Current pick info
          sComp  = servCompany pick
          sLoc   = servServerLocation pick
          sAvail = servAvailability pick
          sCost  = servCost pick
          sSafe  = sLoc <: loc
          sDelay = servDelayedFirstByte pick
          sDur   = servMinStorageDuration pick

          -- Job for skipping current pick
          hasNoPickJob = not $ emptyPool'
          noPickJob    = BuildJob al picks pool'
          noPickJob'   = lift' hasNoPickJob noPickJob

          -- Job for using current pick
          hasPickJob = not endCurrentJob
          pickJob    = BuildJob al' picks' pool'
          pickJob'   = lift' hasPickJob pickJob

          -- Update pick list
          picks' = pick:picks

          -- New acclimit when using pick
          costL'  = addCost costL sCost
          compL'  = updateCompCnt sComp compL
          locL'   = if sSafe then locL else locL + 1
          safeL'  = safeL || sSafe
          cntL'   = cntL + 1
          delayL' = delayL || sDelay
          durL'   = max durL sDur
          al'     = AccLimit costL' compL' locL' safeL' cntL' delayL' durL'

          -- New (including pick) configuration info
          cK     = fromMaybe (locL' + 1) k
          cAvail = atLeast cK $ map servAvailability picks'
          conf   = ServerConf cK cntL' loc cAvail costL' delayL' durL' picks'

          -- New top list
          tl' = if canFinish then updateTopList tl conf ord fullTl else tl

          -- Updated job stack
          jobs = catMaybes [pickJob', noPickJob'] ++ xs

          -- Check if adding pick is not violating the constraints
          inCost' x =
            let cpmHighest = costPerGBStorage $ scCostPerMonth $ head tl
                cpmCurrent = costPerGBStorage costL'
            in inCost x && (not (ord == ByPerGBStorage && fullTl)
                            || cpmHighest >= cpmCurrent)
          fc = inUpperK (locL' + 1) && inK (locL'  + 1) && inCost' costL'
               && inComp compL'

          -- Checks
          emptyPool'      = null pool'
          fullTl          = isLimit' tl
          isSuitable      = fc
          canFinish       = isSuitable && isN cntL' && inLowerN cntL'
                            && inLowerK cntL' && outK cntL' && safeL'
                            && inAvail cAvail
          reachedCap      = isN' cntL' || isUpperN' cntL'
          endCurrentJob   = emptyPool' || reachedCap

      in pickServers tl' jobs
  in reverse $ pickServers [] [startBuildJob]

filterDuration :: Int -> [Server] -> [Server]
filterDuration x = filter ((x >=) . servMinStorageDuration)

filterDelay :: Bool -> [Server] -> [Server]
filterDelay x = if x then id else filter (not . servDelayedFirstByte)

applyOptions :: QueryOptions -> [Server] -> [Server]
applyOptions o = maybeAdjust $ do
  mopts <- optMultOption o
  let singleMult s mopt = let
        id = moptId mopt
        m = moptM mopt
        (mserver, s') = fromMaybe
          (error ("MultOption error: Server with id " ++ show id ++ " does not exist."))
          $ find' ((id ==) . servId) s
        in replicate m mserver ++ s'
  pure $ flip (foldl singleMult) mopts

lift' :: Bool -> a -> Maybe a
lift' True x  = Just x
lift' False _ = Nothing

updateTopList :: [ServerConf] -> ServerConf -> Order -> Bool -> [ServerConf]
updateTopList old el o full =
  let
    compareFn = case o of
                  ByAvail        -> availCompare
                  ByPerGBStorage -> gbStorageCompare
    new = insertBy compareFn el old
  in if full then tail new else new

updateCompCnt :: String -> Map String Int -> Map String Int
updateCompCnt comp cnt = if M.member comp cnt
                         then M.adjust (+1) comp cnt
                         else M.insert comp 1 cnt

availCompare c2 c1 =
  let sc = costPerGBStorage . scCostPerMonth
  in case compare (sc c1) (sc c2) of
       EQ -> flip compare (scAvailability c1) (scAvailability c2)
       x  -> x

gbStorageCompare c2 c1 =
  let sc = costPerGBStorage . scCostPerMonth
  in case compare (sc c1) (sc c2) of
       EQ -> flip compare (scAvailability c1) (scAvailability c2)
       x  -> x

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
