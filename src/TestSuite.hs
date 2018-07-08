module TestSuite(Test, ModuleTest, runTest, runAllTests) where

import GHC.Exception (SomeException)
import Control.DeepSeq (deepseq, NFData)
import Control.Exception.Base (catch)
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Bool (bool)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as Ch8
import Data.Maybe (fromMaybe)
import Data.Scientific
import Control.Arrow

import Util
import Geolocation
import ConfigurationHandler (findConfigs)
import ServerConfig

data Test = Test {testName :: String, test :: IO Bool}
data ModuleTest = ModuleTest {moduleName :: String, tests :: [Test]}

nIoTest :: String -> Bool -> Test
nIoTest name = Test name . return

baseTestDir :: FilePath
baseTestDir = "tests/"

readTestFile :: FilePath -> IO Ch8.ByteString
readTestFile = Ch8.readFile . (baseTestDir ++)

testAgainstFile :: Ch8.ByteString -> FilePath -> IO Bool
testAgainstFile testResult = fmap ((==) testJson . ergJson) . readTestFile
    where
        comp1 = map (costPerGBStorage . scCostPerMonth)
        comp2 = map scAvailability
        testJson = either (error . ("Invalid test json:" ++)) (comp1&&&comp2) $ (eitherDecode testResult :: Either String [ServerConf])
        ergJson ergStr = either (error . ("Invalid json in test file:" ++)) (comp1&&&comp2) $ (eitherDecode ergStr :: Either String [ServerConf])

-- checks if given value contains an exception
throwsException :: NFData a => a -> IO Bool
throwsException x = catch (x `deepseq` return False) (const $ return True :: SomeException -> IO Bool)

configurationHandlerTest :: ModuleTest
configurationHandlerTest =
    ModuleTest "ConfigurationHandler" [
        Test "Test1" $ tf "1_2",
        Test "Test2" $ tf "2_4",
        Test "Test3" $ tf "3_5",
        Test "Test4" $ tf "4_6",
        Test "Test5" $ tf "5_8",
        Test "Test6" $ tf "6_9",
        Test "Test7" $ tf "moptAvail_1_3",
        Test "Test8" $ tf "moptAvail_2_4",
        Test "Test9" $ tf "moptAvail_3_5",
        Test "Test10" $ tf "moptAvail_4_7",
        Test "Test11" $ tf "moptAvail_5_8",
        Test "Test12" $ tf "optAvail_1_3",
        Test "Test13" $ tf "optAvail_2_4",
        Test "Test14" $ tf "optAvail_3_5",
        Test "Test15" $ tf "optAvail_4_6",
        Test "Test16" $ tf "optAvail_5_8",
        Test "Test17" $ tf "optAvail_6_9",
        Test "Test18" $ tf "p5optAvail_1_3",
        Test "Test19" $ tf "p5optAvail_2_4",
        Test "Test20" $ tf "p5optAvail_3_5",
        Test "Test21" $ tf "p5optAvail_4_7",
        Test "Test22" $ tf "p5optAvail_5_8"
    ]
        where tf i = findConfigs (baseTestDir ++ i ++ "_q.json") >>= either error (flip testAgainstFile (i ++ ".json") . encode)

utilTest :: ModuleTest
utilTest =
    ModuleTest "Util" [
        nIoTest "Test1" $ (safeRem 3 [1, 2, 3]) == Just [1, 2],
        nIoTest "Test2" $ (safeRem 4 [1, 2, 3]) == Nothing,
        nIoTest "Test3" $ noOrdEq [1, 1, 1, 3, 4] [4, 1, 1, 3, 1],
        nIoTest "Test4" $ not $ noOrdEq [1, 1, 1, 3, 4, 1] [4, 1, 1, 3, 1],
        nIoTest "Test5" $ not $ noOrdEq [5, 2, 2] [5, 2, 5],
        nIoTest "Test6" $ noOrdEq [9, 6, 3] [3, 9, 6]
    ]

geolocTest :: ModuleTest
geolocTest =
    ModuleTest "Geolocation" [
        nIoTest "Test1" $ either (const False) (== DE) $ readGeolocation "DE",
        nIoTest "Test2" $ either (const False) (== World) $ readGeolocation "World",
        nIoTest "Test3" $ and $ map (Local <:) geolocations,
        nIoTest "Test4" $ not $ and $ map (World <:) geolocations,
        nIoTest "Test5" $ JP <: JP,
        nIoTest "Test6" $ EU <: World,
        nIoTest "Test7" $ not $ JP <: EU,
        nIoTest "Test8" $ not $ EU <: Local,
        nIoTest "Test9" $ not $ World <: Local,
        nIoTest "Test10" $ not $ World <: IE,
        nIoTest "Test11" $ not $ BE <: Local,
        nIoTest "Test12" $ and $ map (<: EU) [IE, DE, EU, BE],
        nIoTest "Test13" $ not $ or $ map (<: EU) [BR, JP, US, HK]
    ]

allTests :: [ModuleTest]
allTests = [utilTest, geolocTest, configurationHandlerTest]

runTest :: ModuleTest -> IO String
runTest mt = ((++) ("Testing module " ++ moduleName mt ++ ": " ++ "\n") . intercalate "\n") <$> mapM (\tst -> (++) ("  " ++ testName tst ++ ": ") <$> fmap (bool "Failed!" "Passed.") (test tst)) (tests mt)

runAllTests :: IO String
runAllTests = intercalate "\n\n" <$> mapM runTest allTests
