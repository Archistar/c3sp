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

import Util
import Geolocation
import ConfigurationHandler (findConfigs)
import ServerConfig (ServerConf)

data Test = Test {testName :: String, test :: IO Bool}
data ModuleTest = ModuleTest {moduleName :: String, tests :: [Test]}

nIoTest :: String -> Bool -> Test
nIoTest name = Test name . return

baseTestDir :: FilePath
baseTestDir = "tests/"

readTestFile :: FilePath -> IO Ch8.ByteString
readTestFile = Ch8.readFile . (baseTestDir ++)

testAgainstFile :: Ch8.ByteString -> FilePath -> IO Bool
testAgainstFile testResult = fmap (noOrdEq testJson . ergJson) . readTestFile
    where
        testJson = either (error . ("Invalid test json:" ++)) id $ eitherDecode testResult :: [Value]
        ergJson ergStr = either (error . ("Invalid json in test file:" ++)) id $ eitherDecode ergStr :: [Value]

-- checks if given value contains an exception
throwsException :: NFData a => a -> IO Bool
throwsException x = catch (x `deepseq` return False) (const $ return True :: SomeException -> IO Bool)

configurationHandlerTest :: ModuleTest
configurationHandlerTest =
    ModuleTest "ConfigurationHandler" [
        Test "Test1" $ tf "bsp1.1" "erg1.1",
        Test "Test2" $ tf "bsp1.2" "erg1.2",
        Test "Test3" $ tf "bsp1.3" "erg1.3",
        Test "Test4" $ tf "bsp1.4" "erg1.4",
        Test "Test5" $ tf "bsp1.5" "erg1.5",
        Test "Test6" $ tf "bsp1.6" "erg1.6",
        Test "Test7" $ tf "bsp1.7" "erg1.7",
        Test "Test8" $ tf "bsp1.8" "erg1.8",
        Test "Test9" $ tf "eval1.json" "erg1.json",
        Test "Test10" $ tf "eval2.json" "erg2.json"
    ]
        where tf i o = findConfigs (baseTestDir ++ i) >>= either error (flip testAgainstFile o . encode)

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
