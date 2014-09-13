{-# LANGUAGE OverloadedStrings #-}

module Plugins.Monitors.NetworkManager(networkManagerConfig, runNetworkManager) where

import Control.Applicative

import Data.Maybe(listToMaybe)
import Data.Word(Word8)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as ByteString
import DBus
import DBus.Client
import System.Console.GetOpt
import System.IO.Unsafe (unsafePerformIO)

import Plugins.Monitors.Common

data NetworkManagerOpts = NetworkManagerOpts
   { statusOther :: String
   , statusWired :: String
   , statusWireless :: String
   , qualityDynamicString :: Maybe DynamicString
   }

defaultOpts :: NetworkManagerOpts
defaultOpts = NetworkManagerOpts
   { statusOther = "Other"
   , statusWired = "Wired"
   , statusWireless = "Wireless"
   , qualityDynamicString = Nothing
   }

options :: [OptDescr (NetworkManagerOpts -> NetworkManagerOpts)]
options =
   [ Option "o" ["status-other"] (ReqArg (\d opts -> opts { statusOther = d }) "") ""
   , Option "w" ["status-wired"] (ReqArg (\d opts -> opts { statusWired = d }) "") ""
   , Option "W" ["status-wireless"] (ReqArg (\d opts -> opts { statusWireless = d }) "") ""
   , Option "" ["quality-dynamic-string"] (ReqArg (\d opts ->
      opts { qualityDynamicString = Just $ parseDynamicString d }) "") ""
   ]

parseOpts :: [String] -> IO NetworkManagerOpts
parseOpts argv =
   case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

getProperty
   :: (IsVariant a)
   => Client
   -> ObjectPath
   -> Maybe BusName
   -> InterfaceName
   -> MemberName
   -> IO (Maybe a)
getProperty client path dest interface propName = do
   reply <- call client (methodCall path "org.freedesktop.DBus.Properties" "Get")
      { methodCallDestination = dest
      , methodCallBody =
         [ toVariant interface
         , toVariant propName
         ]
      }
   return $ fromVariant =<< fromVariant =<< listToMaybe =<< either (const Nothing) (Just . methodReturnBody) reply

getActiveConnectionPath :: Client -> IO (Maybe ObjectPath)
getActiveConnectionPath client = do
   conns <- getProperty client
      "/org/freedesktop/NetworkManager"
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager"
      "ActiveConnections"
   return $ listToMaybe =<< conns

data ConnectionType
   = Wired
   | Wireless
   | Other
   deriving Show

getActiveConnectionType :: Client -> ObjectPath -> IO (Maybe ConnectionType)
getActiveConnectionType client path = do
   name <- getProperty client
      path
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager.Connection.Active"
      "Type"
   return $
      case name :: Maybe String of
       Just "802-11-wireless" -> Just Wireless
       Just "802-3-ethernet" -> Just Wired
       Just _ -> Just Other
       Nothing -> Nothing

getActiveConnectionSpecificObject :: Client -> ObjectPath -> IO (Maybe ObjectPath)
getActiveConnectionSpecificObject client path = do
   getProperty client
      path
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager.Connection.Active"
      "SpecificObject"

data WirelessStats = WirelessStats
   { _ssid :: ByteString
   , _strength :: Word8
   }
   deriving (Show)

getWirelessStats :: Client -> ObjectPath -> IO (Maybe WirelessStats)
getWirelessStats client path = do
   ssid <- getProperty client
      path
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager.AccessPoint"
      "Ssid"
   strength <- getProperty client
      path
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager.AccessPoint"
      "Strength"
   return $ WirelessStats <$> ssid <*> strength

networkManagerConfig :: IO MConfig
networkManagerConfig = mkMConfig "<status>"
   [ "status" -- General
   , "essid", "quality", "qualitybar", "qualityvbar", "qualitydstr" -- Wireless only
   ]

-- Just like in Mpris module
dbusClient :: Client
dbusClient = unsafePerformIO connectSystem

formatStatus :: NetworkManagerOpts -> Maybe ConnectionType -> Monitor String
formatStatus _ Nothing = getConfigValue naString
formatStatus opts (Just Other) = return $ statusOther opts
formatStatus opts (Just Wired) = return $ statusWired opts
formatStatus opts (Just Wireless) = return $ statusWireless opts

formatWireless :: NetworkManagerOpts -> Maybe ConnectionType -> Maybe ObjectPath -> Monitor [String]
formatWireless opts (Just Wireless) (Just cobj) = do
   mstats <- io $ getWirelessStats dbusClient cobj
   case mstats of
    Nothing -> return $ replicate 5 ""
    Just s -> do
       let qlty = fromIntegral (_strength s)
       q <- showPercentWithColors (qlty / 100)
       qb <- showPercentBar qlty (qlty / 100)
       qvb <- showVerticalBar qlty (qlty / 100)
       qdstr <- showDynamicString (qualityDynamicString opts) (qlty / 100)
       return [ByteString.unpack (_ssid s), q, qb, qvb, qdstr]
formatWireless _ _ _ = return $ replicate 5 ""

runNetworkManager :: [String] -> Monitor String
runNetworkManager args = do
   opts <- io $ parseOpts args
   conn <- io $ getActiveConnectionPath dbusClient
   ctype <- maybe (return Nothing) (io . getActiveConnectionType dbusClient) conn
   cobj <- maybe (return Nothing) (io . getActiveConnectionSpecificObject dbusClient) conn
   st <- formatStatus opts ctype
   lst <- formatWireless opts ctype cobj
   parseTemplate $ st : lst
