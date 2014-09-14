{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.NetworkManager
-- Copyright   :  (C) 2014 Alexander Shabalin
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Plugins.Monitors.NetworkManager(networkManagerConfig, runNetworkManager) where

import Control.Applicative

import Data.Maybe(catMaybes, listToMaybe)
import Data.List(group, intercalate, maximumBy)
import Data.Word(Word16, Word32, Word8)
import Data.ByteString(ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import DBus
import DBus.Client
import Numeric(showHex)
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

getActiveConnectionDev :: Client -> ObjectPath -> IO (Maybe String)
getActiveConnectionDev client path = do
   devs <- getProperty client
      path
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager.Connection.Active"
      "Devices"
   getDevName (listToMaybe =<< devs)
 where getDevName Nothing = return Nothing
       getDevName (Just dev) = getProperty client
          dev
          (Just "org.freedesktop.NetworkManager")
          "org.freedesktop.NetworkManager.Device"
          "Interface"

getActiveConnectionName :: Client -> ObjectPath -> IO (Maybe String)
getActiveConnectionName client path =
   getProperty client
      path
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager.Connection.Active"
      "Id"

data IPv4 = IPv4 Word8 Word8 Word8 Word8

instance Show IPv4 where
   show (IPv4 a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

parseIPv4 :: [Word32] -> Maybe IPv4
parseIPv4 [] = Nothing
parseIPv4 (x:_) = Just $ IPv4 (getByte 0) (getByte 1) (getByte 2) (getByte 3)
 where getByte :: Int -> Word8
       getByte i = fromIntegral $ (x `div` (2 ^ (i * 8))) `mod` 256

data IPv6 = IPv6 Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16

instance Show IPv6 where
   show (IPv6 a b c d e f g h) = intercalate ":" $ shorten $ map (flip showHex "") [a, b, c, d, e, f, g, h]
    where shorten vs =
             let grp = group vs
                 cmp xs ys
                  | "0":_ <- xs
                  , "0":_ <- ys
                  = compare (length xs) (length ys)
                  | "0":_ <- xs = GT
                  | "0":_ <- ys = LT
                  | otherwise = EQ
                 mlen = length $ maximumBy cmp grp
                 go [] = []
                 go (x:xs)
                  | mlen > 1
                  , "0":_ <- x
                  , length x == mlen
                  = [""] : xs
                  | otherwise = x : go xs
             in map concat $ go grp

parseIPv6 :: (ByteString, Word32, ByteString) -> Maybe IPv6
parseIPv6 (addr, _, _) = toIP $ to16 $ ByteString.unpack addr
 where to16 :: [Word8] -> [Word16]
       to16 [] = []
       to16 [x] = [fromIntegral x]
       to16 (x1:x2:xs) = (fromIntegral x1 * 256 + fromIntegral x2) : to16 xs
       toIP :: [Word16] -> Maybe IPv6
       toIP (a:b:c:d:e:f:g:h:_) = Just $ IPv6 a b c d e f g h
       toIP _ = Nothing

getActiveConnectionIP
   :: (IsValue a)
   => MemberName
   -> InterfaceName
   -> (a -> Maybe b)
   -> Client
   -> ObjectPath
   -> IO [b]
getActiveConnectionIP mname iname parseIP client path = do
   ips <- getIPs =<< getProperty client
      path
      (Just "org.freedesktop.NetworkManager")
      "org.freedesktop.NetworkManager.Connection.Active"
      mname
   return $ maybe [] (catMaybes . map parseIP) ips
 where getIPs Nothing = return Nothing
       getIPs (Just config) = getProperty client
          config
          (Just "org.freedesktop.NetworkManager")
          iname
          "Addresses"

getActiveConnectionIPv4 :: Client -> ObjectPath -> IO [IPv4]
getActiveConnectionIPv4 =
   getActiveConnectionIP "Ip4Config" "org.freedesktop.NetworkManager.IP4Config" parseIPv4

getActiveConnectionIPv6 :: Client -> ObjectPath -> IO [IPv6]
getActiveConnectionIPv6 =
   getActiveConnectionIP "Ip6Config" "org.freedesktop.NetworkManager.IP6Config" parseIPv6

networkManagerConfig :: IO MConfig
networkManagerConfig = mkMConfig "<status>"
   [ "status", "ipv4", "ipsv4", "ipv6", "ipsv6", "dev", "name" -- General
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
       return [ByteStringChar8.unpack (_ssid s), q, qb, qvb, qdstr]
formatWireless _ _ _ = return $ replicate 5 ""

formatIPs :: [IPv4] -> [IPv6] -> Monitor [String]
formatIPs ipsv4 ipsv6 =
   return [onHead ipsv4, onList ipsv4, onHead ipsv6, onList ipsv6]
 where onHead :: (Show a) => [a] -> String
       onHead = maybe "" show . listToMaybe
       onList :: (Show a) => [a] -> String
       onList = intercalate ", " . map show

runNetworkManager :: [String] -> Monitor String
runNetworkManager args = do
   opts <- io $ parseOpts args
   conn <- io $ getActiveConnectionPath dbusClient
   ctype <- maybe (return Nothing) (io . getActiveConnectionType dbusClient) conn
   cobj <- maybe (return Nothing) (io . getActiveConnectionSpecificObject dbusClient) conn
   cipv4 <- maybe (return []) (io . getActiveConnectionIPv4 dbusClient) conn
   cipv6 <- maybe (return []) (io . getActiveConnectionIPv6 dbusClient) conn
   cdev <- maybe (return Nothing) (io . getActiveConnectionDev dbusClient) conn
   cname <- maybe (return Nothing) (io . getActiveConnectionName dbusClient) conn
   st <- formatStatus opts ctype
   ips <- formatIPs cipv4 cipv6
   lst <- formatWireless opts ctype cobj
   parseTemplate $ st : ips ++ [maybe "" id cdev] ++ [maybe "" id cname] ++ lst
