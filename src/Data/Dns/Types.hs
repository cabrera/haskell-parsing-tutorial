module Data.Dns.Types (
    DnsType(..)
  , DnsClass(..)
  , DnsRecord(..)
  , DomainName(..)
  , HinfoCPU(..)
  , HinfoOS(..)
  , SOASerial(..)
  , SOAExpire(..)
  , SOARefresh(..)
  , SOARetry(..)
  , SOAMinimum(..)
  , SOAMName(..)
  , SOARName(..)
) where

import Data.ByteString
import Data.Word

-- my naming scheme is *glorious*
newtype DomainName = DomainName {unDN :: ByteString} deriving (Show, Eq)
newtype HinfoCPU = HinfoCPU {unHinfoCPU :: ByteString} deriving (Show, Eq)
newtype HinfoOS = HinfoOS {unHinfoOS :: ByteString} deriving (Show, Eq)
newtype SOASerial = SOASerial {unSerial :: Word32} deriving (Show, Eq)
newtype SOARefresh = SOARefresh {unRefresh :: Word32} deriving (Show, Eq)
newtype SOAExpire = SOAExpire {unExpire :: Word32} deriving (Show, Eq)
newtype SOARetry = SOARetry {unRetry :: Word32} deriving (Show, Eq)
newtype SOAMinimum = SOAMinimum {unMinimum :: Word32} deriving (Show, Eq)
newtype SOAMName = SOAMName DomainName deriving (Show, Eq)
newtype SOARName = SOARName DomainName deriving (Show, Eq)

data DnsType = ARecord Word32
             | NSRecord DomainName
             | CNAMERecord DomainName
             | SOARecord SOAMName SOARName SOASerial SOARefresh SOARetry SOAExpire SOAMinimum
             | PTRRecord DomainName
             | HINFORecord HinfoCPU HinfoOS
             | MXRecord Word16 DomainName
             | TXTRecord ByteString
             | UnknownRecord deriving (Show, Eq)

data DnsClass = INClass 
              | UnknownClass deriving (Show, Eq)

data DnsRecord = DnsRecord { dnsName :: ByteString
                           , dnsType :: DnsType
                           , dnsClass :: DnsClass
                           , dnsTTL :: Word32
                           , dnsRdLength :: Word16
                           , dnsRdData :: ByteString
                           } deriving (Show, Eq)