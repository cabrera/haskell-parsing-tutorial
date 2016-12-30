{-# LANGUAGE OverloadedStrings #-}
module Data.Dns.Gen where
import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Char (chr)

import Data.Dns.Types
import Test.Tasty.QuickCheck (listOf1, choose, arbitrary, Arbitrary, elements, oneof)

instance Arbitrary ByteString where
  arbitrary = pack <$> listOf1 validChars
    where validChars = chr <$> choose (35, 126)

instance Arbitrary SOASerial where
  arbitrary = SOASerial <$> choose (0, 1200)

instance Arbitrary HinfoOS where
  arbitrary = HinfoOS <$> elements ["linux", "windows", "mac"]

instance Arbitrary HinfoCPU where
  arbitrary = HinfoCPU <$> elements ["intel", "amd", "arm", "mips"]

instance Arbitrary SOARefresh where
  arbitrary = SOARefresh <$> choose (0, 1200)

instance Arbitrary SOAExpire where
  arbitrary = SOAExpire <$> choose (0, 1200)

instance Arbitrary SOARetry where
  arbitrary = SOARetry <$> choose (0, 1200)

instance Arbitrary SOAMinimum where
  arbitrary = SOAMinimum <$> choose (0, 1200)

instance Arbitrary DnsClass where
  arbitrary = elements [INClass, UnknownClass]

-- TODO(allele): *actually* make this look like a domain name
instance Arbitrary DomainName where
  arbitrary = DomainName <$> arbitrary

instance Arbitrary SOAMName where
  arbitrary = SOAMName <$> arbitrary

instance Arbitrary SOARName where
  arbitrary = SOARName <$> arbitrary

instance Arbitrary DnsType where
  arbitrary = oneof [
      ARecord <$> arbitrary
    , NSRecord <$> arbitrary
    , CNAMERecord <$> arbitrary
    , PTRRecord <$> arbitrary
    , liftA2 HINFORecord arbitrary arbitrary
    , liftA2 MXRecord arbitrary arbitrary
    , SOARecord <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
    , TXTRecord <$> arbitrary
    , return UnknownRecord
    ]

instance Arbitrary DnsRecord where
  arbitrary = DnsRecord <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
