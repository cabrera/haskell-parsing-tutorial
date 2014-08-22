module Test where

import Control.Applicative
import Data.List (sort)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, sample', vectorOf, Gen, arbitrary)

import Data.Dns.Types (DnsRecord)
import Data.Dns.Gen ()


-- example quickCheck-style test for Tasty
tests :: TestTree
tests = testGroup "(quickCheck)" [qProp]

qProp :: TestTree
qProp = testProperty "sort = sort . reverse" (\xs -> sort (xs :: [Int]) == sort (reverse xs))

generate :: Int -> Gen a -> IO [a]
generate n gen = take n . concat <$> (sample' . vectorOf n) gen

-- output 1100 randomly generated DNSRecord data types
main :: IO ()
main = generate 1100 (arbitrary :: Gen DnsRecord) >>= print
