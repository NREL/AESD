{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main (
  main
) where


import CESDS.Types (Color, Tags(..))
import CESDS.Types.Model (Model(..))
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, Value(..), decode, encode)
import Data.Colour.SRGB (sRGB24)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific, fromFloatDigits, scientific)
import Data.Text (Text, pack)
import Network.URI (URI, parseURI)
import System.Exit (exitFailure)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, elements, listOf1, oneof, resize, sample)
import Test.QuickCheck.Property (Property, property)
import Test.QuickCheck.Test (isSuccess, quickCheckResult)

import qualified Data.HashMap.Strict as H (fromList)
import qualified Data.Vector as V (fromList)


checkShowRead :: (Eq a, Read a, Show a) => a -> Property
checkShowRead = property . uncurry (==) . (id &&& read . show)


checkEncodeDecode :: (Eq a, FromJSON a, ToJSON a) => a -> Property
checkEncodeDecode = property . uncurry (==) . (Just &&& decode . encode)


instance Arbitrary Color where
  arbitrary = sRGB24 <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary URI where
  arbitrary = -- FIXME: Make this more rigorous, even though it is good enough for testing this package.
    fromJust
      . parseURI
      . ("http://" ++)
      <$> listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'])


instance Arbitrary Text where
  arbitrary = pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])


instance Arbitrary Scientific where
  arbitrary =
    oneof
      [
        fromFloatDigits <$> (arbitrary :: Gen Double)
      , flip scientific 0 <$> (arbitrary :: Gen Integer)
      ]


instance Arbitrary Value where
  arbitrary =
    oneof
      [
        Object . H.fromList <$> resize 4 arbitrary
      , Array . V.fromList <$> (arbitrary :: Gen [Value])
      , String <$> arbitrary
      , Number <$> arbitrary
      , Bool   <$> arbitrary
      , return Null
      ]


prop_color_io :: Color -> Property
prop_color_io = checkShowRead


prop_color_json :: Color -> Property
prop_color_json = checkEncodeDecode


prop_uri_io :: URI -> Property
prop_uri_io = checkShowRead


prop_uri_json :: URI -> Property
prop_uri_json = checkEncodeDecode


instance Arbitrary Tags where
  arbitrary = Tags <$> resize 4 arbitrary


instance Arbitrary Model where
  arbitrary =
    do
      identifier  <- arbitrary
      uri         <- arbitrary
      name        <- arbitrary
      description <- arbitrary
      tags        <- arbitrary
      generation  <- arbitrary
      variables   <- listOf1 arbitrary
      primaryKey  <- elements variables
      timeKey     <- elements $ Nothing : map Just variables
      return Model{..}


prop_model_json :: Model -> Property
prop_model_json = checkEncodeDecode


main :: IO ()
main =
  do
    results <-
      sequence
        [
          quickCheckResult prop_color_io
        , quickCheckResult prop_color_json
        , quickCheckResult prop_uri_io
        , quickCheckResult prop_uri_json
        , quickCheckResult prop_model_json
        ]
    unless (all isSuccess results) exitFailure
