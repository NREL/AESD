{-# LANGUAGE FlexibleInstances #-}

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
import Data.Text (Text, pack)
import Network.URI (URI, parseURI)
import System.Exit (exitFailure)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, elements, listOf, listOf1, sample)
import Test.QuickCheck.Property (Property, property)
import Test.QuickCheck.Test (isSuccess, quickCheckResult)


checkShowRead :: (Eq a, Read a, Show a) => a -> Property
checkShowRead = property . uncurry (==) . (id &&& read . show)


checkEncodeDecode :: (Eq a, FromJSON a, ToJSON a) => a -> Property
checkEncodeDecode = property . uncurry (==) . (Just &&& decode . encode)


instance Arbitrary Color where
  arbitrary = sRGB24 <$> arbitrary <*> arbitrary <*> arbitrary


prop_color_io :: Color -> Property
prop_color_io = checkShowRead


prop_color_json :: Color -> Property
prop_color_json = checkEncodeDecode


instance Arbitrary URI where
  arbitrary = -- FIXME: Make this more rigorous, even though it is good enough for testing this package.
    fromJust
      . parseURI
      . ("http://" ++)
      <$> listOf1 (choose ('a', 'z'))


prop_uri_io :: URI -> Property
prop_uri_io = checkShowRead


prop_uri_json :: URI -> Property
prop_uri_json = checkEncodeDecode


instance Arbitrary Text where
  arbitrary = pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])


instance Arbitrary Tags where
  arbitrary = Tags <$> listOf ((,) <$> arbitrary <*> (String <$> arbitrary)) -- FIXME: Make this more rigorous.
      


instance Arbitrary Model where
  arbitrary = Model <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


prop_model_json :: Model -> Property
prop_model_json = checkEncodeDecode


main :: IO ()
main =
  do
    sample $ encode <$> (arbitrary :: Gen Model)
    sample (arbitrary :: Gen Model)
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
