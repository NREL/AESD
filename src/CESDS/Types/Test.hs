{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Test (
  arbitraryVal
) where


import CESDS.Types (Color, Tags(..), Val(..))
import CESDS.Types.Variable (Domain(..))
import Data.Aeson.Types (Value(..))
import Data.Colour.SRGB (sRGB24)
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific, fromFloatDigits, scientific, toRealFloat)
import Data.Text (Text, pack)
import Network.URI (URI, parseURI)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, elements, listOf1, oneof, resize)

import qualified Data.HashMap.Strict as H (fromList)
import qualified Data.Vector as V (fromList)


instance Arbitrary Text where
  arbitrary = pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])


instance Arbitrary Scientific where
  arbitrary =
    oneof
      [
        fromFloatDigits   <$> (arbitrary :: Gen Double )
      , flip scientific 0 <$> (arbitrary :: Gen Integer)
      ]


instance Arbitrary Value where
  arbitrary =
    oneof
      [
        Object . H.fromList <$> resize 4 arbitrary
      , Array  . V.fromList <$> resize 4 arbitrary
      , String              <$>          arbitrary
      , Number              <$>          arbitrary
      , Bool                <$>          arbitrary
      , return Null
      ]
      

instance Arbitrary Tags where
  arbitrary = Tags . nubBy ((==) `on` fst) <$> resize 4 arbitrary


instance Arbitrary Color where
  arbitrary = sRGB24 <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary URI where
  arbitrary = -- FIXME: Make this more rigorous, even though it is good enough for testing this package.
    fromJust
      . parseURI
      . ("http://" ++)
      <$> listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'])


arbitraryVal :: Domain -> Gen Val
arbitraryVal Interval{..} =
  Continuous <$>
    case (lowerBound, upperBound) of
      (Nothing, Nothing) -> arbitrary
      (Nothing, Just u ) -> fromFloatDigits <$> choose (toRealFloat $ minimum [0, 2 * u], toRealFloat   u                  :: Double)
      (Just l , Nothing) -> fromFloatDigits <$> choose (toRealFloat   l                 , toRealFloat $ maximum [0, 2 * l] :: Double)
      (Just l , Just u ) -> fromFloatDigits <$> choose (toRealFloat   l                 , toRealFloat   u                  :: Double)
arbitraryVal Set{..} =
  case options of
    [] -> Discrete <$> arbitrary
    xs -> Discrete <$> elements xs


instance Arbitrary Val where
  arbitrary =
    oneof
      [
        Continuous <$> arbitrary
      , Discrete   <$> arbitrary
      ]
