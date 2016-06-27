{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Test (
) where


import CESDS.Types (Color, Tags(..))
import Data.Aeson.Types (Value(..))
import Data.Colour.SRGB (sRGB24)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific, fromFloatDigits, scientific)
import Data.Text (Text, pack)
import Network.URI (URI, parseURI)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, elements, listOf1, oneof, resize)

import qualified Data.HashMap.Strict as H (fromList)
import qualified Data.Vector as V (fromList)


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
      , Array . V.fromList <$> arbitrary
      , String <$> arbitrary
      , Number <$> arbitrary
      , Bool   <$> arbitrary
      , return Null
      ]
      

instance Arbitrary Tags where
  arbitrary = Tags <$> resize 4 arbitrary


instance Arbitrary Color where
  arbitrary = sRGB24 <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary URI where
  arbitrary = -- FIXME: Make this more rigorous, even though it is good enough for testing this package.
    fromJust
      . parseURI
      . ("http://" ++)
      <$> listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'])
