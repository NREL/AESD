module Data.Aeson.Util (
  extractMaybe
, extract
) where


import Data.Aeson.Types (FromJSON(..), Value, (.:), parseMaybe, withObject)
import Data.Maybe (fromJust)
import Data.Text (pack)


extractMaybe :: FromJSON a => String -> Value -> Maybe a
extractMaybe label = parseMaybe (withObject ("extract " ++ label) (.: pack label))


extract :: FromJSON a => String -> Value -> a
extract = (fromJust .) . extractMaybe
