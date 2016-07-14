{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Haystack (
  HaystackAccess(..)
, haystackRequest
, haystackNav
, haystackNavTree
, getNavId
, getId
) where


import Control.Arrow ((***))
import Control.Monad.Except (MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value, (.=), object)
import Data.Aeson.Util (extract)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, addRequestHeader, defaultRequest, getResponseBody, httpJSON, setRequestBasicAuth, setRequestHost, setRequestPath, setRequestPort, setRequestQueryString, setRequestSecure)


data HaystackAccess =
  HaystackAccess
  {
    server        :: String
  , root          :: String
  , authorization :: Maybe (String, String)
  , secure        :: Maybe Bool
  , port          :: Maybe Int
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON HaystackAccess where

instance ToJSON HaystackAccess where


haystackRequest :: HaystackAccess -> String -> Request
haystackRequest HaystackAccess{..} path =
    setRequestHost (pack server)
  $ setRequestPath (pack $ root ++ path)
  $ setRequestSecure secure'
  $ setRequestPort (fromMaybe (if secure' then 443 else 80) port)
  $ maybe id (uncurry setRequestBasicAuth . (pack *** pack)) authorization
    defaultRequest
    where
      secure' = fromMaybe True secure


setNavId :: Maybe String -> Request -> Request
setNavId Nothing           = id
setNavId (Just identifier) = setRequestQueryString [("navId", Just $ pack identifier)]


haystackNav :: MonadIO m => HaystackAccess -> Maybe String -> m [Value]
haystackNav access identifier =
  let
    request =
        addRequestHeader "Accept" "application/json"
      $ setNavId identifier
      $ haystackRequest access "/nav"
  in
    extract "rows"
      . getResponseBody
      <$> httpJSON request


haystackNavTree :: MonadIO m => HaystackAccess -> Maybe String -> m Value
haystackNavTree access identifier =
  let
    visit parent =
      do
        let navId = getNavId parent
        children <- haystackNavTree access $ trace (getId parent) $ Just navId
        return $ object ["entity" .= parent, "children" .= children]
  in
    fmap toJSON
      . mapM visit
      =<< haystackNav access identifier


getNavId :: Value -> String
getNavId = getIdField "navId"


getId :: Value -> String
getId = getIdField "id"


getIdField :: String -> Value -> String
getIdField label o =
  case extract label o of
    'u' : ':' : x -> "`" ++ x ++ "`"
    'r' : ':' : x -> "@" ++ x
    x             -> x
