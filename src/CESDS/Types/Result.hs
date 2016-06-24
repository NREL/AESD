module CESDS.Types.Result (
  ResultIdentifier
, Result
) where


import CESDS.Types (Identifier)
import Data.Aeson (Object)


type ResultIdentifier = Identifier


type Result = Object
