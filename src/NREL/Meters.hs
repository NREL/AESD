{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module NREL.Meters (
  metersRSF2
, modelRSF2
) where


import CESDS.Types (Tags(..))
import CESDS.Types.Model (Model(..))
import CESDS.Types.Variable as V (Display(..), Domain(..), Units(..), Variable(..), VariableIdentifier)
import Control.Arrow ((***))
import Data.Text (Text)
import Data.Tuple (swap)

import qualified Data.Text as T (cons, splitAt, tail)


metersRSF2 :: [(Text, VariableIdentifier)]
metersRSF2 =
  map (swap . (T.cons '@' *** T.tail) . T.splitAt 17)
    [
      "1edb6d30-d9847159 RSF2 PV Generation Power"
    , "1edb6d30-ce98398c RSF2 Plug Loads Power"
    , "1edb6d30-bcf051ea RSF2 Panel SWB Power"
    , "1edb6d30-a9d7ca09 RSF2 Panel H2 Power"
    , "1edb6d30-f6668673 RSF2 Occupant Elevator Power"
    , "1edb6d30-9a89271e RSF2 Mechanical Power"
    , "1edb6d30-f64869a4 RSF2 Main Meter Power"
    , "1edb6d30-6e697a28 RSF2 Lighting Power"
    , "1edb6d30-a4ccaaf4 RSF2 Freight Elevator Power"
    , "1edb6d30-db52fab5 RSF2 Emergency Lighting Power"
    , "1edb6d30-fa21e31d RSF2 Elevators Power"
    , "1edb6d30-1e909c64 RSF2 Building Load Power"
    , "1edb6d30-5d812ce6 RSF2 AC-3 Power"
    , "1edb6d30-60aa00e5 RSF2 AC-2/2a Power"
    , "1edb6d30-73828443 RSF2 AC-1 Power"
    ]


modelRSF2 :: Model
modelRSF2 =
  let
    identifier  = "RSF2v0"
    uri         = Just $ read "http://www.nrel.gov/sustainable_nrel/rsf.html#RSF2v0"
    name        = "RSF 2 Version 0"
    description = Just "Selected power meters from the RSF 2"
    tags        = Just $ Tags
                  [
                    ("DC.source"     , "https://skyspark-ops.nrel.gov/proj/nrel")
                  , ("DC.creator"    , "Brian W Bush <brian.bush@nrel.gov"      )
                  , ("DC.description", "Selected power meters from the RSF 2"   )
                  ]
    generation  = 0
    recordCount = 0
    variables   = [
                    Variable
                    {
                      V.identifier = "time"
                    , display      = Display
                                     {
                                       label      = "Time Stamp"
                                     , shortLabel = Just "Time"
                                     , color      = Nothing
                                     }
                    , domain       = Set []
                    , units        = Nothing
                    , isInput      = False
                    }
                  , Variable
                    {
                      V.identifier = "epoch"
                    , display      = Display
                                     {
                                       label      = "POSIX Seconds"
                                     , shortLabel = Just "Seconds"
                                     , color      = Nothing
                                     }
                    , domain       = Interval (Just 315558000) Nothing
                    , units        = Just $ Units 0 0 1 0 0 0 0 0 1
                    , isInput      = False
                    }
                  ] ++ [
                    Variable
                    {
                      V.identifier = identifier'
                    , display      = Display
                                     {
                                       label      = label'
                                     , shortLabel = Just identifier'
                                     , color      = Nothing
                                     }
                    , domain       = Interval Nothing Nothing
                    , units        = Just $ Units 2 1 (-3) 0 0 0 0 0 1000
                    , isInput      = False
                    }
                  |
                    (label', identifier') <- metersRSF2
                  ]
    primaryKey  = "time"
    timeKey     = Just "epoch"
  in
    Model{..}
