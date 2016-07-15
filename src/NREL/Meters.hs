{-# LANGUAGE OverloadedStrings #-}


module NREL.Meters (
  nrelRSF2
, navRoot
, navRSF2
, idRSF2MainPower
) where


import CESDS.Types (Identifier)
import Control.Arrow ((***))
import Data.Text (Text)
import Data.Tuple (swap)

import qualified Data.Text as T (cons, splitAt, tail)


nrelRSF2 :: [(Text, Identifier)]
nrelRSF2 =
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


navRoot :: Maybe Identifier
navRoot = Nothing


navRSF2 :: Maybe Identifier
navRSF2 = Just "`equip:/1edb6bcb-7a9026e4`"


idRSF2MainPower :: Identifier
idRSF2MainPower = "@1edb6d30-f64869a4"
