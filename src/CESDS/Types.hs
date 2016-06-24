module CESDS.Types (
  Identifier
, Color
, Tags
, Generation
) where


import Data.Aeson (Object)
import Data.Text (Text)


type Identifier = Text


type Color = String -- FIXME SVGcolor


type Tags = Object


type Generation = Int
