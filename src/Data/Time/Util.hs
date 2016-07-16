module Data.Time.Util (
  SecondsPOSIX
, toSecondsPOSIX
, fromSecondsPOSIX
, getSecondsPOSIX
) where


import Control.Arrow (second)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import Data.Time.LocalTime (TimeZone, utcToZonedTime, zonedTimeToUTC)


type SecondsPOSIX = Int


theFormat :: String
theFormat = "%FT%X%z %Z"


toSecondsPOSIX :: String -> SecondsPOSIX
toSecondsPOSIX =
  truncate
    . utcTimeToPOSIXSeconds
    . zonedTimeToUTC
    . parseTimeOrError False defaultTimeLocale theFormat


fromSecondsPOSIX :: TimeZone -> SecondsPOSIX -> String
fromSecondsPOSIX zone =
  uncurry (++)
    . second (':' :)
    . splitAt 22
    . formatTime defaultTimeLocale theFormat
    . utcToZonedTime zone
    . posixSecondsToUTCTime
    . fromIntegral

 
getSecondsPOSIX :: IO SecondsPOSIX
getSecondsPOSIX =
  truncate
    . utcTimeToPOSIXSeconds
    <$> getCurrentTime
