module Data.Time.Util (
  toSecondsPOSIX
, fromSecondsPOSIX
) where


import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import Data.Time.LocalTime (TimeZone, utcToZonedTime, zonedTimeToUTC)


theFormat :: String
theFormat = "%FT%X%z %Z"


toSecondsPOSIX :: String -> Int
toSecondsPOSIX =
  truncate
    . utcTimeToPOSIXSeconds
    . zonedTimeToUTC
    . parseTimeOrError False defaultTimeLocale theFormat


fromSecondsPOSIX :: TimeZone -> Int -> String
fromSecondsPOSIX zone =
  formatTime defaultTimeLocale theFormat
    . utcToZonedTime zone
    . posixSecondsToUTCTime
    . fromIntegral
  
