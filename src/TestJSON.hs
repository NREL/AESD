{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main (
  main
) where


import CESDS.Types (Color)
import CESDS.Types.Bookmark (Bookmark(..))
import CESDS.Types.Bookmark.Test ()
import CESDS.Types.Command (Command, Result)
import CESDS.Types.Command.Test ()
import CESDS.Types.Filter (Filter, SelectionExpression)
import CESDS.Types.Filter.Test ()
import CESDS.Types.Model (Model)
import CESDS.Types.Model.Test ()
import CESDS.Types.Record (Record)
import CESDS.Types.Record.Test ()
import CESDS.Types.Server (Server, Status)
import CESDS.Types.Server.Test ()
import CESDS.Types.Test ()
import CESDS.Types.Variable (Display, Domain, Units, Variable)
import CESDS.Types.Variable.Test ()
import CESDS.Types.Work (Submission, SubmissionResult, WorkStatus)
import CESDS.Types.Work.Test ()
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Data.Aeson.Types (FromJSON, ToJSON)
import Network.URI (URI)
import System.Exit (exitFailure)
import Test.QuickCheck.Property (Property, label, property)
import Test.QuickCheck.Test (isSuccess, quickCheckResult)


checkShowRead :: (Eq a, Read a, Show a) => a -> Property
checkShowRead = property . uncurry (==) . (id &&& read . show)


checkEncodeDecode :: (Eq a, FromJSON a, ToJSON a) => a -> Property
checkEncodeDecode = property . uncurry (==) . (Just &&& decode . encode)


prop_color_io :: Color -> Property
prop_color_io = label "Color read/show" . checkShowRead


prop_color_json :: Color -> Property
prop_color_json = label "Color JSON" . checkEncodeDecode


prop_uri_io :: URI -> Property
prop_uri_io = label "URI read/show" . checkShowRead


prop_uri_json :: URI -> Property
prop_uri_json = label "URI JSON" . checkEncodeDecode


prop_variable_json :: Variable -> Property
prop_variable_json = label "Variable JSON" . checkEncodeDecode


prop_display_json :: Display -> Property
prop_display_json = label "Display JSON" . checkEncodeDecode


prop_domain_json :: Domain -> Property
prop_domain_json = label "Domain JSON" . checkEncodeDecode


prop_units_json :: Units -> Property
prop_units_json = label "Units JSON" . checkEncodeDecode


prop_model_json :: Model -> Property
prop_model_json = label "Model JSON" . checkEncodeDecode


prop_status_json :: Status -> Property
prop_status_json = label "Status JSON" . checkEncodeDecode


prop_server_json :: Server -> Property
prop_server_json = label "Server JSON" . checkEncodeDecode


prop_result_json :: Result -> Property
prop_result_json = label "Result JSON" . checkEncodeDecode


prop_command_json :: Command -> Property
prop_command_json = label "Command JSON" . checkEncodeDecode


prop_record_json :: Record -> Property
prop_record_json = label "Record JSON" . checkEncodeDecode


prop_submission_json :: Submission -> Property
prop_submission_json = label "Submission JSON" . checkEncodeDecode


prop_submission_result_json :: SubmissionResult -> Property
prop_submission_result_json = label "SubmissionResult JSON" . checkEncodeDecode


prop_work_status_json :: WorkStatus -> Property
prop_work_status_json = label "WorkStatus JSON" . checkEncodeDecode


prop_bookmark_json :: Bookmark -> Property
prop_bookmark_json = label "Bookmark JSON" . checkEncodeDecode


prop_selection_json :: SelectionExpression -> Property
prop_selection_json = label "SelectionExpression JSON" . checkEncodeDecode


prop_filter_json :: Filter -> Property
prop_filter_json = label "Filter JSON" . checkEncodeDecode


main :: IO ()
main =
  do
    results <-
      sequence
        [
          quickCheckResult prop_color_io
        , quickCheckResult prop_color_json
        , quickCheckResult prop_uri_io
        , quickCheckResult prop_uri_json
        , quickCheckResult prop_display_json
        , quickCheckResult prop_domain_json
        , quickCheckResult prop_units_json
        , quickCheckResult prop_variable_json
        , quickCheckResult prop_model_json
        , quickCheckResult prop_status_json
        , quickCheckResult prop_server_json
        , quickCheckResult prop_result_json
        , quickCheckResult prop_command_json
        , quickCheckResult prop_record_json
        , quickCheckResult prop_submission_json
        , quickCheckResult prop_submission_result_json
        , quickCheckResult prop_work_status_json
        , quickCheckResult prop_bookmark_json
        , quickCheckResult prop_selection_json
        , quickCheckResult prop_filter_json
        ]
    unless (all isSuccess results) exitFailure
