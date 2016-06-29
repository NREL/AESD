{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import CESDS.Server (ServerM, Service(..), gets, modifys, runService)
import CESDS.Types.Model (Model, ModelIdentifier)
import CESDS.Types.Model.Test ()
import CESDS.Types.Server (Server)
import CESDS.Types.Server.Test ()
import Control.Monad.Reader (liftIO)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (frequency, generate)

import qualified CESDS.Types.Command as Command
import qualified CESDS.Types.Model as Model
import qualified CESDS.Types.Server as Server


data ServerState =
  ServerState
  {
    server :: Server
  , models :: [(ModelIdentifier, Model)]
  }


service :: Service ServerState
service =
  let
    getServer = gets server
    postServer Command.RestartServer =
      do
        outcome <- randomOutcome 9
        if outcome
          then do
                 server' <- liftIO $ generate arbitrary
                 modifys $ \s -> s {server = server'}
                 return Command.Success
          else return $ Command.Error "failed" $ Just "random failure for testing"
    postServer _ = return $ Command.Error "not implemented" Nothing
    getModel identifier = gets (lookup identifier . models)
  in
    Service{..}


initialize :: IO ServerState
initialize =
  do
    server <- generate arbitrary
    models <-
      sequence
        [
          do
            model <- generate arbitrary
            return (identifier, model {Model.identifier = identifier})
        |
          identifier <- Server.models server
        ]
    return ServerState{..}


main :: IO ()
main = runService 8090 service =<< initialize


randomOutcome :: Int -> ServerM ServerState Bool
randomOutcome success =
  liftIO
    . generate
    $ frequency [(success, return True), (1, return False)]


{-

GET /server/MODEL_ID - - MODEL

POST /server/MODEL_ID  - COMMAND COMMAND_RESULT

GET /server/MODEL_ID/recods from=GENERATION to=GENERATION primary_key=RECORD_ID result_id= multually explclusi - [RECORD]

GET /server/MODEL_ID/work status=WORK_STATUS from=GENERATION work_id=WORK_ID [WORK_STATUS_RESULT]

POST /server/MODEL_ID/work WORK_SUBMISSION WORK_SUBMISSION_RESULT

GET /server/MODEL/bookmark_metas TAG_ID multiple BOOKMARK_META

GET /server/MODEL/bookmarks bookmark_id=BOOKMARK_ID BOOKMARK

POST /server/MODEL_ID/bookmarks BOOKMARK w/o ID BOOKMARK_META

GET /server/MODEL_ID/filter_metas TAG_ID=TEXT multiple optional FILTER_META

GET /server/MODEL_ID/filters filter_id=FILTER_ID optional FILTER

POST /server/MODEL_ID/filters FILTER w/o ID FILTER_META

-}
