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

import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Server as Server (Server(..))


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
    postServer (Command.Restart _) =
      randomFailure
        $ do
          new <- liftIO initialize
          modifys $ const new
          return Command.Success
    postServer _ = notImplemented
    getModel identifier = gets (lookup identifier . models)
    postModel (Command.Restart _) identifier =
      randomFailure
        $ maybe
          (return $ Command.Error "model not found" Nothing)
          (const $ return Command.Success)
        =<< gets (lookup identifier . models)
    postModel _ _ = notImplemented
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


notImplemented :: ServerM ServerState Command.Result
notImplemented = return $ Command.Error "not implemented" Nothing


randomFailure :: ServerM ServerState Command.Result -> ServerM ServerState Command.Result
randomFailure x =
  do
    outcome <- randomOutcome 9
    if outcome
      then x
      else return $ Command.Error "failed" $ Just "random failure for testing"


randomOutcome :: Int -> ServerM ServerState Bool
randomOutcome success =
  liftIO
    . generate
    $ frequency [(success, return True), (1, return False)]


{-

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
