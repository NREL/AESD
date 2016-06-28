{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import CESDS.Server (Service(..), runService)
import CESDS.Types.Server (Server)
import CESDS.Types.Server.Test ()
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (generate)


data ServerState =
  ServerState
  {
    server :: Server
  }


service :: Service ServerState
service =
  let
    getServer = server
  in
    Service{..}


initialize :: IO ServerState
initialize =
  do
    server <- generate arbitrary
    return ServerState{..}


main :: IO ()
main = runService 8090 service =<< initialize


{-

GET /server - - SERVER

POST /server - COMMAND COMMAND_RESULT

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
