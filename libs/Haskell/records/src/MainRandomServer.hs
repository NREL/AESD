{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Server for random data.
-}


{-# LANGUAGE TupleSections #-}


module Main (
-- * Entry point
  main
) where


import CESDS.Records.Server (serverMain)
import CESDS.Records.Server.Manager (makeInMemoryManager)
import CESDS.Types.Model (makeModelMeta)
import CESDS.Types.Value (realValue)
import CESDS.Types.Variable (makeVarMeta)
import Control.Arrow (second)
import Data.List.Split (chunksOf)
import Data.UUID (toString)
import Data.UUID.V5 (generateNamed, namespaceURL)
import System.Environment (getArgs)
import System.Random (getStdGen, randoms)


-- | Action for running the server.
main :: IO ()
main =
  do
    [host, port, persistence, chunkSize] <- getArgs -- FIXME
    xs <- chunksOf 5 . randoms <$> getStdGen
    serverMain host (read port) (Just $ read chunkSize)
      =<< makeInMemoryManager (Just persistence) ()
      (
        \() ->
          return
            (
              [
                makeModelMeta
                  (toString . generateNamed namespaceURL $ toEnum . fromEnum <$> "random")
                  "random data"
                  "http://random.nrel.gov"
                  (zipWith makeVarMeta [1..] ["x", "y", "z", "u", "v"])
                  []
              ]
            , ()
            )
      )
      (
        \() _ ->
          return
            (
              second (fmap (second realValue) . zip [1..]) <$> zip [1..] xs
            , ()
            )
      )
      (
        error "Static data only."
      )