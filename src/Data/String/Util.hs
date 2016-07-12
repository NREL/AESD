{-# LANGUAGE OverloadedStrings #-}


module Data.String.Util (
  maybeString
) where


import Data.String (IsString)


maybeString :: (Eq a, IsString a) => a -> Maybe a
maybeString s
  | s == ""   = Nothing
  | otherwise = Just s
