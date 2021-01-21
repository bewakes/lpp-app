{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty

import LPP(LPP, getInitialState, accumulateStates)


main :: IO ()
main = do
    scotty 4000 $ do
        get "/" $ do
            html "<h1>This is from scotty</h1>"
        post "/" $ do
            lppRequest <- jsonData :: ActionM LPP
            json $ accumulateStates (getInitialState lppRequest)
