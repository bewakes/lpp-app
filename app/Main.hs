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
        options "/" $ do
            setHeader "Access-Control-Allow-Origin" "*"
            setHeader "Access-Control-Allow-Headers" "content-type"
        post "/" $ do
            setHeader "Access-Control-Allow-Origin" "*"
            lppRequest <- jsonData :: ActionM LPP
            json $ accumulateStates (getInitialState lppRequest)
