{-# LANGUAGE OverloadedStrings #-}

module ParamScriptAPI where

import qualified TestingValidator    as TV
import qualified ParamDemoValidator  as PDV

import qualified SIDANPlutusServer   as SIDAN
import qualified ContributorToken.ReferenceValidator as CTRV

main :: IO ()
main = SIDAN.createServer app

app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "testing-validator" $ SIDAN.mkV1Validator TV.validator
  SIDAN.createEndpoint "param-validator" $ SIDAN.mkV1Validator PDV.validator
  SIDAN.createEndpoint "contributor-token" $ SIDAN.mkV2Validator CTRV.validator
