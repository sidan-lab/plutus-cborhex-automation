{-# LANGUAGE OverloadedStrings #-}

module ParamScriptAPI where

import qualified TestingValidator    as TV
import qualified ParamDemoValidator  as PDV

import qualified ContributorToken.ReferenceValidator as CTRV
import qualified GBTE.TreasuryValidator              as GBTETV
import qualified GBTE.EscrowValidator                as GBTEEV

import qualified SIDANPlutusServer   as SIDAN
main :: IO ()
main = SIDAN.createServer app

app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "testing-validator" $ SIDAN.mkV1Validator TV.validator
  SIDAN.createEndpoint "param-validator" $ SIDAN.mkV1Validator PDV.validator
  SIDAN.createEndpoint "contributor-token" $ SIDAN.mkV2Validator CTRV.validator
  SIDAN.createEndpoint "gbte-treasury-validator" $ SIDAN.mkV2Validator GBTETV.validator
  SIDAN.createEndpoint "gbte-escrow-validator" $ SIDAN.mkV2Validator GBTEEV.validator
