{-# LANGUAGE OverloadedStrings #-}

module ParamScriptAPI where

-- import           TestingValidator
import qualified SIDANPlutusServer   as SIDAN
import qualified ContributorToken.ReferenceValidator as CTRV

main :: IO ()
main = SIDAN.createServer app

app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "contributor-token" $ SIDAN.mkV2Validator CTRV.validator
  -- SIDAN.createEndpoint "validatorV1" $ SIDAN.mkV1Validator validator
  -- SIDAN.createEndpoint "validatorV2" $ SIDAN.mkV2Validator validator

