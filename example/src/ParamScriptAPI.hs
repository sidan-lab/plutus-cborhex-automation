{-# LANGUAGE OverloadedStrings #-}

module ParamScriptAPI where

import qualified SIDANPlutusServer   as SIDAN
import           TestingValidator

main :: IO ()
main = SIDAN.createServer app

app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "validatorV1" $ SIDAN.mkV1Validator validator
  SIDAN.createEndpoint "validatorV2" $ SIDAN.mkV2Validator validator
