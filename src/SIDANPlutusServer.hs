{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module SIDANPlutusServer where

import           Cardano.Api                       as API
import qualified Cardano.Api.SerialiseTextEnvelope as SAPI
import           Cardano.Api.Shelley               (PlutusScript (..))
import           Codec.Serialise                   (serialise)
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Short             as SBS
import           Data.HVect                        hiding (pack)
import           Data.Text                         (pack)
import qualified Plutus.V1.Ledger.Scripts          as PlutusV1
import qualified Plutus.V2.Ledger.Api              as PlutusV2
import           Web.Spock
import           Web.Spock.Config

type Api = SpockM () () () ()

createServer :: Web.Spock.SpockM () () () () -> IO ()
createServer app = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

createEndpoint :: Data.HVect.HasRep xs =>
     Path xs ps
     -> Data.HVect.HVectElim xs (SpockActionCtx ctx conn sess st ())
     -> SpockCtxM ctx conn sess st ()
createEndpoint path = do post path

-- Validator

writeV1Validator :: PlutusV1.Validator -> LBS.ByteString
writeV1Validator = SAPI.textEnvelopeToJSON @(PlutusScript PlutusScriptV1) Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . PlutusV1.unValidatorScript

mkV1Validator :: (FromJSON a, ToJSON a) => (a -> PlutusV1.Validator) -> ActionCtxT () (WebStateM () () ()) b
mkV1Validator val = do
    scriptParam <- jsonBody'
    json $ pack $ show $ writeV1Validator $ val scriptParam

writeV2Validator :: PlutusV2.Validator -> LBS.ByteString
writeV2Validator = SAPI.textEnvelopeToJSON @(PlutusScript PlutusScriptV2) Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . PlutusV2.unValidatorScript

mkV2Validator :: (FromJSON a, ToJSON a) => (a -> PlutusV2.Validator) -> ActionCtxT () (WebStateM () () ()) b
mkV2Validator val = do
    scriptParam <- jsonBody'
    json $ pack $ show $ writeV2Validator $ val scriptParam

-- Minting Policy

writeV1MintingPolicy :: PlutusV1.MintingPolicy -> LBS.ByteString
writeV1MintingPolicy  = SAPI.textEnvelopeToJSON @(PlutusScript PlutusScriptV1) Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . PlutusV1.getMintingPolicy

mkV1MintingPolicy :: (FromJSON a, ToJSON a) => (a -> PlutusV2.MintingPolicy) -> ActionCtxT () (WebStateM () () ()) b
mkV1MintingPolicy val = do
    scriptParam <- jsonBody'
    json $ pack $ show $ writeV1MintingPolicy $ val scriptParam

writeV2MintingPolicy :: PlutusV2.MintingPolicy -> LBS.ByteString
writeV2MintingPolicy  = SAPI.textEnvelopeToJSON @(PlutusScript PlutusScriptV2) Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . PlutusV2.getMintingPolicy

mkV2MintingPolicy :: (FromJSON a, ToJSON a) => (a -> PlutusV2.MintingPolicy) -> ActionCtxT () (WebStateM () () ()) b
mkV2MintingPolicy val = do
    scriptParam <- jsonBody'
    json $ pack $ show $ writeV2MintingPolicy $ val scriptParam
