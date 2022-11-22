{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GBTE.Compiler where

import              Prelude (FilePath, IO)
import              Cardano.Api
import              Cardano.Api.Shelley             ( PlutusScript (..), PlutusScriptV2 )
import              Codec.Serialise                 (serialise)
import              Data.Aeson
import qualified    Data.ByteString.Lazy            as LBS
import qualified    Data.ByteString.Short           as SBS
import qualified    Plutus.V1.Ledger.Scripts
import qualified    Plutus.V1.Ledger.Value
import qualified    Plutus.V2.Ledger.Api
import qualified    Plutus.V2.Ledger.Contexts
import qualified    PlutusTx
import              PlutusTx.Prelude


import qualified GBTE.TreasuryValidator as Treasury
import qualified GBTE.EscrowValidator as Escrow
import GBTE.Types

-- If we do not import Ledger, then
-- how to replace Ledger.Validator?

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript = writeValidator "output/escrow-gbte-v2-with-bounty-hash2.plutus" $ Escrow.validator $ BountyParam
    {
      bountyTokenPolicyId     = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , bountyTokenName         = "tGimbal"
    , accessTokenPolicyId     = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , treasuryIssuerPolicyId  = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/treasury-gbte-v2-with-bounty-hash2.plutus" $ Treasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , bountyContractHash   = "98d2b58a4eff29e8e9f9d16fb2fb0ac4146d0b0772a417daab840a74"
    , tBountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName     = "tGimbal"
    , tIssuerPolicyId      = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
    }
