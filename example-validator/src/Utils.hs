{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       as Hash (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Codec.Serialise             (serialise)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (pack)
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import qualified Ledger                      as Plutus
import           PlutusTx.Builtins.Class
import           TestingValidator as SimTest

makeTestParam :: Integer -> String -> TestParam
makeTestParam num addr = TestParam { testNumber = num, testPpkh = addrToPpkh addr}

addrToPpkh :: String -> Plutus.PaymentPubKeyHash
addrToPpkh = unsafePaymentPubKeyHash . unsafeReadAddress

getCredentials :: Plutus.Address -> Maybe (Plutus.PaymentPubKeyHash, Maybe Plutus.StakePubKeyHash)
getCredentials (Plutus.Address x y) = case x of
    ScriptCredential _   -> Nothing
    PubKeyCredential pkh ->
      let
        ppkh = Plutus.PaymentPubKeyHash pkh
      in
        case y of
            Nothing                        -> Just (ppkh, Nothing)
            Just (Plutus.StakingPtr {}) -> Nothing
            Just (StakingHash h)           -> case h of
                ScriptCredential _    -> Nothing
                PubKeyCredential pkh' -> Just (ppkh, Just $ Plutus.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: Plutus.Address -> Plutus.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error $ "script address " ++ show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeReadAddress :: String -> Plutus.Address
unsafeReadAddress s = fromMaybe (error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (Hash.ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p _)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = Nothing
        -- , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

writeValidator :: FilePath -> Plutus.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.unValidatorScript

writeTestingValidator :: Integer -> String -> IO (Either (FileError ()) ())
writeTestingValidator num addr = writeValidator "scripts/testingValidator.plutus" $ SimTest.validator $ makeTestParam num addr

writeExampleValidator = writeTestingValidator 123 "addr_test1qqknp3kr3y55ej9a5680w939ltpe8752zjkj67hemegmeldx8s2nlvl23f82fut92a82jytnw4k7p0esygk2p626vjdquna9wq"
