{-# LANGUAGE OverloadedStrings #-}

module Dev.DevInstances where

-- Below import for JSON instances implementations
import qualified Plutus.V1.Ledger.Value            as PlutusV1
import qualified Plutus.V2.Ledger.Api              as PlutusV2
import           Data.Aeson ((.:), (.:?), (.=), parseJSON, withObject, toJSON, toEncoding, object, pairs, ToJSON, FromJSON)
import           Ledger.Builtins.Orphans
import           Cardano.Ledger.Crypto

-- JSON Instances

instance FromJSON PlutusV2.CurrencySymbol where
  parseJSON = withObject "CurrencySymbol" $ \v -> do
    cs <- v .: "unCurrencySymbol"
    return (PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = cs })

instance ToJSON PlutusV2.CurrencySymbol where
    toJSON (PlutusV2.CurrencySymbol cs) =
        object ["unCurrencySymbol" .= cs]

    toEncoding (PlutusV2.CurrencySymbol cs) =
        pairs ("unCurrencySymbol" .= cs)

instance FromJSON PlutusV2.TokenName where
  parseJSON = withObject "TokenName" $ \v -> do
    cs <- v .: "unTokenName"
    return (PlutusV2.TokenName { PlutusV2.unTokenName = cs })

instance ToJSON PlutusV2.TokenName where
    toJSON (PlutusV2.TokenName cs) =
        object ["unTokenName" .= cs]

    toEncoding (PlutusV2.TokenName cs) =
        pairs ("unTokenName" .= cs)

-- TODO: To be tested below
instance FromJSON PlutusV1.AssetClass where
  parseJSON = withObject "AssetClass" $ \v -> do
    ac <- v .: "unAssetClass"
    return (PlutusV1.AssetClass { PlutusV1.unAssetClass = ac })

instance ToJSON PlutusV1.AssetClass where
    toJSON (PlutusV1.AssetClass ac) =
        object ["unAssetClass" .= ac]

    toEncoding (PlutusV1.AssetClass ac) =
        pairs ("unAssetClass" .= ac)

instance FromJSON PlutusV2.POSIXTime where
  parseJSON = withObject "POSIXTime" $ \v -> do
    pst <- v .: "getPOSIXTime"
    return (PlutusV2.POSIXTime { PlutusV2.getPOSIXTime = pst })

instance ToJSON PlutusV2.POSIXTime where
    toJSON (PlutusV2.POSIXTime pst) =
        object ["getPOSIXTime" .= pst]

    toEncoding (PlutusV2.POSIXTime pst) =
        pairs ("getPOSIXTime" .= pst)

instance FromJSON PlutusV2.Address where
  parseJSON = withObject "Address" $ \v -> do
    ac <- v .: "addressCredential"
    asc <- v .:? "addressStakingCredential"
    return (PlutusV2.Address { PlutusV2.addressCredential = ac, PlutusV2.addressStakingCredential = asc })

instance ToJSON PlutusV2.Address where
    toJSON (PlutusV2.Address ac asc) =
        object ["addressCredential" .= ac, "addressStakingCredential" .= asc]

    toEncoding (PlutusV2.Address ac asc) =
        pairs ("addressCredential" .= ac <> "addressStakingCredential" .= asc)

instance FromJSON PlutusV2.Credential where
  parseJSON = withObject "PubKeyCredential" $ \v -> do
    pkh <- v .:? "PubKeyHash"
    case pkh of
      Just pkh' -> return (PlutusV2.PubKeyCredential pkh')
      Nothing   -> do
        vh <- v .: "ValidatorHash"
        return (PlutusV2.ScriptCredential vh)
    
instance ToJSON PlutusV2.Credential where
    toJSON (PlutusV2.PubKeyCredential pkh) =
        object ["PubKeyHash" .= pkh]
    toJSON (PlutusV2.ScriptCredential vh) =
        object ["ValidatorHash" .= vh]

    toEncoding (PlutusV2.PubKeyCredential pkh) =
        pairs ("PubKeyHash" .= pkh)
    toEncoding (PlutusV2.ScriptCredential vh) =
        pairs ("ValidatorHash" .= vh)

instance FromJSON PlutusV2.PubKeyHash where
  parseJSON = withObject "PubKeyHash" $ \v -> do
    pkh <- v .: "getPubKeyHash"
    return (PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = pkh })

instance ToJSON PlutusV2.PubKeyHash where
    toJSON (PlutusV2.PubKeyHash pkh) =
        object ["getPubKeyHash" .= pkh]

    toEncoding (PlutusV2.PubKeyHash pkh) =
        pairs ("getPubKeyHash" .= pkh)

instance FromJSON PlutusV2.ValidatorHash where
  parseJSON = withObject "ValidatorHash" $ \v -> do
    vh <- v .: "ValidatorHash"
    return (PlutusV2.ValidatorHash vh)

instance ToJSON PlutusV2.ValidatorHash where
    toJSON (PlutusV2.ValidatorHash vh) =
        object ["ValidatorHash" .= vh]

    toEncoding (PlutusV2.ValidatorHash vh) =
        pairs ("ValidatorHash" .= vh)

instance FromJSON PlutusV2.StakingCredential where
  parseJSON = withObject "StakingCredential" $ \v -> do
    sh <- v .:? "StakingHash"
    case sh of
      Just sh' -> return (PlutusV2.StakingHash sh')
      Nothing  -> do
        sp <- v .: "StakingPtr"
        int1 <- sp .: "int1"
        int2 <- sp .: "int2"
        int3 <- sp .: "int3"
        return (PlutusV2.StakingPtr int1 int2 int3)

instance ToJSON PlutusV2.StakingCredential where
    toJSON (PlutusV2.StakingHash sh) =
        object ["StakingHash" .= sh]
    toJSON (PlutusV2.StakingPtr int1 int2 int3) =
        object ["StakingPtr" .= [int1, int2, int3]]

    toEncoding (PlutusV2.StakingHash sh) =
        pairs ("StakingHash" .= sh)
    toEncoding (PlutusV2.StakingPtr int1 int2 int3) =
        pairs ("StakingPtr" .= [int1, int2, int3])