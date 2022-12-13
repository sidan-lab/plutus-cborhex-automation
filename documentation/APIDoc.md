# SIDAN Plutus Server

## Setup

1. Include the `source-repository-package` in the `cabal.project` for your Plutus project:

```
source-repository-package
  type: git
  location: https://github.com/SIDANWhatever/plutus-cborhex-automation
  tag: 93af478c89e861a4803acad0a194bcbec73dfd8f
```

> Note: This checkout tag is corresponding to `next-node` tag of `plutus-apps`. To avoid package version conflict, please follow the same exact content of `cabal.project` inside `example` sub-dir (remember to change the `packages` tag on line 4).

2. Include the `sidan-plutus-server` in your `build-depends`
3. Import the package

```haskell
import qualified SIDANPlutusServer   as SIDAN
```

## APIs

1. `createServer`

   > To create the server instance, example:

2. `createEndpoint`

   > To create a POST request endpoint for custom route, example:

3. `mkV1Validator`

   > To make PlutusV1 Validator, which is to be attached to one custom endpoint

4. `mkV2Validator`

   > To make PlutusV2 Validator, which is to be attached to one custom endpoint

5. `mkV1MintingPolicy`

   > To make PlutusV1 MintingPolicy, which is to be attached to one custom endpoint

6. `mkV2MintingPolicy`
   > To make PlutusV2 MintingPolicy, which is to be attached to one custom endpoint

### Important note: You have to derive `Generic`, `FromJSON` and `ToJSON` instances for your custom param in order to utilize the above APIs

> IOG has wrapped some of its `FromJSON` and `ToJSON` instances in their Orphans, in case an error of no instances found for some Plutus data type, you can do `import SIDANDefaultOrphans()`, where we help to catch those orphans import. It is also always recommended.

```haskell
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import SIDANDefaultOrphans()

data TestParam = TestParam {
  testNumber :: Integer,
  testPpkh   :: PaymentPubKeyHash
  }
  deriving (Show, Generic, FromJSON, ToJSON)
```

## Example usage of all the above APIs

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ParamScriptAPI where

import qualified SIDANPlutusServer   as SIDAN
import qualified V1Validator         as V1V
import qualified V2Validator         as V2V
import qualified V1Validator         as V1V
import qualified V1Validator         as V1V

main :: IO ()
main = SIDAN.createServer app

app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "your-custom-route-1" $ SIDAN.mkV1Validator V1V.validator
  SIDAN.createEndpoint "your-custom-route-2" $ SIDAN.mkV2Validator V2V.validator
  SIDAN.createEndpoint "your-custom-route-3" $ SIDAN.mkV1MintingPolicy V1MP.mintingPolicy
  SIDAN.createEndpoint "your-custom-route-4" $ SIDAN.mkV2MintingPolicy V2MP.mintingPolicy
```

## Endpoint usage example

Let's say you have a custom parameter of your V1 validator like below:

```haskell
data TestParam = TestParam {
  testNumber :: Integer,
  testPpkh   :: PaymentPubKeyHash
  }
  deriving (Show, Generic, FromJSON, ToJSON)
```

And you have implemented `sidan-plutus-server` like below:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ParamScriptAPI where

import qualified SIDANPlutusServer   as SIDAN
import           V1Validator         as V1V

main :: IO ()
main = SIDAN.createServer app

app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "get-parameterized-script" $ SIDAN.mkV1Validator V1V.validator
```

After your server is up:

```
Spock is running on port 8080
```

Your can craft the body of your post request like this:

```json
{
  "testNumber": 245324,
  "testPpkh": {
    "unPaymentPubKeyHash": {
      "getPubKeyHash": "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73"
    }
  }
}
```

You can see we have to wrapped our `String` input from JSON body to the type `PaymentPubKeyHash`, while it is not needed for `Integer` typed param. Full list of endpoint crafting guideline for common Plutus parameters could be found [here](EndpointGuide.md)

## Supported `plutus-apps` checkout tags:

1. Release tag: 93af478c89e861a4803acad0a194bcbec73dfd8f
   - `plutus-apps` checkout tag: `next-node` (97b4c1da03faf9bc35f348802fb7927231657e75)

### Please request if you want us to support for `plutus-apps` checkout tag for your Dapp!
