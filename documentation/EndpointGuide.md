# Supplying JSON body endpoints for SIDAN Plutus Server

Here we have the guide of supplying JSON body for different Plutus common data type. Notice some wrappers are needed for the derived `FromJSON` instances. In the future we might reduce the wrappers needed by our custom instances implementation.

## Before start

> You can import our module `SIDANDefaultOrphans` to where you declare your custom parameter, which we helped to catch all `FromJSON` and `ToJSON` instances derivation developed by IOG.
>
> ```haskell
> import SIDANDefaultOrphans()
>
> data TestParam = TestParam {
>  testParam   :: TokenName
> } deriving (Show, Generic, FromJSON, ToJSON)
> ```

## JSON Body Examples

### `TokenName`

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: TokenName
} deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body

```json
{
  "testParam": { "unTokenName": "67696d62616c" }
}
```

### `CurrencySymbol`

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: CurrencySymbol
} deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body

```json
{
  "testParam": {
    "unCurrencySymbol": "2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30"
  }
}
```

### `AssetClass`

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: AssetClass
} deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body

```json
{
  "testParam": {
    "unAssetClass": [
      {
        "unCurrencySymbol": "2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30"
      },
      { "unTokenName": "67696d62616c" }
    ]
  }
}
```

### `POSIXTime`

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: POSIXTime
} deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body

```json
{
  "testParam": 1643235300000
}
```

### `Address`

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: Address
} deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body

```haskell
testParam :: Address
testParam = Address {
  addressCredential = PubKeyCredential "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73",
  addressStakingCredential = Just $ StakingPtr 1 2 3
}
```

```json
{
  "testParam": {
    "addressCredential": {
      "tag": "PubKeyCredential",
      "contents": {
        "getPubKeyHash": "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73"
      }
    },
    "addressStakingCredential": { "tag": "StakingPtr", "contents": [1, 2, 3] }
  }
}
```

### `ValidatorHash`

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: ValidatorHash
} deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body

```json
{
  "testParam": "28700837facbdbc51cff052012ed1c3286112c786d0ce2f019525c76"
}
```

### `StakingCredential`

TODO: Adding in sensible example content

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: StakingCredential
} deriving (Show, Generic, FromJSON, ToJSON)
```

- Example 1

```haskell
testParam :: StakingCredential
testParam = StakingHash (ScriptCredential 28700837facbdbc51cff052012ed1c3286112c786d0ce2f019525c76)
```

```json
{
  "testParam": {
    "tag": "StakingHash",
    "contents": {
      "tag": "ScriptCredential",
      "contents": "28700837facbdbc51cff052012ed1c3286112c786d0ce2f019525c76"
    }
  }
}
```

- Example 2

```haskell
testParam :: StakingCredential
testParam = StakingPtr 1 2 3
```

```json
{
  "testParam": { "tag": "StakingPtr", "contents": [1, 2, 3] }
}
```

### `PaymentPubKeyHash`

- Parameter type

```haskell
data TestParam = TestParam {
  testParam   :: PaymentPubKeyHash
} deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body

```json
{
  "testParam": {
    "unPaymentPubKeyHash": {
      "getPubKeyHash": "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73"
    }
  }
}
```

## A Complete Example:

For our `ParamDemoValidator.hs`, we could supply the below json for the endpoint call:

```json
{
  "testTN": { "unTokenName": "67696d62616c" },
  "testCS": {
    "unCurrencySymbol": "2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30"
  },
  "testAC": {
    "unAssetClass": [
      {
        "unCurrencySymbol": "2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30"
      },
      { "unTokenName": "67696d62616c" }
    ]
  },
  "testTime": 1643235300000,
  "testAddr": {
    "addressCredential": {
      "tag": "PubKeyCredential",
      "contents": {
        "getPubKeyHash": "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73"
      }
    }
  },
  "testPkh": {
    "getPubKeyHash": "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73"
  },
  "testVH": "",
  "testSC": {
    "tag": "StakingHash",
    "contents": {
      "tag": "ScriptCredential",
      "contents": "28700837facbdbc51cff052012ed1c3286112c786d0ce2f019525c76"
    }
  },
  "testNumber": 123,
  "testPpkh": {
    "unPaymentPubKeyHash": {
      "getPubKeyHash": "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73"
    }
  }
}
```
