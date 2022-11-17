# Supplying JSON body endpoints for SIDAN Plutus Server

## `PaymentPubKeyHash`
- Parameter type
```haskell
data TestParam = TestParam {
  testNumber :: Integer,
  testPpkh   :: PaymentPubKeyHash
  }
  deriving (Show, Generic, FromJSON, ToJSON)
```

- JSON body
```json
{
	"testNumber": 245324,
	"testPpkh": {"unPaymentPubKeyHash": {"getPubKeyHash": "8f2ac4b2a57a90feb7717c7361c7043af6c3646e9db2b0e616482f73"}}
}
```