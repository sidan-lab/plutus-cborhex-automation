# Revision history for plutus-cborhex-automation

## 0.0.0.1 -- 2022-10-12

- First version. Released on an unsuspecting world.
- branch `main` builds locally and on a remote server; `cabal run` locally produces

## 0.1.0.0 -- 2022-11-18

- Test release with 6 endpoints:
  1. `createServer`
  2. `createEndpoint`
  3. `mkV1Validator`
  4. `mkV2Validator`
  5. `mkV1MintingPolicy`
  6. `mkV2MintingPolicy`
- Supported checkout tags:
  1. Release tag: fb044b39dd8cbfe166b0fec376038f7980c3a398
     - `plutus-apps` checkout tag: `next-node` (97b4c1da03faf9bc35f348802fb7927231657e75)

## 0.1.0.1 -- 2022-11-22

- Adding ToJSON & FromJSON instances for below Plutus data types:
  1. `TokenName`
  2. `CurrencySymbol`
  3. `AssetClass`
  4. `POSIXTime`
  5. `Address`
  6. `Credential`
  7. `PubKeyHash`
  8. `ValidatorHash`
  9. `StakingCredential`
- Supported checkout tags:
  1. Release tag: dc122c4524f3c4bb34e2a8a951f0630554b14ba0
     - `plutus-apps` checkout tag: `next-node` (97b4c1da03faf9bc35f348802fb7927231657e75)
