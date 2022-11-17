# Example Usage of Plutus-Cborhex-Generator backend

This is the directory showing the example usage of the Plutus-Cborhex-Generator backend.

## Getting start
1. Make sure you have all dependencies needed, including setting up `nix`, entering `nix-shell` using `v1.0.0-alpha1` tag of `plutus-apps`.
2. Run `cabal build` to build the modules needed
3. Simply run `cabal run example-validator.cabal` then you have your backend setup on localhost.

## The example plutus script
This is just a demo parameterized script taking in an `Integer` and a `PaymentPubKeyHash` as parameter. Helper functions are built in order to take `Integer` and a `String` of `Bech32` address to build the PlutusV1 script.

## Documentation on VM Endpoints Setup Please Find [VMSetup.md](VMSetup.md)