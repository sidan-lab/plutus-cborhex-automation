# Example Usage of Plutus-Cborhex-Generator backend

This is the directory showing the example usage of the Plutus-Cborhex-Generator backend.

## Getting start
1. Make sure you have all dependencies needed, including setting up `nix`, entering `nix-shell` using `next-node` (97b4c1da03faf9bc35f348802fb7927231657e75) tag of `plutus-apps`.
2. Run `cabal build` to build the modules needed
3. Simply run `cabal run sidan-plutus-server` then you have your backend setup on localhost.

## The example plutus script
This example would be setup for 3 instances of Gimbalabs:
1. [Token Gated Faucet from PPBL](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/tree/master/project-301-faucet)
2. [Treasury and Escrow contracts as in GPTE](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/projects/gpte/gpte-plutus-v2)
3. [Rails for CIP-68 style metadatum token pairs](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/projects/completion-token/ppbl-contributor-token)

## Documentation on VM Endpoints Setup Please Find [Deployment.md](../documentation/Deployment.md)
