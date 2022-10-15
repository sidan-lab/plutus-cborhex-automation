### TESTING
## Getting start
1. Make sure you have all dependencies needed, including setting up `nix`, entering `nix-shell` using `v1.0.0-alpha1` tag of `plutus-apps`.
2. Run `cabal build` to build the modules needed
3. Simply run `cabal run example-validator.cabal` then you have your backend setup on localhost.

## The example plutus script
This is just a demo parameterized script taking in an `Integer` and a `PaymentPubKeyHash` as parameter. Helper functions are built in order to take `Integer` and a `String` of `Bech32` address to build the PlutusV1 script.

# plutus-cborhex-automation
### [Miro board for brainstorming](https://miro.com/app/board/uXjVPPxeTj4=/?share_link_id=943831871410)

This is an open-source project supported by [Gimbalabs](https://discord.gg/2Qz73CjSxz) to create RESTful API Endpoint for Compiling Plutus Scripts.

## Why we need this?
In Cardano's eUTxO model, parameterized script is heavily used for real scalable smart contract. With different parameters supplied to the same validator function, it would return multiple plutus `Validator` and generate multiple `cborHex`. With different `cborHex` created, different script addresses could be derived. It is a best practice to split users who never interact with each others into different places, which avoid multiple issues such as [double satifaction](https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/double-satisfaction.html) etc.

However, in the available education resources most script `cborHex`s are created through the `Utils.hs` function inside Haskell library. In the real Dapp production, user cannot get the script calculated easily on the frontend. Therefore, currently there is still gap implementing such Plutus best practice. This project is trying to reduce the pain in between, building a module or service which could help Plutus developers to easily convert their Plutus cborHex generating functions into RESTful API endpoints, which are easily interacted with the client side.


In the long-term, an API for compiling plutus scripts can be served via Dandelion. In addition to being an essential tool, this project can serve as an example of how we can extend Dandelion.


## Milestones


### Milestone 0: Study the problem
- Exploring different alternatives such as [Helios](https://github.com/Hyperion-BT/Helios)
- Testing out different Haskell backend libraries and dockerize functions for simple function to see if there is any major blocker.
- Set the workflow for the rest of the research.


### Milestone 1:
Developers can use a Docker image to create a REST endpoint that returns CBOR and Address for any pre-defined, parameterized Plutus Validator.
Upon delivery of Docker image, code repo and documentation.
#### Deliverables:
- Open source repository
- Docker Image
- Developer-Facing Documentation for the following steps:
1. How to add an arbitrary contract to Docker image
2. How to specify Validator Parameters related to that Contract
3. How to use local endpoint that takes Parameters and returns CBOR + Address


### Milestone 2:
Outcomes:
- Use the library to build exposed endpoints for creating GBTE Instances
- Add this example microservice to Dandelion
#### Deliverables:
- Public endpoint for creating GBTE Contract Instance


## What's next
If we’re successful on all of the above, we can take the final step of using this service as a first step to putting reference scripts on-chain. (Will be easy after accomplishing all of the above!). This will also serve as a hands-on example that can be used in upcoming Dandelion PBL course.
