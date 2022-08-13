# NFT Minter CLI

Based on the https://github.com/input-output-hk/plutus-starter , which is starter repo from IOHK with the boilerplate for smart contract development

To a large extent this NFT Minter CLI is a worked example of how the IOHK starter repo can be used to build something useful.

We need a minting functionality for our project and thought that others might find it useful.


## 1. Debugging and Installing

### Clone the Repo onto your local machine
`git clone https://github.com/dynamicstrategies/nft-minter-cli.git`


### Install nix-shell
Using nix-shell ensures you get the complete environment out of the box and have less to worry about importing required Plutus libraries

To install nix-shell, go here: https://nixos.org/download.html
I recommend installing the "Single-user installation," which avoids setting up different user groups and granting permissions. I found that it is easier to uninstall nix-shell from this set-up in the future.

### Open nix-shell
Make sure you are in the root directory of the repo that you cloned, then run `nix-shell`

The first time you run nix-shell it can take a few minutes up to an hour to download everything.
Subsequent launches will take much less time, a couple of minutes at most.


### Debug the code
Run `cabal repl` from the root directory.

Ensure you do it after you have launched nix-shell and your command line starts with `[nix-shell: ...`

### Test run

Run `cabal run nft-minter-cli -- "c8650bafb70de1caf55b1c66c7aee396225a82dbf7eeb71a9d8ad1f2cf1064dd" 0 "HEY" 5` to test if the CLI is working as expected. Note you need to include the -- before the packaged name and the passed parameters

### Build and Install

If you want to use this in production, then you need to generate an executable that runs in a fraction of a second
To do this run:

```
cabal build
cabal install --overwrite-policy=always
```

Ensure you are within the nix-shell, and your command line starts with `[nix-shell: ...`

After the install has been run it will tell you the path where the executable has been placed. You can go to that path and run:

`./nft-minter-cli "c8650bafb70de1caf55b1c66c7aee396225a82dbf7eeb71a9d8ad1f2cf1064dd" 0 "HEY" 5`


## 2. Minting

### Check UTXO

You need to find a UTXO to consume for the minting. The protocol fees will be paid out of this UTXO

```
cd shell
./utxo.sh $(cat ../wallets/01.addr)
```

### Mint

The file `mint.sh` does all the heavy lifting.

It does 4 things:
- Defines the Token Name, quantity, description, image
- Generates the smart contract used in the minting by querying the NFT Minter CLI from above. This ensures that the NFTs are unique (their policyIds will never be repeated)
- Generated the Metadata that is stored On-chain and describes what the NFT is
- Generates and Submits the Minting transaction to the blockchain

Once the NFT is minted at the last step above, it is then automatically sent back to the user's wallet.

### Testnet

The `mint.sh` is configured for minting on the TESTNET, to change it so it does the minting on the MAINNET change `NETWORK="--testnet-magic 1097911063"` to `NETWORK="--mainnet"`

The smart contract and minting scripts are compatible after the Alonzo hard fork.
