#!/usr/bin/env bash

source ./functions.sh

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

###############################################
# Set NFT Parameters
TOKEN_DESCRIPT="Genesis Cardano Beam Event"
TOKEN_NAME="Lisbon Algarve 2022"
IPFS_LINK="ipfs://QmVaUwbA63nepvWJpAAVZi1xnKjB3rz4yXHFd3WvWW17Jp"
TOKEN_AMOUNT=7 # How many tokens to mint

WALLET_TXHASH=5178864ffe290059984ebe18d3bd2cc2ccb0101ab32050caf57ebda581346e50 # find a suitable UTXO TxHash
WALLET_TXIX=0 # find a suitable UTXO TxIx
COLLATERAL_TX=58d5fb261fda9d8e4fea9dbcd8cc98b16e4cba566da47ab2423c9e99b2616f6c#0 # Set a UTXO with at least 5 ADA to be used as Collateral

NFT_MINTER_CLI=nft-minter-cli
WALLET_UTXO=$WALLET_TXHASH#$WALLET_TXIX
TOKEN_NAME_HEX=$(echo -n $TOKEN_NAME | xxd -b -ps -c 80 | tr -d '\n')
WALLET_ADDR=$(cat $WALLETS_DIR/01.addr)

###############################################
# Generate the Minting Policy Hash
# The NFT Minter takes in 4 parameters:
# - UTXO Hash
# - UTXO Txix (usually)
# - Token Name (aka NFT Name)
# - Amount to mint, must be an Integer (whole number)

echo "----- Output from Plutus NFT-MINTER -----"
(cd ../data && $NFT_MINTER_CLI "$WALLET_TXHASH" $WALLET_TXIX "$TOKEN_NAME" $TOKEN_AMOUNT)

TOKEN_MPH=$(cat $DATA_DIR/mintHash.dat)


SCRIPT_FILE=$DATA_DIR/mintPolicyJSON.dat

###############################################
# Create metadata.json
# this stores the image of the NFT and desctiption

echo "{" > $DATA_DIR/metadata.json
echo "    \"721\": {" >> $DATA_DIR/metadata.json
echo "        \"$TOKEN_MPH\": {" >> $DATA_DIR/metadata.json
echo "          \"$TOKEN_NAME\": {" >> $DATA_DIR/metadata.json
echo "            \"name\": \"$TOKEN_NAME\"," >> $DATA_DIR/metadata.json
echo "            \"image\": \"$IPFS_LINK\"," >> $DATA_DIR/metadata.json
echo "            \"mediaType\": \"image\"," >> $DATA_DIR/metadata.json
echo "            \"description\": \"$TOKEN_DESCRIPT\"" >> $DATA_DIR/metadata.json 
echo "          }" >> $DATA_DIR/metadata.json
echo "        }," >> $DATA_DIR/metadata.json
echo "        \"version\": \"1.0\"" >> $DATA_DIR/metadata.json
echo "      }" >> $DATA_DIR/metadata.json
echo "}" >> $DATA_DIR/metadata.json

###############################################
# Do the Minting
# Submit the Minting transaction 
# to the blockchain

# Get network parameters

$CARDANO_CLI query protocol-parameters $NETWORK > params.json

echo "----- Output from CARDANO-CLI -----"

# Build the transaction
$CARDANO_CLI transaction build \
 --babbage-era \
 --cardano-mode \
  $NETWORK \
 --tx-in $WALLET_UTXO \
 --tx-in-collateral $COLLATERAL_TX \
 --mint-script-file $SCRIPT_FILE \
 --mint-redeemer-file $DATA_DIR/typedRedeemer.dat \
 --tx-out "$WALLET_ADDR+1200000 + $TOKEN_AMOUNT $TOKEN_MPH.$TOKEN_NAME_HEX" \
 --mint "$TOKEN_AMOUNT $TOKEN_MPH.$TOKEN_NAME_HEX" \
 --change-address $WALLET_ADDR \
 --metadata-json-file $DATA_DIR/metadata.json  \
 --protocol-params-file params.json \
 --out-file tx.build

# Sign the transaction

$CARDANO_CLI transaction sign \
--tx-body-file tx.build \
--signing-key-file $WALLETS_DIR/01.skey \
--out-file tx.signed \
$NETWORK

echo "Transaction UTXO: " $($CARDANO_CLI transaction txid --tx-file tx.signed)

# Submit the transaction to the blockchain
$CARDANO_CLI transaction submit --tx-file tx.signed $NETWORK