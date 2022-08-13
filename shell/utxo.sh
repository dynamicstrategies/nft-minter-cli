#!/usr/bin/env bash

source ./functions.sh

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# Check if command line argument is empty or not present
if [ -z $1 ]
then
    file=$DATA_DIR/addrBech32.dat
    ADDR=$(head -n 1 $file)
else
    ADDR=$1
fi


echo "querying UTXO at: " $ADDR

$CARDANO_CLI query utxo --address $ADDR $NETWORK