{- ########################################
 BOILER PLATE CODE - START
 ########################################## -}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- Haskell Imports
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy.Char8 as CL
import System.IO (writeFile)
import System.Environment
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Plutus Imports
import Cardano.Api (writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Ledger.Ada (Ada (Lovelace), fromValue)
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (TokenName(..), Value, CurrencySymbol, mpsSymbol, flattenValue)
import Ledger.Value qualified as Value
import Ledger.TxId (TxId(..))
import Plutus.Script.Utils.V1.Scripts (MintingPolicy, mintingPolicyHash)
import Plutus.V1.Ledger.Api (ScriptContext (..), ScriptPurpose (..))
import Plutus.V1.Ledger.Contexts qualified as Validation
import Plutus.V1.Ledger.Scripts (mkMintingPolicyScript, Script, unMintingPolicyScript, Validator(..), MintingPolicyHash)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Prelude hiding (pred, ($), (&&), (||), (<), (==), not, any)
import Plutus.V1.Ledger.Contexts (TxOutRef(..), TxInfo(..), TxInInfo(..))

-- Custom Imports
import Utils (handleHexConversion)
import WriteTypedData (typedDataToJSON)

{- ########################################
 BOILER PLATE CODE - END
 ########################################## -}

 

data NftParams = NftParams
        { 
          nftTokenName  :: TokenName
        , nftAmount     :: Integer
        , nftTxOutRef   :: TxOutRef
        } 
    deriving (Prelude.Show)


--- Essential Smart contract Logic below ----


{-# INLINABLE mkPolicyNft #-}
mkPolicyNft :: NftParams -> () -> ScriptContext -> Bool
mkPolicyNft nftParams _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                               traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == nftTxOutRef nftParams) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == nftTokenName nftParams && amt' == nftAmount nftParams
        _                -> False


--- Essential Smart contract Logic above ----


{- ########################################
 
 BOILER PLATE CODE - STARTS
 
 If you leave the below part alone it should just work
 That code takes care of converting Haskell into Plutus core
 And generating the CBORHex , PlolicyIds ...


 ########################################## -}

policy :: NftParams -> MintingPolicy
policy nftParams = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [|| \c -> Scripts.mkUntypedMintingPolicy (mkPolicyNft c) ||]) `PlutusTx.applyCode` PlutusTx.liftCode nftParams


PlutusTx.makeLift ''NftParams
PlutusTx.unstableMakeIsData ''NftParams

plutusScript :: NftParams -> Script
plutusScript p = unMintingPolicyScript $ policy p

validator :: NftParams -> Validator
validator p = Validator $ unMintingPolicyScript $ policy p

scriptAsCbor :: NftParams -> LB.ByteString
scriptAsCbor p = serialise $ validator p

mintingScript :: NftParams -> PlutusScript PlutusScriptV1
mintingScript p = PlutusScriptSerialised $ mintingScriptShortBs p

mintingScriptShortBs :: NftParams -> SBS.ShortByteString
mintingScriptShortBs p = (SBS.toShort . LB.toStrict) $ scriptAsCbor p

mPH :: NftParams -> MintingPolicyHash
mPH p = mintingPolicyHash $ policy p

cSymbol :: NftParams -> CurrencySymbol
cSymbol p = mpsSymbol $ mPH p


-- Create Redeemer

newtype ContractRedeemer = ContractRedeemer { _nothing :: () }

PlutusTx.makeLift ''ContractRedeemer
PlutusTx.unstableMakeIsData ''ContractRedeemer



main :: Prelude.IO ()
main = do
    args <- getArgs
    let txHash = args Prelude.!!0
    let txIx = read (args Prelude.!!1) :: Integer
    let nftTokenName' = TokenName $ stringToBuiltinByteString $ args Prelude.!!2
    let nftAmount' = read (args Prelude.!!3) :: Integer

    let nftParams = NftParams { 
        nftTokenName = nftTokenName'
      , nftAmount = nftAmount'
      , nftTxOutRef = TxOutRef (TxId $ handleHexConversion txHash) txIx
      }

    let vRedeemer = ContractRedeemer ()
    let bRedeemerJson = CL.unpack $ typedDataToJSON vRedeemer

    putStrLn ("Minting Policy Hash")
    putStrLn (show (mPH nftParams))
    
    let mintPolicyHashFilename = "mintHash.dat"
    writeFile mintPolicyHashFilename (show $ mPH nftParams)

    let mintPolicyFilename = "mintPolicyJSON.dat"
    nftPolicyResult <- writeFileTextEnvelope mintPolicyFilename Nothing (mintingScript nftParams)
    case nftPolicyResult of
        Left err -> print err
        Right () -> putStrLn ("wrote NFT policy to file: " Prelude.++ show mintPolicyFilename)

    let redeemerFilename = "typedRedeemer.dat"
    writeFile redeemerFilename bRedeemerJson
    putStrLn ("Sample Redeemer written to file: " Prelude.++ show redeemerFilename)
    