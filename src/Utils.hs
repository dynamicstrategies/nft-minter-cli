{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass        #-} 
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Utils (handleHexConversion) where

-- Haskell Imports
import Prelude hiding (pred, ($), (&&), (||), (<), (==), not, any)
import System.IO (writeFile)
import System.Environment
import Codec.Serialise (serialise)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Base16 as Base16
import Data.Maybe
import Codec.Binary.Bech32 as B32
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)
import Data.Text.Encoding as TE

-- Plutus Imports
import qualified Plutus.V1.Ledger.Scripts as Plutus
import Plutus.V1.Ledger.Scripts (MintingPolicyHash(..))
import Plutus.V1.Ledger.Interval (interval, contains, after, upperBound,  lowerBound, Interval(..))
import Plutus.V1.Ledger.Time (POSIXTime, fromMilliSeconds, POSIXTimeRange, DiffMilliSeconds(..))
import Plutus.V1.Ledger.Contexts (ScriptContext(scriptContextTxInfo), TxInfo(txInfoValidRange, txInfoOutputs), TxOut(txOutValue), txSignedBy)
import qualified Ledger hiding (singleton)
import Ledger.Crypto (PubKeyHash(..))
import Ledger.Address (PaymentPubKeyHash(..))
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Api as A
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Plutus.V1.Ledger.Bytes (fromHex, LedgerBytes(..))
import Plutus.V1.Ledger.Value (Value, singleton, CurrencySymbol(..), TokenName(..), AssetClass(..), assetClassValueOf, flattenValue, mpsSymbol, tokenName)
import PlutusTx
import PlutusTx.Builtins (unsafeDataAsI, toBuiltin, encodeUtf8, decodeUtf8, unsafeDataAsB, emptyByteString)
import PlutusTx.Builtins.Internal (BuiltinString(..))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))
import qualified Cardano.Binary as CBOR
import Cardano.Api (serialiseToTextEnvelope)
import Cardano.Codec.Bech32.Prefixes (addr_test, addr)

{-
  Convert a HexString (Base16) 
  into a Plutus BuiltinByteString
-}
{-# INLINABLE handleHexConversion #-}
handleHexConversion :: String -> BuiltinByteString
handleHexConversion v = case bn of
    Right a -> getLedgerBytes a
    Left _  -> emptyByteString
  where bn = fromHex $ C.pack v