{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-} 

module WriteTypedData (typedDataToJSON) where

-- Haskell Imports
import Prelude hiding (pred, ($), (<$>), (&&), (<), (==))
import qualified Data.ByteString.Lazy  as LBS
import Data.Aeson (encode)

-- Plutus Imports
import Cardano.Api
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))


dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x', dataToScriptData y) | (x', y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs


typedDataToJSON :: PlutusTx.ToData a => a -> LBS.ByteString
typedDataToJSON = encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData