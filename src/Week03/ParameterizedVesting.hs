{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Week03.ParameterizedVesting where

import           GHC.Generics                  (Generic)
import           PlutusLedgerApi.Common        (SerialisedScript,
                                                serialiseCompiledCode)
import           PlutusLedgerApi.Data.V3       (POSIXTime, PubKeyHash, from)
import           PlutusLedgerApi.V1.Interval   (contains)
import           PlutusLedgerApi.V3            (ScriptContext (..),
                                                TxInfo (txInfoValidRange))
import           PlutusLedgerApi.V3.Contexts   (txSignedBy)
import           PlutusTx                      (BuiltinData, CompiledCode,
                                                UnsafeFromData (unsafeFromBuiltinData),
                                                compile,
                                                makeIsDataSchemaIndexed,
                                                makeLift)
import           PlutusTx.Blueprint            (HasBlueprintDefinition)
import           PlutusTx.Blueprint.Definition (definitionRef)
import           PlutusTx.Bool                 (Bool (..), (&&))
import           PlutusTx.Prelude              (BuiltinUnit, check,
                                                traceIfFalse, ($))

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------------- TYPES ------------------------------------------ -}

data VestingParams = VestingParams
  { beneficiary :: PubKeyHash
  , deadline    :: POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeLift ''VestingParams
makeIsDataSchemaIndexed ''VestingParams [('VestingParams, 0)]

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------------- VALIDATOR ---------------------------------------- -}

{-# INLINEABLE vestingVal #-}
vestingVal :: VestingParams -> ScriptContext -> Bool
vestingVal vp ctx =
  traceIfFalse "Is not the beneficiary" checkBeneficiary
    && traceIfFalse "Deadline not reachec" checkDeadline
 where
  checkBeneficiary :: Bool
  checkBeneficiary = txSignedBy info (beneficiary vp)

  checkDeadline :: Bool
  checkDeadline = from (deadline vp) `contains` txInfoValidRange info

  info :: TxInfo
  info = scriptContextTxInfo ctx

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledVestingVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVestingVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> PlutusTx.Prelude.BuiltinUnit
  wrappedVal params ctx = check $ vestingVal (unsafeFromBuiltinData params) (unsafeFromBuiltinData ctx)

serializedParamVestingVal :: SerialisedScript
serializedParamVestingVal = serialiseCompiledCode compiledVestingVal
