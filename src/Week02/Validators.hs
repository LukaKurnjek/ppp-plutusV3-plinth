{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Week02.Validators where

import PlutusLedgerApi.Common (
  FromData (fromBuiltinData),
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V3 (
  Redeemer (getRedeemer),
  ScriptContext (..),
 )
import PlutusTx (
  BuiltinData,
  CompiledCode,
  UnsafeFromData (unsafeFromBuiltinData),
  compile,
 )
import PlutusTx.Bool (Bool (..))
import PlutusTx.Prelude (
  BuiltinUnit,
  Eq (..),
  Integer,
  Maybe (..),
  check,
  traceError,
  traceIfFalse,
  ($),
 )

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------- Always True validator --------------------------------- -}

{-# INLINEABLE alwaysTrueVal #-}
alwaysTrueVal :: ScriptContext -> Bool
alwaysTrueVal _ctx = True

compiledAlwaysTrueVal :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledAlwaysTrueVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ alwaysTrueVal (unsafeFromBuiltinData ctx)

serializedAlwaysTrueValidator :: SerialisedScript
serializedAlwaysTrueValidator = serialiseCompiledCode compiledAlwaysTrueVal

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------- Always False validator --------------------------------- -}

{-# INLINEABLE alwaysFalseVal #-}
alwaysFalseVal :: ScriptContext -> Bool
alwaysFalseVal _ctx = True

compiledAlwaysFalseVal :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledAlwaysFalseVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ alwaysFalseVal (unsafeFromBuiltinData ctx)

serializedAlwaysFalseValidator :: SerialisedScript
serializedAlwaysFalseValidator = serialiseCompiledCode compiledAlwaysFalseVal

{- ------------------------------------------------------------------------------------------ -}
{- -------------------------------------- 42 validator -------------------------------------- -}

{-# INLINEABLE fortyTwoVal #-}
fortyTwoVal :: ScriptContext -> Bool
fortyTwoVal ctx = traceIfFalse "Redeemer is a number different than 42" $ 42 == r
 where
  r = case fromBuiltinData $ getRedeemer (scriptContextRedeemer ctx) of
    Just @Integer n -> n
    Nothing -> traceError "Redeemer is not a number"

compiledFortyTwoVal :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledFortyTwoVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ fortyTwoVal (unsafeFromBuiltinData ctx)

serializedFortyTwoValidator :: SerialisedScript
serializedFortyTwoValidator = serialiseCompiledCode compiledFortyTwoVal
