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
  compile
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
  otherwise
 )

{- ----------------------------------------------------------------------------------------- -}
{- --------------------------------- Always True validator --------------------------------- -}

{-# INLINEABLE mkGiftValidator #-}
mkGiftValidator :: BuiltinData -> BuiltinUnit
mkGiftValidator _ctx = check True

compiledMkGiftValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMkGiftValidator = $$(compile [||mkGiftValidator||])

serializedMkGiftValidator :: SerialisedScript
serializedMkGiftValidator = serialiseCompiledCode compiledMkGiftValidator

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------- Always False validator --------------------------------- -}

{-# INLINEABLE mkBurnValidator #-}
mkBurnValidator :: BuiltinData -> BuiltinUnit
mkBurnValidator _ctx = traceError "it burns!!!"

compiledMkBurnValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMkBurnValidator = $$(compile [||mkBurnValidator||])

serializedMkBurnValidator :: SerialisedScript
serializedMkBurnValidator = serialiseCompiledCode compiledMkBurnValidator

{- ------------------------------------------------------------------------------------------ -}
{- -------------------------------------- 42 validator -------------------------------------- -}

{-# INLINEABLE mk42Validator #-}
mk42Validator :: BuiltinData -> BuiltinUnit
mk42Validator ctx 
    | r == 42   = check True
    | otherwise = traceError "expected 42"
 where
  ctxTyped = case fromBuiltinData ctx of
    Just @ScriptContext c -> c
    Nothing -> traceError "ScriptContext could not be converted from BuiltinData" 
  r = case fromBuiltinData $ getRedeemer (scriptContextRedeemer ctxTyped) of
    Just @Integer n -> n
    Nothing -> traceError "Redeemer is not a number"  

compiledMk42Validator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMk42Validator = $$(compile [||mk42Validator||])

serializedMk42Validator :: SerialisedScript
serializedMk42Validator = serialiseCompiledCode compiledMk42Validator

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------- 42 validator typed ----------------------------------- -}

{-# INLINEABLE mk42TypedValidator #-}
mk42TypedValidator :: ScriptContext -> Bool
mk42TypedValidator ctx = traceIfFalse "Redeemer is a number different than 42" $ 42 == r
 where
  r = case fromBuiltinData $ getRedeemer (scriptContextRedeemer ctx) of
    Just @Integer n -> n
    Nothing -> traceError "Redeemer is not a number"

compiledMk42TypedValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMk42TypedValidator = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ mk42TypedValidator (unsafeFromBuiltinData ctx)

serializedMk42TypedValidator :: SerialisedScript
serializedMk42TypedValidator = serialiseCompiledCode compiledMk42TypedValidator
