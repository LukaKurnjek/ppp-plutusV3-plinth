{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusTx.Blueprint
import qualified Week02.Validators           as Week02
import qualified Week03.Vesting              as Vesting
import           PlutusLedgerApi.Data.V3     (POSIXTime, PubKeyHash)
import           PlutusLedgerApi.V3          (TokenName, TxOutRef)
import qualified Week05.NFT                  as NFT

{- -------------------------------------------------------------------------------------------- -}
{- ---------------------------------------- ENTRY POINT --------------------------------------- -}

main :: IO ()
main = writeBlueprint "blueprint.json" blueprint

{- -------------------------------------------------------------------------------------------- -}
{- -------------------------------------------- SHARED ---------------------------------------- -}

blueprint :: ContractBlueprint
blueprint =
  MkContractBlueprint
    { contractId = Just "plutus-pioneer-program"
    , contractPreamble = preamble
    , contractValidators =
        Set.fromList
          [ mkGiftVal
          , mkBurnVal
          , mk42ValLarge
          , mk42ValSmall
          , mk42TypedVal
          , mk42CustomVal
          , read42ValSmall
          , vestingValidator
          , vestingValidatorParam
          , vestingValidatorParam2
          , nftValidator
          ]
    , contractDefinitions =
        deriveDefinitions
          @[ ()
           , Integer
           , Vesting.VestingDatum
           , Vesting.VestingParam
           , PubKeyHash
           , POSIXTime
           , TxOutRef
           , TokenName
           ]
    }

preamble :: Preamble
preamble =
  MkPreamble
    { preambleTitle = "Plutus Pioneer Program Blueprint"
    , preambleDescription = Just "Blueprint for the Plutus Pioneer Program validators"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV3
    , preambleLicense = Just "MIT"
    }

{- -------------------------------------------------------------------------------------------- -}
{- ------------------------------------ VALIDATORS - WEEK02 ----------------------------------- -}

mkGiftVal :: ValidatorBlueprint referencedTypes
mkGiftVal =
  MkValidatorBlueprint
    { validatorTitle = "Always True Validator"
    , validatorDescription = Just "Validator that always returns True (always succeeds)"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the always true validator"
          , argumentPurpose = Set.fromList [Spend, Mint, Withdraw, Publish]
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Week02.serializedMkGiftValidator
    }

mkBurnVal :: ValidatorBlueprint referencedTypes
mkBurnVal =
  MkValidatorBlueprint
    { validatorTitle = "Always False Validator"
    , validatorDescription = Just "Validator that always returns False (always fails)"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the always false validator"
          , argumentPurpose = Set.fromList [Spend, Mint, Withdraw, Publish]
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Week02.serializedMkBurnValidator
    }

mk42ValLarge :: ValidatorBlueprint referencedTypes
mk42ValLarge =
  MkValidatorBlueprint
    { validatorTitle = "42 Validator untyped - large CBOR"
    , validatorDescription = Just "Validator that returns true only if the redeemer is 42"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the 42 validator"
          , argumentPurpose = Set.fromList [Spend, Mint, Withdraw, Publish]
          , argumentSchema = definitionRef @Integer
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Week02.serializedMk42ValidatorLarge
    }

mk42ValSmall :: ValidatorBlueprint referencedTypes
mk42ValSmall =
  MkValidatorBlueprint
    { validatorTitle = "42 Validator untyped - small CBOR"
    , validatorDescription = Just "Validator that returns true only if the redeemer is 42"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the 42 validator"
          , argumentPurpose = Set.fromList [Spend, Mint, Withdraw, Publish]
          , argumentSchema = definitionRef @Integer
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Week02.serializedMk42ValidatorSmall
    }

mk42TypedVal :: ValidatorBlueprint referencedTypes
mk42TypedVal =
  MkValidatorBlueprint
    { validatorTitle = "42 Validator typed"
    , validatorDescription = Just "Validator that returns true only if the redeemer is 42"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the 42 typed validator"
          , argumentPurpose = Set.fromList [Spend, Mint, Withdraw, Publish]
          , argumentSchema = definitionRef @Integer
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Week02.serializedMk42TypedValidator
    }

mk42CustomVal :: ValidatorBlueprint referencedTypes
mk42CustomVal =
  MkValidatorBlueprint
    { validatorTitle = "42 Validator custom redeemer"
    , validatorDescription = Just "Validator that returns true only if the redeemer is correctly wrapped number 42."
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the 42 typed validator"
          , argumentPurpose = Set.fromList [Spend, Mint, Withdraw, Publish]
          , argumentSchema = definitionRef @Integer
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Week02.serializedMk42CustomValidator
    }

read42ValSmall :: ValidatorBlueprint referencedTypes
read42ValSmall =
  MkValidatorBlueprint
    { validatorTitle = "42 datum validator untyped"
    , validatorDescription = Just "Validator that returns true only if the datum is number 42."
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the 42 typed validator"
          , argumentPurpose = Set.fromList [Spend, Mint, Withdraw, Publish]
          , argumentSchema = definitionRef @Integer
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Week02.serializedRead42ValidatorSmall
    }

{- -------------------------------------------------------------------------------------------- -}
{- ------------------------------------ VALIDATORS - WEEK03 ----------------------------------- -}

vestingValidator :: ValidatorBlueprint referencedTypes
vestingValidator =
  MkValidatorBlueprint
    { validatorTitle = "Vesting validator"
    , validatorDescription = Just "Validator that allows spending only by a certain key and after a certain deadline"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the vesting validator"
          , argumentPurpose = Set.singleton Spend
          , argumentSchema = definitionRef @()
          }
    , validatorDatum =
        Just $
          MkArgumentBlueprint
            { argumentTitle = Just "VestingDatum"
            , argumentDescription = Just "Datum for the vesting validator"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @Vesting.VestingDatum
            }
    , validatorCompiledCode =
        Just . Short.fromShort $ Vesting.serializedVestingVal
    }

vestingValidatorParam :: ValidatorBlueprint referencedTypes
vestingValidatorParam =
  MkValidatorBlueprint
    { validatorTitle = "Parameterized vesting validator"
    , validatorDescription = Just "Validator that allows spending only by a certain key and after a certain deadline"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "VestingParam"
            , parameterDescription = Just ""
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @Vesting.VestingParam
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the vesting validator"
          , argumentPurpose = Set.singleton Spend
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Vesting.serializedParamVestingVal
    }

vestingValidatorParam2 :: ValidatorBlueprint referencedTypes
vestingValidatorParam2 =
  MkValidatorBlueprint
    { validatorTitle = "2 times parameterized vesting validator"
    , validatorDescription = Just "Validator that allows spending only by a certain key and after a certain deadline"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "PubKeyHash"
            , parameterDescription = Just ""
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @PubKeyHash
            }
        , MkParameterBlueprint
            { parameterTitle = Just "POSIXTime"
            , parameterDescription = Just ""
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @POSIXTime
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the vesting validator"
          , argumentPurpose = Set.singleton Spend
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Vesting.serializedParam2VestingVal
    }

{- -------------------------------------------------------------------------------------------- -}
{- ------------------------------------ VALIDATORS - WEEK05 ----------------------------------- -}

nftValidator :: ValidatorBlueprint referencedTypes
nftValidator =
  MkValidatorBlueprint
    { validatorTitle = "NFT Validator"
    , validatorDescription = Just "Validator that allows spending only once and only to mint one token"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "TxOutRef"
            , parameterDescription = Just "Reference to the UTxO to consume to be able to mint the NFT"
            , parameterPurpose = Set.singleton Mint
            , parameterSchema = definitionRef @TxOutRef
            }
        , MkParameterBlueprint
            { parameterTitle = Just "TokenName"
            , parameterDescription = Just "NFT's token name"
            , parameterPurpose = Set.singleton Mint
            , parameterSchema = definitionRef @TokenName
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Nothing
          , argumentPurpose = Set.singleton Mint
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ NFT.serializedNFTVal
    }

