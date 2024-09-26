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
import           PlutusLedgerApi.V3          (Address, TokenName, TxOutRef)
import           PlutusTx.Blueprint
import qualified Week02.Validators           as Week02
import qualified Week03.ParameterizedVesting as ParamVesting
import qualified Week03.Vesting              as Vesting
import qualified Week05.NFT                  as NFT
import qualified Week06.ExploitableSwap      as ExploitableSwap
import qualified Week06.NegativeRTimed       as NegativeRTimed
import qualified Week08.Staking              as Staking
import qualified Week09.Oracle               as Oracle
import qualified Week09.Stablecoin           as Stablecoin

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
          [ alwaysTrueValidator
          , alwaysFalseValidator
          , fortyTwoValidator
          , vestingValidator
          , vestingValidatorParam
          , negativeRTimedValidator
          , exploitableSwapValidator
          , oracleValidator
          , stablecoinValidator
          ]
    , contractDefinitions =
        deriveDefinitions
          @[ ()
           , Integer
           , Vesting.VestingDatum
           , ParamVesting.VestingParams
           , TxOutRef
           , TokenName
           , NegativeRTimed.CustomDatum
           , ExploitableSwap.DatumSwap
           , Address
           , Oracle.OracleParams
           , Oracle.OracleRedeemer
           , Stablecoin.StablecoinParams
           , Stablecoin.StablecoinRedeemer
           , Stablecoin.StablecoinDatum
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

alwaysTrueValidator :: ValidatorBlueprint referencedTypes
alwaysTrueValidator =
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
        Just . Short.fromShort $ Week02.serializedAlwaysTrueValidator
    }

alwaysFalseValidator :: ValidatorBlueprint referencedTypes
alwaysFalseValidator =
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
        Just . Short.fromShort $ Week02.serializedAlwaysFalseValidator
    }

fortyTwoValidator :: ValidatorBlueprint referencedTypes
fortyTwoValidator =
  MkValidatorBlueprint
    { validatorTitle = "42 Validator"
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
        Just . Short.fromShort $ Week02.serializedFortyTwoValidator
    }

{- -------------------------------------------------------------------------------------------- -}
{- ------------------------------------ VALIDATORS - WEEK03 ----------------------------------- -}

vestingValidator :: ValidatorBlueprint referencedTypes
vestingValidator =
  MkValidatorBlueprint
    { validatorTitle = "Vesting Validator"
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
    { validatorTitle = "Parameterized Vesting Validator"
    , validatorDescription = Just "Validator that allows spending only by a certain key and after a certain deadline"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "VestingParams"
            , parameterDescription = Just ""
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @ParamVesting.VestingParams
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
        Just . Short.fromShort $ ParamVesting.serializedParamVestingVal
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

{- -------------------------------------------------------------------------------------------- -}
{- ------------------------------------ VALIDATORS - WEEK06 ----------------------------------- -}

negativeRTimedValidator :: ValidatorBlueprint referencedTypes
negativeRTimedValidator =
  MkValidatorBlueprint
    { validatorTitle = "negativeRTimed Validator"
    , validatorDescription = Just "Validator that allows spending only when the redeemer is negative after a certain deadline"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Some number"
          , argumentDescription = Nothing
          , argumentPurpose = Set.singleton Spend
          , argumentSchema = definitionRef @Integer
          }
    , validatorDatum =
        Just $
          MkArgumentBlueprint
            { argumentTitle = Just "CustomDatum"
            , argumentDescription = Just "Dummy newtype that wraps a deadline"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @NegativeRTimed.CustomDatum
            }
    , validatorCompiledCode =
        Just . Short.fromShort $ NegativeRTimed.serializedNegativeRTimedVal
    }

exploitableSwapValidator :: ValidatorBlueprint referencedTypes
exploitableSwapValidator =
  MkValidatorBlueprint
    { validatorTitle = "Exploitable Sawp Validator"
    , validatorDescription = Just "Validator that allows the user to do swaps but is vulnerable to double satisfaction"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Nothing
          , argumentPurpose = Set.singleton Spend
          , argumentSchema = definitionRef @()
          }
    , validatorDatum =
        Just $
          MkArgumentBlueprint
            { argumentTitle = Just "DatumSwap"
            , argumentDescription = Just "Contains the beneficiary and the price of the swap"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @ExploitableSwap.DatumSwap
            }
    , validatorCompiledCode =
        Just . Short.fromShort $ NegativeRTimed.serializedNegativeRTimedVal
    }

{- -------------------------------------------------------------------------------------------- -}
{- ------------------------------------ VALIDATORS - WEEK08 ----------------------------------- -}

stakingValidator :: ValidatorBlueprint referencedTypes
stakingValidator =
  MkValidatorBlueprint
    { validatorTitle = "Staking Validator"
    , validatorDescription = Just "Validator that allows withdrawls only when half of the rewards are paid to the parameterlized address"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Nothing
          , argumentPurpose = Set.fromList [Withdraw, Publish]
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiledCode =
        Just . Short.fromShort $ Staking.serializedStakingVal
    }

{- -------------------------------------------------------------------------------------------- -}
{- ------------------------------------ VALIDATORS - WEEK09 ----------------------------------- -}

oracleValidator :: ValidatorBlueprint referencedTypes
oracleValidator =
  MkValidatorBlueprint
    { validatorTitle = "Oracle Validator"
    , validatorDescription = Just "Validator that allows to provide data using a centralized oracle"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "OracleParams"
            , parameterDescription = Just "Parameters of the oracle. The operator's PKH and the thread NFT"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @Oracle.OracleParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "OracleRedeemer"
          , argumentDescription = Just "Allows to update or delete the oracle data"
          , argumentPurpose = Set.singleton Spend
          , argumentSchema = definitionRef @Oracle.OracleRedeemer
          }
    , validatorDatum =
        Just $
          MkArgumentBlueprint
            { argumentTitle = Just "Rate"
            , argumentDescription = Just "The ADA/USD rate provided by the oracle"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @Integer
            }
    , validatorCompiledCode =
        Just . Short.fromShort $ Oracle.serializedOracleVal
    }

stablecoinValidator :: ValidatorBlueprint referencedTypes
stablecoinValidator =
  MkValidatorBlueprint
    { validatorTitle = "Stablecion Validator"
    , validatorDescription = Just "Validator that allows withdrawls only when half of the rewards are paid to the parameterlized address"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "StablecoinParams"
            , parameterDescription = Just "Parameters of the oracle. The operator's PKH and the thread NFT"
            , parameterPurpose = Set.fromList [Mint, Spend]
            , parameterSchema = definitionRef @Stablecoin.StablecoinParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "StablecoinRedeemer"
          , argumentDescription = Nothing
          , argumentPurpose = Set.fromList [Mint, Spend]
          , argumentSchema = definitionRef @Stablecoin.StablecoinRedeemer
          }
    , validatorDatum =
        Just $
          MkArgumentBlueprint
            { argumentTitle = Just "StablecoinDatum"
            , argumentDescription = Just "Contains tha collateral's owner and the amount of stablecoins minted with this collateral"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @Stablecoin.StablecoinDatum
            }
    , validatorCompiledCode =
        Just . Short.fromShort $ Stablecoin.serializedStablecoinVal
    }
