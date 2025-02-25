
/*
Off-chain code for the redeemer 42 validators defined in 
https://github.com/LukaKurnjek/ppp-plutusV3-plinth/blob/main/src/Week02/Validators.hs
*/

import { 
  Lucid, 
  Blockfrost,
  SpendingValidator,
  Data, 
  TxHash,
  Address,
  AddressDetails
} from "@lucid-evolution/lucid";
import { Constr } from "@lucid-evolution/plutus";
import {
  validatorToAddress,
  getAddressDetails
} from "@lucid-evolution/utils";
import { secretSeed } from "./seed.ts";

const lucid = await Lucid(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0", 
    "<blockfrost-key>"
  ),
  "Preview"
);

// Load local stored seed as a wallet into lucid
lucid.selectWallet.fromSeed(secretSeed);
const addr: Address = await lucid.wallet().address();

// Defining our public key hash
const details: AddressDetails = getAddressDetails(addr);
const ourPKH: string = details.paymentCredential?.hash!;

// Defining the spending script 
const redeemer42Script: SpendingValidator = {
  type: "PlutusV3",
  script: "581e010100255333573466e1d2054375a6ae84d5d11aab9e3754002229308b01"
};
const redeemer42Address = validatorToAddress("Preview", redeemer42Script);

// Function that sends an amount of lovelace to the script 
async function sendFunds(amount: bigint): Promise<TxHash> {
  const tx = await lucid
    .newTx()
    .pay.ToContract(redeemer42Address, { kind: "inline", value: Data.void() }, { lovelace: amount })
    .complete();
  const signedTx = await tx.sign.withWallet().complete();
  const txHash = await signedTx.submit();
  return txHash
}

// Get the UTXO that contains the previous created funds at the redeemer42 script 
// NOTE: Input the correct transaction hash that sendFunds() returns
async function getUTxO() {
  const utxos = await lucid.utxosByOutRef([{
      txHash:
        "<transaction_hash>",
      outputIndex: 0
  }]);
  return utxos;
}

// Function for claiming funds from script 
async function claimFunds(): Promise<TxHash> {
  const ourUTxO = await getUTxO();

  //For the validator with the custom defined type for redeemer use instead: 
  //const redeemer42 = Data.to(new Constr(0, [BigInt(42)]));
  const redeemer42 = Data.to(BigInt(42));
  
  if (ourUTxO && ourUTxO.length > 0) {
    const tx = await lucid
      .newTx()
      .collectFrom(ourUTxO, redeemer42) 
      .addSignerKey(ourPKH)
      .attach.SpendingValidator(redeemer42Script)
      .complete();

    const signedTx = await tx.sign.withWallet().complete();
    const txHash = await signedTx.submit();
    return txHash
  }
  else return "No UTxO's found that can be claimed"
}

// Function calls: 
//console.log(await sendFunds(5_000_000n));
//console.log(await claimFunds());
