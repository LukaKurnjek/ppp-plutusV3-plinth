
/*
Off-chain code for the always true validator (mkGiftValidator) defined in 
https://github.com/LukaKurnjek/ppp-plutusV3-plinth/blob/main/src/Week02/Validators.hs

The claimFunds() function is constructed in similar way as in the following MeshJS code:
https://github.com/MeshJS/examples/blob/main/aiken-vesting/src/withdraw-fund.ts

The error that the claimFunds() function returns: 
error: Uncaught (in promise) Error: Tx evaluation failed: "{\"type\":\"jsonwsp/response\",
\"version\":\"1.0\",\"servicename\":\"ogmios\",\"methodname\":\"EvaluateTx\",\"result\":
{\"EvaluationFailure\":{\"ScriptFailures\":{}}},\"reflection\":{\"id\":\"70c408a7-f37d-4a98-9412-52ba5aef7c32\"}}"
*/

import { 
  BlockfrostProvider, 
  MeshWallet, 
  Transaction, 
  PlutusScript,
  resolvePlutusScriptAddress,
  MeshTxBuilder,
  deserializeAddress,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot
} from "@meshsdk/core";
import { UTxO } from "@meshsdk/common";
import { CardanoSDKSerializer } from "@meshsdk/core-cst";
import { secretSeed } from "./seed.ts";
/* seed.ts has to be in form of: 
   export const secretSeed = ["seed1", "seed2", ... ] */

// Define blockchain provider and wallet 
const provider = new BlockfrostProvider("<blockfrost-key>");
const wallet = new MeshWallet({
  networkId: 0, //0=testnet, 1=mainnet
  fetcher: provider,
  submitter: provider,
  key: {
    type: "mnemonic",
    words: secretSeed
  }
});

// Define address and public key hash of it 
const walletAddress = await wallet.getChangeAddress();
const signerHash = deserializeAddress(walletAddress).pubKeyHash;

// Defining our gift script 
const giftScript: PlutusScript = {
  code: "450101002499",
  version: "V3"
};
const scriptAddr = resolvePlutusScriptAddress(giftScript, 0);

// Function for creating UTXO at gift script 
async function sendFunds(amount: string) {
  const tx = new Transaction({ initiator: wallet })
    .setNetwork("preview")
    .sendLovelace({address: scriptAddr, datum: {value: "", inline: true }}, amount)
    .setChangeAddress(walletAddress);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

// Function that retunrs the UTXO created with sendFunds
// NOTE: The correct transaction hash needs to be input into the code 
async function getAssetUtxo(scriptAddress) {
  const utxos = await provider.fetchAddressUTxOs(scriptAddress);
  if (utxos.length == 0) {
    throw 'No listing found.';
  }
  let filteredUtxo = utxos.find((utxo: any) => {
    return utxo.input.txHash == "1cf9d34529699de798c0921cdb115bd90d3c11c83d688e96104c8bcdbbabedeb";
  })!;
  return filteredUtxo
}

// Define the transaction builder 
const txBuilder = new MeshTxBuilder({
  fetcher: provider, // get a provider https://meshjs.dev/providers
  submitter: provider,
  evaluator: provider,
  serializer: new CardanoSDKSerializer(),
  verbose: true,
});

// Function for claiming funds 
async function claimFunds() {
  const assetUtxo: UTxO = await getAssetUtxo(scriptAddr);
  const utxos = await wallet.getUtxos();
  const collateral = await wallet.getCollateral();
  const invalidBefore = unixTimeToEnclosingSlot(
      Date.now() - 15000,
      SLOT_CONFIG_NETWORK.preview
    ) + 1;

  const unsignedTx = await txBuilder
  .setNetwork("preview")
  .spendingPlutusScript("V3")
  .txIn(
    assetUtxo.input.txHash,
    assetUtxo.input.outputIndex,
    assetUtxo.output.amount,
    assetUtxo.output.address
  )  
  .spendingReferenceTxInInlineDatumPresent()
  .spendingReferenceTxInRedeemerValue("")
  .txInScript(giftScript.code)
  .txOut(walletAddress, [])
  .txInCollateral(
    collateral[0].input.txHash,
    collateral[0].input.outputIndex,
    collateral[0].output.amount,
    collateral[0].output.address
  )
  .invalidBefore(invalidBefore)
  .requiredSignerHash(signerHash)
  .changeAddress(walletAddress)
  .selectUtxosFrom(utxos)
  .complete();

  const signedTx = await wallet.signTx(unsignedTx);
  const txHash = await wallet.submitTx(signedTx);
  return txHash
}

// Function calls 
//console.log(await sendFunds("5000000"));
//console.log(await getAssetUtxo(scriptAddr));
//console.log(await claimFunds());
