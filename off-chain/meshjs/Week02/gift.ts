
/*
Off-chain code for the always true validator (mkGiftValidator) defined in 
https://github.com/LukaKurnjek/ppp-plutusV3-plinth/blob/main/src/Week02/Validators.hs

The claimFunds() function is constructed in similar way as in the following MeshJS code:
https://github.com/MeshJS/mesh/blob/main/apps/playground/src/pages/aiken/transactions/redeem.tsx

The error that the claimFunds() function returns: 
error: Uncaught (in promise) '{"data":{"error":"Bad Request","message":"{\\"contents\\":
{\\"contents\\":{\\"contents\\":{\\"era\\":\\"ShelleyBasedEraConway\\",\\"error\\":
[\\"ConwayUtxowFailure (MalformedScriptWitnesses

Explanation from the web: malformedScriptWitnesses, occurs when a script witness specified 
in the transaction does not properly deserialize to a Plutus script.  
Source: https://newreleases.io/project/github/CardanoSolutions/ogmios/release/v5.5.0
*/

import { 
  BlockfrostProvider, 
  MeshWallet, 
  Transaction, 
  PlutusScript,
  resolvePlutusScriptAddress
} from "@meshsdk/core";
import { UTxO } from "@meshsdk/common";
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

// Defining our gift script 
const trueScript: PlutusScript = {
  code: "450101002499",
  version: "V3"
};
const scriptAddr = resolvePlutusScriptAddress(trueScript, 0);

// Function for creating UTXO at gift script 
async function sendFunds(amount: string) {
  const tx = new Transaction({ initiator: wallet })
    .setNetwork("preview")
    .sendLovelace(
      { address: scriptAddr, 
      /*datum: {value: "", inline: true }*/}, 
      amount)
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
    return utxo.input.txHash == "a9705c29b745aa9f4e01bf9439f9aef6b2c6d0618fc7f670daa9e1cb156a11be";
  })!;
  return filteredUtxo
}

// Function for claiming funds 
async function claimFunds() {
  const assetUtxo: UTxO = await getAssetUtxo(scriptAddr);
  const redeemer = { data: { alternative: 1, fields: [""] } };
  
  const tx = new Transaction({ initiator: wallet, fetcher: provider, verbose: true })
    .setNetwork("preview")
    .redeemValue({ value: assetUtxo, 
                   script: trueScript,
                   datum: undefined,
                   redeemer: redeemer})
    .sendValue(walletAddress, assetUtxo)
    .setRequiredSigners([walletAddress]);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

// Function calls 
//console.log(await sendFunds("5000000"));
//console.log(await getAssetUtxo(scriptAddr));
//console.log(await claimFunds());
