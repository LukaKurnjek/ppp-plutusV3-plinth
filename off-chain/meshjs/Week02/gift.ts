
/*
Off-chain code for the always true validator (mkGiftValidator) defined in 
https://github.com/LukaKurnjek/ppp-plutusV3-plinth/blob/main/src/Week02/Validators.hs
*/

import { 
  BlockfrostProvider, 
  MeshWallet, 
  Transaction, 
  PlutusScript,
  resolvePlutusScriptAddress,
  applyCborEncoding
} from "@meshsdk/core";
import { UTxO } from "@meshsdk/common";
import { secretSeed } from "./seed.ts";
/* seed.ts has to be in form of: 
   export const secretSeed = ["seed1", "seed2", ... ] */

// Define blockchain provider and wallet and wallet address
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
const walletAddress = await wallet.getChangeAddress();

// Defining our gift script 
const trueScript: PlutusScript = {
  code: applyCborEncoding("450101002499"),
  version: "V3"
};
const trueAddr = resolvePlutusScriptAddress(trueScript, 0);

// Function for creating UTXO at gift script 
async function sendFunds(amount: string) {
  const tx = new Transaction({ initiator: wallet })
    .setNetwork("preview")
    .sendLovelace(
      { address: trueAddr, 
      /*datum: {value: "", inline: true }*/}, 
      amount)
    .setChangeAddress(walletAddress);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

// Retunrs a UTXO at a given address that contians the given transaction hash
async function getUtxo(scriptAddress, txHash) {
  const utxos = await provider.fetchAddressUTxOs(scriptAddress);
  if (utxos.length == 0) {
    throw 'No listing found.';
  }
  let filteredUtxo = utxos.find((utxo: any) => {
    return utxo.input.txHash == txHash;
  })!;
  return filteredUtxo
}

// Function for claiming funds 
async function claimFunds(txHashGiftUtxo) {
  const assetUtxo: UTxO = await getUtxo(trueAddr, txHashGiftUtxo);
  const redeemer = { data: { alternative: 0, fields: [] } };
  
  const tx = new Transaction({ initiator: wallet, fetcher: provider, /*verbose: true*/ })
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
//console.log(await claimFunds("<tx-hash>"));
