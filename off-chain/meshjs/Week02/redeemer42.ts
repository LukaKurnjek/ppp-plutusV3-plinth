
/*
Off-chain code for the redeemer 42 validators defined in 
https://github.com/LukaKurnjek/ppp-plutusV3-plinth/blob/main/src/Week02/Validators.hs
*/

import { 
  BlockfrostProvider, 
  MeshWallet, 
  Transaction, 
  PlutusScript,
  resolvePlutusScriptAddress,
  applyCborEncoding,
  mConStr0
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
const redeemer42Script: PlutusScript = {
  code: applyCborEncoding("581e010100255333573466e1d2054375a6ae84d5d11aab9e3754002229308b01"),
  version: "V3"
};
const redeemer42Addr = resolvePlutusScriptAddress(redeemer42Script, 0);

// Function for creating UTXO at gift script 
async function sendFunds(amount: string) {
  const tx = new Transaction({ initiator: wallet })
    .setNetwork("preview")
    .sendLovelace(
      { address: redeemer42Addr }, 
      amount)
    .setChangeAddress(walletAddress);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

// Retunrs UTXOs at a given address that contian the given transaction hash 
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
async function claimFunds(txHashAssetUtxo) {
  const assetUtxo: UTxO = await getUtxo(redeemer42Addr, txHashAssetUtxo);
  const redeemer = { data: { alternative: 0, fields: [BigInt(42)] } };
  
  const tx = new Transaction({ initiator: wallet, fetcher: provider })
    .setNetwork("preview")
    .redeemValue({ value: assetUtxo, 
                   script: redeemer42Script,
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
//console.log(await sendFunds("3000000"));
//console.log(await claimFunds("<tx-hash>"));
