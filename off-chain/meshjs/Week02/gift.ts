
import { 
  BlockfrostProvider, 
  MeshWallet, 
  Transaction, 
  PlutusScript,
  resolvePlutusScriptAddress
} from "@meshsdk/core";
import {
  UTxO
} from "@meshsdk/common";
import { secretSeed } from "./seed.ts";
// seed.ts has to be in form of: 
// export const secretSeed = ["seed1", "seed2", ... ]

const provider = new BlockfrostProvider("<blockfrost-key>")

const wallet = new MeshWallet({
  networkId: 0, //0=testnet, 1=mainnet
  fetcher: provider,
  submitter: provider,
  key: {
    type: "mnemonic",
    words: secretSeed
  }
})

const trueScript: PlutusScript = {
  code: "450101002499",
  version: "V3",
};
const scriptAddr = resolvePlutusScriptAddress(trueScript, 0);
const ourAddr = await wallet.getChangeAddress();
const walletInfo = await wallet.getWalletInfoForTx;
console.log(ourAddr);
console.log(walletInfo);

async function sendFunds(amount: string) {
  const tx = new Transaction({ initiator: wallet })
    .sendLovelace(scriptAddr, amount)
    .setChangeAddress(ourAddr);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

async function getAssetUtxo(scriptAddress) {
  const utxos = await provider.fetchAddressUTxOs(scriptAddress);
  if (utxos.length == 0) {
    throw 'No listing found.';
  }
  let filteredUtxos = utxos.find((utxo: any) => {
    return utxo.input.txHash == "53e3dee440c2b300dfeff7f44ce43729d1c5cb48c8cbeab9487ee65915e1a2f7";
  });
  return filteredUtxos[0]
}

// Look at: https://github.com/MeshJS/mesh/blob/main/packages/mesh-contract/src/payment-splitter/offchain.ts
async function claimFunds() {
  const assetUtxo: UTxO = await getAssetUtxo(scriptAddr);
  const tx = new Transaction({ initiator: wallet })
  .redeemValue({
    value: assetUtxo,
    script: trueScript,
  })
  .sendValue(ourAddr, assetUtxo);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

//console.log(scriptAddr); 
//console.log(await sendFunds("5000000"));
//console.log(await getAssetUtxo(scriptAddr));
//console.log(await claimFunds());

/*
console.log(await wallet.getUtxos());
console.log(await wallet.getLovelace()); // Gives: 984.532.894
From https://meshjs.dev/apis/wallets/browserwallet: 2696.263.627
*/
