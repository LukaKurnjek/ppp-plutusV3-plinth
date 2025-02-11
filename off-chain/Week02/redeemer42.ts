
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
  script: "590a9701010032323232323232323232259932323255333573466e1d200000211323232328009919192a999ab9a3370e900000108991919191919191919191919191919199999999999999199199111111111111111400404201f00e806c03201700a804c02200f006802c0120070028008c004d5d080918009aba1011301e232323255333573466e1d200000211800980d1aba100198029aba1357440021130284901035054310035573c0046aae74004dd50009980f0009aba100f232323255333573466e1d20000021132328009919192a999ab9a3370e900000108c004c09cd5d0800ccc0a08c8c8c954ccd5cd19b87480000084600260566ae8400422aa666ae68cdc3a40040042265003375a6ae8400a6eb4d5d0800cdd69aba135744002357440022260609201035054310035573c0046aae74004dd50009aba13574400211302c491035054310035573c0046aae74004dd51aba10039bab3574200532323255333573466e1d20000021180008aa999ab9a3370e900100108c014dd71aba100108aa999ab9a3370e900200108c00cd5d0800844c0b12401035054310035573c0046aae74004dd51aba100199812bae357426ae880046ae88004d5d1000889813a49035054310035573c0046aae74004dd50009bad3574201a6eacd5d08061980e80a1aba100b37566ae84028c8c8c954ccd5cd19b874800000846002646464aa666ae68cdc3a4000004230013302775a6ae84006604c6ae84d5d1000844c0a52401035054310035573c0046aae74004dd51aba10019919192a999ab9a3370e900000108c004cc09dd69aba100198131aba1357440021130294901035054310035573c0046aae74004dd51aba1357440021130264901035054310035573c0046aae74004dd51aba10093301d75c6ae84020dd59aba100737566ae84018dd71aba100537566ae84010cc07404cd5d080198009aba10023001357426ae88008c079d69aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440022260289201035054310035573c0046aae74004dd51aba10059aba10049919192a999ab9a3370e900000108c00cdd71aba100108aa999ab9a3370e900100108c024c018d5d0800ccc0408004d5d09aba200108aa999ab9a3370e900200108c01cc03cd5d080084554ccd5cd19b8748018008460026eb4d5d0800cc014d5d09aba200108aa999ab9a3370e900400108c02cc8c8c954ccd5cd19b87480000084600260246ae8400422aa666ae68cdc3a4004004230033012357420021155333573466e1d2004002118029bae35742002113017491035054310035573c0046aae74004dd51aba100108aa999ab9a3370e900500108c014dd69aba100198021aba1357440021130144901035054310035573c0046aae74004dd51aba135744008232323255333573466e1d200000211328009bad3574200530103574200332323255333573466e1d200000211328049980a80c1aba10029aba10019980abae357426ae880046ae880044554ccd5cd19b8748008008460026602802e6ae84006646464aa666ae68cdc3a400000423001375a6ae840066eb4d5d09aba200108980da481035054310035573c0046aae74004dd51aba1357440021155333573466e1d2004002118059bab357420033301475c6ae84d5d100084554ccd5cd19b87480180084600e6602802e6ae8400422aa666ae68cdc3a401000422646500d3301601935742007330140153574200537566ae840072646464aa666ae68cdc3a400000423001375a6ae840066eb4d5d09aba200108980ea481035054310035573c0046aae74004dd51aba13574400322330140020010d5d10009aba20011155333573466e1d200a002118029980a00b9aba10019919192a999ab9a3370e9000001089980bbae357420022260369201035054310035573c0046aae74004dd51aba1357440021155333573466e1d200c0021180108980c2481035054310035573c0046aae74004dd51aba135744002357440022260289201035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c034c038d5d0800ccc03dd69aba1357440021155333573466e1d20020021180998071aba100199807bad357426ae8800422a64a666ae68cdc3a400800623003300f357420053001357426ae8800822aa666ae68cdc3a400c006226500b301035742007300235742003375a6ae84d5d10008d5d100108aa999ab9a3370e900400188c024c03cd5d08014dd69aba1357440041155333573466e1d200a0031180a98079aba100208aa999ab9a3370e900600188c044c03cd5d08014dd69aba1357440041155333573466e1d200e003118029bae35742005375c6ae84d5d100104554ccd5cd19b874804000c4600e6eb8d5d08014dd69aba1357440041155333573466e1d20120031180098079aba100298079aba1357440041155333573466e1d20140031180798079aba100208980a24810350543100232323255333573466e1d2000002118009bae35742002115325333573466e1d20020031180298009aba100208aa999ab9a3370e900200188c00cdd71aba100298009aba13574400411301849010350543100232323255333573466e1d200000211800980b1aba100108aa999ab9a3370e900100108c0084554ccd5cd19b8748010008460082260369201035054310035573c0046aae74004dd50009aab9e00235573a0026ea8004d55cf0011aab9d00137540024646464aa666ae68cdc3a400000423001375c6ae840066eb4d5d09aba20010898092481035054310035573c0046aae74004dd50009aba200111300d4901035054310035573c0046aae74004dd5000c88954ccd5cd19b874815164ccccc8cc00400c8954ccd5cd19b87480000084564cccd55cf800940088c8ca002004357440066ae8400a00121593333330040012280114008a00450010011400a43001800800918012300208c008888888ccccccd5d2003119198039aab9d00135573c0026ea801c8c014dd5803918021bac00723003375a00e460046eb801e0005002280114008a0052190019000a2c1180008c009149a260149201035054350018049112a999ab9a3370e9000000889805a48103505433001155333573466e200052000113300333702900000119b814800000444ca00266e1000c00666e10008004660080040026010444aa666ae68cdc3a400000222004226600600266e180080048c88c008dd60009804111999aab9f00128001400cc010d5d08014c00cd5d100120004646464aa666ae68cdc3a400000423001375c6ae8400422aa666ae68cdc3a400400423003375c6ae840042260129201035054310035573c0046aae74004dd5000911919192a999ab9a3370e900100108c0084554ccd5cd19b874800000846002600a6ae84004226012921035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c0084554ccd5cd19b87480080084600022600e9201035054310035573c0046aae74004dd5000911919192a999ab9a3370e900000108c0084554ccd5cd19b874800800846002600a6ae8400422aa666ae68cdc3a400800423004113007491035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c004dd71aba10019bad357426ae8800422600a9201035054310035573c0046aae74004dd5000919319ab9c0018001191800800918011198010010009"
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

// Get the UTXO that contains our vesting script 
// NOTE: Input the correct transaction hash that sendFunds() returns
async function getUTxO() {
  const utxos = await lucid.utxosByOutRef([{
      txHash:
        "<transaction_hash>",
      outputIndex: 0
  }]);
  return utxos;
}
const ourUTxO = await getUTxO();

// Function for claiming funds from script 
async function claimFunds(): Promise<TxHash> {
  const redeemer42 = Data.to(BigInt(42));
  // For the validator with the custom defined type for redeemer
  //const redeemer42 = Data.to(new Constr(0, [BigInt(42)]));

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

//console.log(await sendFunds(5_000_000n));
//console.log(await claimFunds());
