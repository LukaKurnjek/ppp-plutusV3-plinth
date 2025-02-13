
/*
Off-chain code for the NFT validator (signedVal) defined in 
https://github.com/LukaKurnjek/ppp-plutusV3-plinth/blob/main/src/Week05/Minting.hs
*/

import { 
  Lucid, 
  Blockfrost,
  Address,
  MintingPolicy,
  Data, 
  fromText,
  PolicyId,
  Unit,
  TxHash,
  AddressDetails
} from "@lucid-evolution/lucid";
import {
  applyParamsToScript,
  mintingPolicyToId,
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

// Read out the public key hash from our address and define token name 
const details: AddressDetails = getAddressDetails(addr);
const pkh: string = details.paymentCredential?.hash!;
const tn = fromText("MyTokens");

// Creating parameters type 
const Params = Data.Tuple([Data.Bytes()]);
type Params = Data.Static<typeof Params>;

// Defining the minting script
const signedPolicy: MintingPolicy = {
  type: "PlutusV3",
  script: applyParamsToScript<Params>(
      "590b4901010032323232323232323232323222593232323232323232932323255333573466e1d200000211328009919192a999ab9a3370e90000010899191919191919191919191919191919194004c00cd5d08094c00cd5d0808ccc094078d5d08084dd69aba100f99801280f9aba100e9981280d9aba100d9998140133ad3574201932323255333573466e1d2000002118009919192a999ab9a3370e900000108c004cc0c5d69aba100198181aba135744002113033491035054310035573c0046aae74004dd51aba10019919192a999ab9a3370e900000108c004cc0c5d69aba100198181aba1357440021130334901035054310035573c0046aae74004dd51aba1357440021130304901035054310035573c0046aae74004dd51aba100b99812bae35742015333028232323255333573466e1d2000002118019bae357420021155333573466e1d20020021180498109aba100108aa999ab9a3370e900200108c01cc0a8d5d080084554ccd5cd19b8748018008460026eb4d5d0800cc07cd5d09aba200108aa999ab9a3370e900400108c02cc080d5d080084554ccd5cd19b87480280084600a6eb4d5d0800cc078d5d09aba2001089818a481035054310035573c0046aae74004dd50008149aba1009998010149aba10089bae3574200f33302801c3302802c232323255333573466e1d20000021180108aa999ab9a3370e900100108c0104554ccd5cd19b8748010008460002260629201035054310035573c0046aae74004dd50009aba10069981280d1aba100598009aba100498009aba135744008302675a604ceb8c0888c8c8c954ccd5cd19b874800000846002603c6ae84006603e6ae84d5d1000844c0b9241035054310035573c0046aae74004dd50009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba200135744002226038921035054310035573c0046aae74004dd51aba10029aba10019919192a999ab9a3370e900000108c00cdd71aba100108aa999ab9a3370e900100108c024c030d5d0800ccc058060d5d09aba200108aa999ab9a3370e900200108c01cc054d5d080084554ccd5cd19b8748018008460026eb4d5d0800cc028d5d09aba200108aa999ab9a3370e900400108c02cc02cd5d080084554ccd5cd19b87480280084600a6eb4d5d0800cc024d5d09aba200108980e2481035054310035573c0046aae74004dd51aba135744002357440022260309201035054310035573c0046aae74004dd5004c88966400e444444444444444460280108c00230020c05489640063002910aa999ab9a3371e0160042300011300400111919192a999ab9a3370e900000108994004dd69aba100298089aba10019919192a999ab9a3370e900000108994024cc05806cd5d08014d5d0800ccc059d71aba1357440023574400222aa666ae68cdc3a4004004230013301501a3574200332323255333573466e1d2000002118009bad35742003375a6ae84d5d1000844c0792401035054310035573c0046aae74004dd51aba1357440021155333573466e1d20040021180599980b00a3ad357420033301575c6ae84d5d100084554ccd5cd19b87480180084600e6602a0346ae8400422aa666ae68cdc3a401000422646500d3301701c35742007330150163574200533301801675a6ae840072646464aa666ae68cdc3a400000423001375a6ae840066eb4d5d09aba20010898102481035054310035573c0046aae74004dd51aba13574400322330150020010d5d10009aba20011155333573466e1d200a002118029980a80d1aba10019919192a999ab9a3370e9000001089980c3ae3574200222603c9201035054310035573c0046aae74004dd51aba1357440021155333573466e1d200c0021180108980da481035054310035573c0046aae74004dd51aba1357440023574400222602e9201035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c034c03cd5d0800ccc041d69aba1357440021155333573466e1d20020021180998079aba1001998083ad357426ae8800422a64a666ae68cdc3a4008006230033010357420053001357426ae8800822aa666ae68cdc3a400c006226500b301135742007300235742003375a6ae84d5d10008d5d100108aa999ab9a3370e900400188c024c040d5d08014dd69aba1357440041155333573466e1d200a0031180a98081aba100208aa999ab9a3370e900600188c044c040d5d08014dd69aba1357440041155333573466e1d200e003118029bae35742005375c6ae84d5d100104554ccd5cd19b874804000c4600e6eb8d5d08014dd69aba1357440041155333573466e1d20120031180098081aba100298081aba1357440041155333573466e1d20140031180798081aba100208980ba4810350543100232323255333573466e1d2000002118009bae35742002115325333573466e1d20020031180298009aba100208aa999ab9a3370e900200188c00cdd71aba100298009aba13574400411301b49010350543100232323255333573466e1d200000211800980b9aba100108aa999ab9a3370e900100108c0084554ccd5cd19b87480100084600822603c9201035054310035573c0046aae74004dd50009aab9e00235573a0026ea8004d55cf0011aab9d00137540024646464aa666ae68cdc3a400000423001300e357420021155333573466e1d20020021180198071aba100108aa999ab9a3370e900200108c014dd71aba100108980aa481035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c004dd71aba10019bad357426ae880042260289201035054310035573c0046aae74004dd500091919192a999ab9a3370e90000010899194004c8c8c954ccd5cd19b87480000084600260226ae84006660244646464aa666ae68cdc3a4000004230013015357420021155333573466e1d200200211328019bad35742005375a6ae840066eb4d5d09aba20011aba200111301c4901035054310035573c0046aae74004dd50009aba135744002113018491035054310035573c0046aae74004dd51aba10039998083ae50073574200532323255333573466e1d20000021180008aa999ab9a3370e900100108c014dd71aba100108aa999ab9a3370e900200108c00cd5d0800844c061241035054310035573c0046aae74004dd51aba100199807bae357426ae880046ae88004d5d1000889809a49035054310035573c0046aae74004dd5000899804bae75a6eb800a29344c0352401035054350018059112a999ab9a3370e9000000889806a48103505433001155333573466e200052000113300333702900000119b814800000444ca00266e1000c00666e10008004660080040026014444aa666ae68cdc3a400000222004226600600266e180080048c88c008dd60009805111999aab9f00128001400cc010d5d08014c00cd5d100120004646464aa666ae68cdc3a400000423001375c6ae8400422aa666ae68cdc3a400400423003375c6ae840042260169201035054310035573c0046aae74004dd5000911919192a999ab9a3370e900100108c0084554ccd5cd19b874800000846002600a6ae84004226016921035054310035573c0046aae74004dd50009119118011bab00130082233335573e0025000232801c004c018d55ce800cc014d55cf000a60086ae8800c6ae8400a00040024646464aa666ae68cdc3a4000004230021155333573466e1d200200211800089803a481035054310035573c0046aae74004dd5000911919192a999ab9a3370e900000108c0084554ccd5cd19b874800800846002600a6ae8400422aa666ae68cdc3a400800423004113007491035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c004dd71aba10019bad357426ae8800422600a9201035054310035573c0046aae74004dd5000919319ab9c0018001191800800918011198010010009",
      [pkh])
};

// Defining policy ID and unit of NFT 
const policyId: PolicyId = mintingPolicyToId(signedPolicy);
const unit: Unit = policyId + tn;

// Function for minting the signed tokens  
async function mintSignedTokens(): Promise<TxHash> {
  const tx = await lucid
      .newTx()
      .mintAssets({[unit]: 10n}, Data.void())
      .attach.MintingPolicy(signedPolicy)
      .addSignerKey(pkh)
      .complete();

  const signedTx = await tx.sign.withWallet().complete();
  const txHash = await signedTx.submit();
  return txHash
}

// Function calls 
console.log("minting policy: " + policyId);
console.log(await mintSignedTokens());

