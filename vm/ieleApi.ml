open IeleClientUtils
open Yojson.Basic.Util
open Msg_types

type block = {
  state: (string * Yojson.Basic.json) list;
  timestamp: string
}

let blocks = ref [{state=[];timestamp=""}]

let nextTimestamp = ref None
let nextBeneficiary = ref "0x0000000000000000000000000000000000000000"

let pendingTx = ref []

module StringMap = Map.Make(String)

let receipts = ref StringMap.empty

(* not secure, but we should only use this for testing purposes *)
let random = Cryptokit.Random.pseudo_rng "1234567890123456"

let get_block blocknumber = 
  let num_int = if blocknumber = "latest" then 0 else (List.length !blocks) - (int_of_string blocknumber) - 1 in
  List.nth !blocks num_int

let personal_newAccount () =
  let acct = Cryptokit.Random.string random 20 in
 `String (to_hex_unsigned (Bytes.of_string acct))

(* assumes that the specified account in the latest block has the empty state *)
let setBalance (address, account) =
  let value = `Assoc [("nonce", `String "0x00"); ("balance", `String "0x00"); ("storage", `Assoc []); ("code", `String "")] in
  let latest = List.hd !blocks in
  let state = latest.state in
  let new_state = (address, value) :: state in
  blocks := {state=new_state;timestamp=latest.timestamp} :: List.tl !blocks;
  let balance = account |> member "wei" |> to_string in
  let latest = List.hd !blocks in
  let state = latest.state in
  let value = List.assoc address state in
  let list = value |> to_assoc in
  let list_without = List.remove_assoc "balance" list in
  let new_account = `Assoc (sort_assoc_list (("balance", `String balance) :: list_without)) in
  let state_without = List.remove_assoc address state in
  let new_state = (address, new_account) :: state_without in
  blocks := {state=new_state;timestamp=latest.timestamp} :: List.tl !blocks
 
let test_rewindToBlock blocknumber =
  let num_to_remove = List.length !blocks - blocknumber - 1 in
  for i = 1 to num_to_remove do
    blocks := List.tl !blocks
  done;
  IeleClientUtils.InMemoryWorldState.reset_blockhash ();
  `Bool true

let test_setChainParams params =
  let _ = test_rewindToBlock 0 in
  (* setChainParams sets the initial state of the blockchain so we overwrite the state of the genesis block *)
  let latest = List.hd !blocks in
  blocks := {state=[]; timestamp=latest.timestamp} :: List.tl !blocks;
  let accounts = params |> member "accounts" |> to_assoc in
  List.iter setBalance accounts;
 `Bool true

let get_account_field address blocknumber field convert default =
  let block = get_block blocknumber in
  try
    let acct = List.assoc ("0x" ^ address) block.state in
    acct |> member field |> convert
  with Not_found -> default

let eth_getCode address blocknumber = 
  `String (get_account_field address blocknumber "code" to_string "0x")

let eth_getBlockByNumber blocknumber =
  let block = get_block blocknumber in
  `Assoc [("timestamp", `String block.timestamp)]

let eth_getBalance address blocknumber =
  let balance = get_account_field address blocknumber "balance" to_string "0x00" in
  `String balance

let eth_isStorageEmpty address blocknumber =
  let storage = get_account_field address blocknumber "storage" to_assoc [] in
  (storage = [])

let test_modifyTimestamp timestamp = 
  nextTimestamp := Some timestamp;
  `Bool true

let miner_setEtherbase address =
  nextBeneficiary := address;
  `Bool true

let iele_sendTransaction tx =
  let tx_str = Yojson.Basic.to_string tx in
  pendingTx := tx_str :: !pendingTx;
  let hash = Cryptokit.hash_string (hash ()) tx_str in
  let hash_hex = to_hex_unsigned (Bytes.of_string hash) in
  `String hash_hex

let eth_getTransactionReceipt hash =
  StringMap.find hash !receipts

let mine_block () =  
  let beneficiary = of_hex !nextBeneficiary in
  let now = Unix.gettimeofday () in
  let now_millis = Int64.of_float (now *. 1000.0) in
  let timestamp = match !nextTimestamp with
  | None -> now_millis
  | Some timestamp -> Int64.of_int timestamp
  in
  let difficulty = Bytes.empty in
  let number = IeleClientUtils.of_z (Z.of_int (List.length !blocks)) in
  let gas_limit = of_hex "0x7a1200" in
  let header = {beneficiary=beneficiary;unix_timestamp=timestamp;number=number;difficulty=difficulty;gas_limit=gas_limit} in
  let latest = List.hd !blocks in
  let initial_state = latest.state in
  let timestamp = Int64.to_string timestamp in
  if List.length !pendingTx = 0 then begin
    let new_block = {state=initial_state; timestamp=timestamp} in
    blocks := new_block :: !blocks
  end else begin
    let tx_str = List.hd !pendingTx in
    pendingTx := List.tl !pendingTx;
    let tx = Yojson.Basic.from_string tx_str in
    let post_state, call_result = exec_transaction (Z.of_string "1000000000000000") false "gasprice" "gas" header initial_state tx in
    (* TODO: apply gas for mine*)
    let new_block = {state=post_state; timestamp=timestamp} in
    blocks := new_block :: !blocks;
    let hash = Cryptokit.hash_string (hash ()) tx_str in
    let hash_hex = to_hex_unsigned (Bytes.of_string hash) in
    let tx_gas = tx |> member "gas" |> to_string in
    let z_tx_gas = IeleClientUtils.to_z (of_hex tx_gas) in
    let z_gas_remaining = IeleClientUtils.to_z (call_result.gas_remaining) in
    let z_gas_used = Z.sub z_tx_gas z_gas_remaining in
    let gasUsed = to_hex_unsigned (IeleClientUtils.of_z z_gas_used) in
    let status = to_hex_unsigned call_result.return_code in
    let blockNumber = to_hex_unsigned number in
    let output_bytes = unpack_output call_result.return_data in
    let output = List.map to_hex_unsigned output_bytes in
    let output_json = List.map (fun t -> `String t) output in
    let log_entry_to_json entry =
      let address = to_hex_unsigned entry.address in
      let topics = List.map to_hex_unsigned entry.topics in
      let json_topics = List.map (fun t -> `String t) topics in
      let data = to_hex_unsigned entry.data in
      `Assoc [("address", `String address); ("topics", `List json_topics); ("data", `String data)]
    in
    let logs = List.map log_entry_to_json call_result.logs in
    let owner = tx |> member "to" |> to_string in
    let txcreate = owner = "" || owner = "0x" in
    let receipt = `Assoc [("gasUsed", `String gasUsed); ("status", `String status); ("contractAddress", if txcreate && List.length output > 0 then `String (to_hex (List.hd output_bytes)) else `Null); ("output", `List output_json); ("blockNumber", `String blockNumber); ("logs", `List logs)] in
    IeleClientUtils.InMemoryWorldState.add_blockhash (Bytes.of_string (String.sub hash 0 32));
    receipts := StringMap.add hash_hex receipt !receipts
  end

let iele_call tx _ =
  let `String hash = iele_sendTransaction tx in
  mine_block ();
  blocks := List.tl !blocks;
  let receipt = eth_getTransactionReceipt hash in
  receipt |> member "output"

let test_mineBlocks n =
  for i = 1 to n do
    mine_block ()
  done;
  `Bool true
