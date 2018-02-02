open Yojson
open Yojson.Basic.Util
open Msg_types

let () = if Array.length Sys.argv <> 2 then 
  prerr_endline ("usage: " ^ Sys.argv.(0) ^ " <file.iele.json>")

let json = Yojson.Basic.from_channel (open_in Sys.argv.(1))

let context = Secp256k1.Context.create [Secp256k1.Context.Sign; Secp256k1.Context.Verify]
let hash = Cryptokit.Hash.keccak 256

let test =
  match json with
  `Assoc [(_, test)] -> test
| _ -> failwith "Invalid json structure: expected an object at top level."

let sort l =
  List.sort (fun (a,_) (b,_) -> compare a b) l

let of_hex str =
  let str = Hex.to_string (`Hex str) in
  Bytes.of_string str

let add_account (id,data) =
  let nonce = data |> member "nonce" |> to_string in
  let balance = data |> member "balance" |> to_string in
  let code = data |> member "code" |> to_string in
  let storage = data |> member "storage" |> to_assoc in
  let map = List.fold_left (fun map (k,v) -> World.StringMap.add (Bytes.to_string (of_hex k)) (of_hex (v |> to_string)) map) World.StringMap.empty storage in
  World.InMemoryWorldState.add_account (of_hex id) (of_hex nonce) (of_hex balance) (of_hex code) map

let init_state state =
  List.iter add_account state

let pack_input args function_ =
  let l = List.map (fun arg -> Rlp.RlpData (Rope.of_string (Bytes.to_string arg))) args in
  let rlp = Rlp.RlpList[Rlp.RlpData (Rope.of_string function_);Rlp.RlpList l] in
  Bytes.of_string (Rope.to_string (Rlp.encode rlp))

let unpack_output data =
  let rlp = Rlp.decode (Rope.of_string (Bytes.to_string data)) in
  match rlp with
  Rlp.RlpList(rets) ->
  List.map (fun rlp -> World.of_z (IeleVM.z_of_rlp rlp)) rets
| _ -> failwith "Invalid value where rlp-encoded return values expected"

let test_transaction header state tx result =
  init_state state;
  let owner = tx |> member "to" |> to_string in
  let secretkey = of_hex (tx |> member "secretKey" |> to_string) in
  let secretkey_buffer = Bigarray.Array1.of_array Bigarray.char Bigarray.c_layout (Array.init 32 (fun idx -> Bytes.get secretkey idx)) in
  let pubkey = Secp256k1.Public.of_secret context (Secp256k1.Secret.read_exn context secretkey_buffer) in
  let pubkey_string = String.init 64 (fun idx -> Bigarray.Array1.get (Secp256k1.Public.to_bytes ~compress:false context pubkey) (idx+1)) in
  let pubkey_hash = Cryptokit.hash_string hash pubkey_string in
  let origin = String.sub pubkey_hash 12 20 in
  let code = World.InMemoryWorldState.get_code (of_hex owner) in
  let args = List.map (fun json -> of_hex (json |> to_string)) (tx |> member "arguments" |> to_list) in
  let value = tx |> member "value" |> to_string in
  let gas_price = tx |> member "gasPrice" |> to_string in
  let gas_provided = tx |> member "gasLimit" |> to_string in
  let function_ = tx |> member "function" |> to_string in
  let ctx = {owner_addr=of_hex owner;caller_addr=Bytes.of_string origin;origin_addr=Bytes.of_string origin;contract_code=code;input_data=pack_input args function_;call_value=of_hex value;gas_price=of_hex gas_price;gas_provided=of_hex gas_provided;block_header=Some header;config=None;call_depth=0l} in
  let call_result = IeleVM.run_transaction ctx in
  let expected_return = List.map (fun json -> of_hex (json |> to_string)) (result |> member "out" |> to_list) in
  let rets = unpack_output call_result.return_data in
  let actual_return = List.map (fun arg -> `String (Bytes.to_string arg)) rets in
  if expected_return <> rets then
    prerr_endline ("failed: out:\n" ^ Yojson.Basic.to_string (`List actual_return));
  failwith "unimplemented"

let test_block state block =
  let bh = block |> member "blockHeader" in
  let beneficiary = bh |> member "beneficiary" |> to_string in
  let difficulty = bh |> member "difficulty" |> to_string in
  let number = bh |> member "number" |> to_string in
  let gas_limit = bh |> member "gasLimit" |> to_string in
  let timestamp = bh |> member "timestamp" |> to_string in
  let block_header = {beneficiary=of_hex beneficiary;difficulty=of_hex difficulty;number=of_hex number;gas_limit=of_hex gas_limit;unix_timestamp=Z.to_int64 (World.to_z (of_hex timestamp))} in
  let transactions = block |> member "transactions" |> to_list in
  let results = block |> member "results" |> to_list in
  List.fold_left2 (test_transaction block_header) state transactions results

let pre = sort (json |> member "pre" |> to_assoc)
let blocks = json |> member "blocks" |> to_list
let expected = sort (json |> member "postState" |> to_assoc)
let actual = sort (List.fold_left test_block pre blocks)
let () = if expected <> actual then
  prerr_endline ("failed: postState:\n" ^ Yojson.Basic.to_string (`Assoc actual))
