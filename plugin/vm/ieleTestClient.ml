open Yojson
open Yojson.Basic.Util
open Msg_types

let file = Sys.argv.(1)

let () = if Array.length Sys.argv <> 3 then
  prerr_endline ("usage: " ^ Sys.argv.(0) ^ " <file.iele.json> <port>")

let json = Yojson.Basic.from_channel (open_in file)

let context = Secp256k1.Context.create [Secp256k1.Context.Sign; Secp256k1.Context.Verify]
let hash () = Cryptokit.Hash.keccak 256

let test =
  match json with
  `Assoc [(_, test)] -> test
| _ -> failwith "Invalid json structure: expected an object at top level."

let sort l =
  List.sort (fun (a,_) (b,_) -> compare a b) l

let failed = ref false

let of_hex signed str =
  if str = "" then Bytes.empty else
  let str,neg = if String.sub str 0 1 = "-" then String.sub str 1 (String.length str - 1),true else str,false in
  let str = if String.sub str 0 2 = "0x" then String.sub str 2 (String.length str - 2) else str in
  let str = Hex.to_string (`Hex str) in
  let str = if signed && not neg && String.length str > 0 && World.is_negative str.[0] then "\000" ^ str else str in
  let res = Bytes.of_string str in
  if neg then World.of_z (Z.neg (World.to_z res)) else if signed then World.of_z (World.to_z res) else res

let of_hex_unsigned = of_hex false
let of_hex = of_hex true

let to_hex bytes =
  if Bytes.length bytes = 0 then "0x00" else
  let str = Bytes.to_string bytes in
  let str = if String.length str > 1 && str.[0] = '\000' then
    String.sub str 1 (String.length str - 1)
  else
    str
  in
  "0x" ^ (match Hex.of_string str with (`Hex str) -> str)

let abs_path rel =
  if Filename.is_relative rel then (Sys.getcwd ()) ^ "/" ^ rel else rel

let assemble file =
  let file_dir = Filename.dirname Sys.argv.(1) in
  let abs_file_dir = abs_path file_dir in
  let build_vm = Filename.dirname Sys.executable_name in
  let _in = Unix.open_process_in("cd " ^ (Filename.quote build_vm) ^ "/../../compiler/ && stack exec iele-assemble " ^ (Filename.quote (abs_file_dir ^ "/" ^ file))) in
  let result = input_line _in in
  match Unix.close_process_in _in with
  | Unix.WEXITED 0 -> of_hex_unsigned result
  | _ -> prerr_endline result; failwith ("failed to assemble " ^ file)

let checkpoint pre gas_provided gas_price origin : (string * Basic.json) list =
  List.map (fun (k,v) ->
    if of_hex_unsigned k = origin then match v with
    | `Assoc a ->
      let without = List.remove_assoc "nonce" (List.remove_assoc "balance" a) in
      let new_nonce = World.of_z (Z.add Z.one (World.to_z_unsigned (of_hex (v |> member "nonce" |> to_string)))) in
      let gas_payment = Z.mul (World.to_z_unsigned gas_price) (World.to_z_unsigned gas_provided) in
      let new_balance = World.of_z (Z.sub (World.to_z_unsigned (of_hex (v |> member "balance" |> to_string))) gas_payment) in
      (k,`Assoc(("nonce", `String(to_hex new_nonce))::("balance", `String(to_hex new_balance))::without))
    | _ -> failwith "Invalid value where json object expected"
    else (k,v)) pre

let add_account (id,data) =
  let acctID = of_hex_unsigned id in
  let old_nonce = data |> member "nonce" |> to_string in
  let old_balance = data |> member "balance" |> to_string in
  let code = data |> member "code" |> to_string in
  let asm_code = if code = "" then Bytes.empty else if String.sub code 0 2 = "0x" then of_hex code else assemble code in
  let storage = data |> member "storage" |> to_assoc in
  let nonce,balance = of_hex old_nonce,of_hex old_balance in
  let map = List.fold_left (fun map (k,v) -> World.StringMap.add (Bytes.to_string (of_hex k)) (of_hex (v |> to_string)) map) World.StringMap.empty storage in
  World.InMemoryWorldState.add_account acctID nonce balance asm_code map

let init_state state =
  List.iter add_account state

let pack_input args function_ txcreate data =
  let str = if txcreate then Bytes.to_string data else function_ in
  let l = List.map (fun arg -> Rlp.RlpData (Rope.of_string (Bytes.to_string arg))) args in
  let rlp = Rlp.RlpList[Rlp.RlpData (Rope.of_string str);Rlp.RlpList l] in
  Bytes.of_string (Rope.to_string (Rlp.encode rlp))

let unpack_output data =
  let rlp = Rlp.decode (Rope.of_string (Bytes.to_string data)) in
  match rlp with
  Rlp.RlpList(rets) ->
  List.map (fun rlp -> World.of_z (VM.z_of_rlp rlp)) rets
| _ -> failwith "Invalid value where rlp-encoded return values expected"

let rlp_of_bytes (b: bytes) : Rlp.t =
  Rlp.RlpData (Rope.of_string (Bytes.to_string b))

let log_to_rlp (log: log_entry) : Rlp.t =
  Rlp.RlpList[rlp_of_bytes log.address; Rlp.RlpList(List.map rlp_of_bytes log.topics); rlp_of_bytes log.data]

module StringSet = Set.Make(String)

let update_storage_entry (storage: (string * Basic.json) list) (update: storage_update) : (string * Basic.json) list =
  let key = to_hex update.offset in
  let z_value = World.to_z update.data in
  let value = to_hex update.data in
  let without = List.remove_assoc key storage in
  if Z.equal z_value Z.zero then without else
  (key,`String value) :: without

let update_storage (storage: (string * Basic.json) list) (updates: storage_update list) : (string * Basic.json) list =
  sort (List.fold_left update_storage_entry storage updates)

let mod_acct_to_json (pre: (string * Basic.json) list) (acct: modified_account) : string * Basic.json =
  let address = to_hex acct.address in
  let storage,old_code = try
    let account = List.assoc address pre in
    let storage = account |> member "storage" |> to_assoc in
    let code = account |> member "code" |> to_string in
    storage,code
  with Not_found -> [],"" in
  let new_code = if Bytes.length acct.code = 0 then
    old_code
  else
    to_hex acct.code
  in
  (address,`Assoc(
    [("nonce",`String(to_hex acct.nonce));
     ("balance",`String(to_hex acct.balance));
     ("code",`String(new_code));
     ("storage",`Assoc(update_storage storage acct.storage_updates))]))

let update_state (pre: (string * Basic.json) list) (mod_accts: modified_account list) del_accts =
  let del_accts_set = StringSet.of_list (List.map to_hex del_accts) in
  let mod_accts_set = StringSet.of_list (List.map (fun (acct: modified_account) -> to_hex acct.address) mod_accts) in
  let deleted = List.filter (fun (acct,data) -> not (StringSet.mem acct del_accts_set || StringSet.mem acct mod_accts_set)) pre in
  deleted @ (List.map (mod_acct_to_json pre) mod_accts)

let rec rlp_to_hex = function
  | Rlp.RlpData str -> Rlp.RlpData (Rope.of_string (to_hex (Bytes.of_string (Rope.to_string str))))
  | Rlp.RlpList l -> Rlp.RlpList (List.map rlp_to_hex l)

let send_request ctx =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback,(int_of_string Sys.argv.(2))) in
  World.send addr ctx

let test_transaction header (state: (string * Basic.json) list) (tx: Basic.json) (result: Basic.json) : (string * Basic.json) list =
  let gas_price = of_hex (tx |> member "gasPrice" |> to_string) in
  let gas_provided = of_hex (tx |> member "gasLimit" |> to_string) in
  let owner = tx |> member "to" |> to_string in
  let txcreate = owner = "" in
  let secretkey = of_hex_unsigned (tx |> member "secretKey" |> to_string) in
  let secretkey_buffer = Bigarray.Array1.of_array Bigarray.char Bigarray.c_layout (Array.init 32 (fun idx -> Bytes.get secretkey idx)) in
  let pubkey = Secp256k1.Public.of_secret context (Secp256k1.Secret.read_exn context secretkey_buffer) in
  let pubkey_string = String.init 64 (fun idx -> Bigarray.Array1.get (Secp256k1.Public.to_bytes ~compress:false context pubkey) (idx+1)) in
  let pubkey_hash = Cryptokit.hash_string (hash()) pubkey_string in
  let origin = String.sub pubkey_hash 12 20 in
  let origin = Bytes.of_string origin in
  let checkpoint_state = checkpoint state gas_price gas_provided origin in
  init_state checkpoint_state;
  let code = if txcreate then
    Bytes.empty
  else
    World.InMemoryWorldState.get_code (of_hex_unsigned owner)
  in
  let data_str = tx |> member "data" |> to_string in
  let data = if data_str = "" then Bytes.empty else assemble data_str in
  let args = List.map (fun json -> of_hex (json |> to_string)) (tx |> member "arguments" |> to_list) in
  let value = tx |> member "value" |> to_string in
  let function_ = tx |> member "function" |> to_string in
  let txdata = pack_input args function_ txcreate data in
  let g0 = VM.g0 txdata txcreate in
  let gas_provided = Z.sub (World.to_z_unsigned gas_provided) g0 in
  let ctx = {owner_addr=of_hex_unsigned owner;caller_addr=origin;origin_addr=origin;contract_code=code;input_data=txdata;call_value=of_hex value;gas_price=gas_price;gas_provided=World.of_z gas_provided;block_header=Some header;config=Iele_config;call_depth=0l} in
  let call_result = send_request ctx in
  let expected_return = List.map (fun json -> of_hex (json |> to_string)) (result |> member "out" |> to_list) in
  let rets = unpack_output call_result.return_data in
  let actual_return = List.map (fun arg -> `String (Bytes.to_string arg)) rets in
  if expected_return <> rets then begin
    prerr_endline ("failed " ^ file ^ ": out:\n" ^ Yojson.Basic.to_string (`List actual_return));
    failed := true;
  end;
  let expected_returncode = of_hex (result |> member "status" |> to_string) in
  if World.to_z expected_returncode <> World.to_z call_result.return_code then begin
    prerr_endline ("failed " ^ file ^ ": status:\n" ^ (to_hex call_result.return_code));
    failed := true;
  end;
  let expected_gas = of_hex (result |> member "gas" |> to_string) in
  if World.to_z_unsigned expected_gas <> World.to_z_unsigned call_result.gas_remaining then begin
    prerr_endline ("failed " ^ file ^ ": gas:\n" ^ (to_hex call_result.gas_remaining));
    failed := true;
  end;
  let expected_refund = of_hex (result |> member "refund" |> to_string) in
  if World.to_z_unsigned expected_refund <> World.to_z_unsigned call_result.gas_refund then begin
    prerr_endline ("failed " ^ file ^ ": refund:\n" ^ (to_hex call_result.gas_refund));
    failed := true;
  end;
  let expected_logs = of_hex_unsigned (result |> member "logs" |> to_string) in
  let rlp_logs = Rlp.RlpList(List.map log_to_rlp call_result.logs) in
  let actual_logs = Bytes.of_string (Cryptokit.hash_string (hash()) (Rope.to_string (Rlp.encode rlp_logs))) in
  if expected_logs <> actual_logs then begin
    prerr_endline ("failed " ^ file ^ ": logs:\nactual: " ^ (Rlp.display (rlp_to_hex rlp_logs)) ^ "\nhash: " ^ (to_hex actual_logs));
    failed := true;
  end;
  update_state checkpoint_state call_result.modified_accounts call_result.deleted_accounts

let rec canonicalize assoc =
  List.map (fun (k,v) -> match (k,v) with
  | "code", `String code -> "code",`String(if code = "" then "" else if String.sub code 0 2 = "0x" then code else to_hex (assemble code))
  | _, `Assoc a -> (k,`Assoc(sort (canonicalize a)))
  | _ -> k,v) assoc

let test_block state block =
  let bh = block |> member "blockHeader" in
  let beneficiary = bh |> member "coinbase" |> to_string in
  let difficulty = bh |> member "difficulty" |> to_string in
  let number = bh |> member "number" |> to_string in
  let gas_limit = bh |> member "gasLimit" |> to_string in
  let timestamp = bh |> member "timestamp" |> to_string in
  let block_header = {beneficiary=of_hex_unsigned beneficiary;difficulty=of_hex difficulty;number=of_hex number;gas_limit=of_hex gas_limit;unix_timestamp=Z.to_int64 (World.to_z (of_hex timestamp))} in
  let transactions = block |> member "transactions" |> to_list in
  let results = block |> member "results" |> to_list in
  List.fold_left2 (test_transaction block_header) state transactions results

let pre = test |> member "pre" |> to_assoc
let blocks = test |> member "blocks" |> to_list
let expected = sort (canonicalize (test |> member "postState" |> to_assoc))
let json_blockhashes = test |> member "blockhashes" |> to_list
let str_blockhashes = List.map to_string json_blockhashes
let blockhashes = List.map of_hex_unsigned str_blockhashes;;
List.iter World.InMemoryWorldState.add_blockhash (List.rev blockhashes);;
let actual = sort (canonicalize (List.fold_left test_block pre blocks));;
if expected <> actual then begin
  prerr_endline ("failed " ^ file ^ ": postState:\nexpected:" ^ Yojson.Basic.to_string (`Assoc expected) ^ "\nactual: " ^ Yojson.Basic.to_string (`Assoc actual));
  failed := true;
end;;
if !failed then failwith "assertions failed";;
