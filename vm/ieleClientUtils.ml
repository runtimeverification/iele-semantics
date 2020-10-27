open Yojson
open Yojson.Basic.Util
open Msg_types

let g0_byte res b = match b with
| '\000' -> res := Z.add !res (Z.of_int 4)
| _ -> res := Z.add !res (Z.of_int 68)

let g0 txdata txcreate =
  let res = ref (Z.of_int (if txcreate then 53000 else 21000)) in
  Bytes.iter (g0_byte res) txdata;
  !res

let hash () = Cryptokit.Hash.keccak 256

let sort_assoc_list l =
  List.sort (fun (a,_) (b,_) -> compare a b) l

let rev_string str =
  let buf = Buffer.create (String.length str) in
  for i = (String.length str - 1) downto 0 do
    Buffer.add_char buf str.[i]
  done;
  Buffer.contents buf

let is_negative ch = match ch with
| '\000'..'\127' -> false
| '\128'..'\255' -> true

let z_bits z =
  let rec aux z n =
    if z = Z.zero then n
    else aux (Z.shift_right z 8) (n+8)
  in aux z 0

exception Break of int

let be_int i =
  let le = Z.to_bits i in
  let be = rev_string le in
  let len = String.length be in
  try
    for i = 0 to len - 1 do
      if be.[i] <> '\000' then raise (Break i)
    done;
    ""
  with Break i -> String.sub be i (len - i)

let of_z z =
  if Z.equal z Z.zero then Bytes.of_string "\000" else
  let twos = if Z.gt z Z.zero then z else Z.extract z 0 (z_bits (Z.sub (Z.mul (Z.neg z) (Z.of_int 2)) Z.one)) in
  let big_endian = be_int twos in
  if Z.gt z Z.zero && is_negative big_endian.[0] then
    Bytes.of_string ("\000" ^ big_endian)
  else
    Bytes.of_string big_endian

let of_z_width width i =
  let be = be_int i in
  let unpadded_byte_width = String.length be in
  let padded = Bytes.make width '\000' in
  Bytes.blit_string be 0 padded (width - unpadded_byte_width) unpadded_byte_width;
  padded

let to_z_unsigned b =
  let little_endian = rev_string (Bytes.to_string b) in
  Z.of_bits little_endian

let to_z b =
  let unsigned = to_z_unsigned b in
  if Bytes.length b > 0 && is_negative (Bytes.get b 0) then
  Z.signed_extract unsigned 0 (Bytes.length b * 8)
  else unsigned

let zero = of_z Z.zero

(** of_hex str takes a hex-encoded string and converts it to a sequence of bytes.
    of_hex ~signed:true also interprets the string as a signed integer *)
let of_hex ?signed:(signed=false) str =
  if str = "" then Bytes.empty else
  let str,neg = if String.sub str 0 1 = "-" then String.sub str 1 (String.length str - 1),true else str,false in
  let str = if String.sub str 0 2 = "0x" then String.sub str 2 (String.length str - 2) else str in
  let str = Hex.to_string (`Hex str) in
  let str = if signed && not neg && String.length str > 0 && is_negative str.[0] then "\000" ^ str else str in
  let res = Bytes.of_string str in
  if neg then of_z (Z.neg (to_z res)) else if signed then of_z (to_z res) else res

let to_hex bytes =
  if Bytes.length bytes = 0 then "0x00" else
  let str = Bytes.to_string bytes in
  let str = if String.length str > 1 && str.[0] = '\000' then
    String.sub str 1 (String.length str - 1)
  else
    str
  in
  "0x" ^ (match Hex.of_string str with (`Hex str) -> str)

let to_hex_unsigned bytes =
  let str = Bytes.to_string bytes in
  "0x" ^ (match Hex.of_string str with (`Hex str) -> str)

let abs_path rel =
  if Filename.is_relative rel then (Sys.getcwd ()) ^ "/" ^ rel else rel

let assemble file =
  let file_dir = Filename.dirname Sys.argv.(1) in
  let abs_file_dir = abs_path file_dir in
  let _in = Unix.open_process_in("iele-assemble " ^ (Filename.quote (abs_file_dir ^ "/" ^ file))) in
  let result = input_line _in in
  match Unix.close_process_in _in with
  | Unix.WEXITED 0 -> of_hex result
  | _ -> prerr_endline result; failwith ("failed to assemble " ^ file)

let get_code code =
  if code = "" then Bytes.empty else if String.sub code 0 2 = "0x" then of_hex code else assemble code

let checkpoint pre gas_provided gas_price origin : (string * Basic.json) list =
  List.map (fun (k,v) ->
    if of_hex k = origin then match v with
    | `Assoc a ->
      let without = List.remove_assoc "nonce" (List.remove_assoc "balance" a) in
      let new_nonce = of_z (Z.add Z.one (to_z_unsigned (of_hex ~signed:true (v |> member "nonce" |> to_string)))) in
      let gas_payment = Z.mul (to_z_unsigned gas_price) (to_z_unsigned gas_provided) in
      let new_balance = of_z (Z.sub (to_z_unsigned (of_hex ~signed:true (v |> member "balance" |> to_string))) gas_payment) in
      (k,`Assoc(("nonce", `String(to_hex new_nonce))::("balance", `String(to_hex new_balance))::without))
    | _ -> failwith "Invalid value where json object expected"
    else (k,v)) pre

module StringMap = Map.Make(String)

module InMemoryWorldState = struct

  type mock_account = {balance:bytes;nonce:bytes;code:bytes;storage:bytes StringMap.t}

  let accounts = ref StringMap.empty

  let add_account ~id ~nonce ~balance ~code storage =
    accounts := StringMap.add (Bytes.to_string id) {balance=balance;nonce=nonce;code=code;storage=storage} !accounts

  let hashes = ref []

  let add_blockhash hash =
    hashes := !hashes @ [hash]

  let get_account id =
    let id = Bytes.to_string id in
    try
      let acct = StringMap.find id !accounts  in
      {Msg_types.nonce=acct.nonce;Msg_types.balance=acct.balance;code_empty=Bytes.length acct.code = 0}
    with Not_found -> {Msg_types.nonce=Bytes.empty;Msg_types.balance=Bytes.empty;code_empty=true}

  let get_storage_data id offset =
    let id = Bytes.to_string id in
    let offset = Bytes.to_string offset in
    try (StringMap.find offset (StringMap.find id !accounts).storage) with Not_found -> zero

  let get_code id =
    let id = Bytes.to_string id in
    try (StringMap.find id !accounts).code with Not_found -> Bytes.empty

  let get_blockhash i = List.nth !hashes i

  let reset () = accounts := StringMap.empty; hashes := []
  let reset_state () = accounts := StringMap.empty
  let reset_blockhash () = hashes := [Bytes.make 32 '\000']
end

let add_account (id,data) =
  let acctID = of_hex id in
  let old_nonce = data |> member "nonce" |> to_string in
  let old_balance = data |> member "balance" |> to_string in
  let code = data |> member "code" |> to_string in
  let asm_code = get_code code in
  let storage = data |> member "storage" |> to_assoc in
  let nonce,balance = of_hex ~signed:true old_nonce,of_hex ~signed:true old_balance in
  let map = List.fold_left (fun map (k,v) -> StringMap.add (Bytes.to_string (of_hex ~signed:true k)) (of_hex ~signed:true (v |> to_string)) map) StringMap.empty storage in
  InMemoryWorldState.add_account acctID nonce balance asm_code map

let init_state state =
  InMemoryWorldState.reset_state ();
  List.iter add_account state

let pack_input args str =
  let l = List.map (fun arg -> Rlp.RlpData (Rope.of_string (Bytes.to_string arg))) args in
  let rlp = Rlp.RlpList[Rlp.RlpData (Rope.of_string str);Rlp.RlpList l] in
  Bytes.of_string (Rope.to_string (Rlp.encode rlp))

let z_of_rlp rlp =
  match rlp with
  Rlp.RlpData rope -> to_z (Bytes.of_string (Rope.to_string rope))
| Rlp.RlpList _ -> failwith "Invalid value where rlp-encoded string expected"

let unpack_output data =
  let rlp = Rlp.decode (Rope.of_string (Bytes.to_string data)) in
  match rlp with
  Rlp.RlpList(rets) ->
  List.map (fun rlp -> of_z (z_of_rlp rlp)) rets
| _ -> failwith "Invalid value where rlp-encoded return values expected"

let rlp_of_bytes (b: bytes) : Rlp.t =
  Rlp.RlpData (Rope.of_string (Bytes.to_string b))

let log_to_rlp (log: log_entry) : Rlp.t =
  Rlp.RlpList[rlp_of_bytes log.address; Rlp.RlpList(List.map rlp_of_bytes log.topics); rlp_of_bytes log.data]

module StringSet = Set.Make(String)

let update_storage_entry (storage: (string * Basic.json) list) (update: storage_update) : (string * Basic.json) list =
  let key = to_hex update.offset in
  let z_value = to_z update.data in
  let value = to_hex update.data in
  let without = List.remove_assoc key storage in
  if Z.equal z_value Z.zero then without else
  (key,`String value) :: without

let update_storage (storage: (string * Basic.json) list) (updates: storage_update list) : (string * Basic.json) list =
  sort_assoc_list (List.fold_left update_storage_entry storage updates)

let mod_acct_to_json (pre: (string * Basic.json) list) (acct: modified_account) : string * Basic.json =
  let address = to_hex_unsigned acct.address in
  let storage,old_code = try
    let account = List.assoc address pre in
    let storage = account |> member "storage" |> to_assoc in
    let code = account |> member "code" |> to_string in
    storage,code
  with Not_found -> [],"" in
  let new_code = if Bytes.length acct.code = 0 then
    old_code
  else
    to_hex_unsigned acct.code
  in
  (address,`Assoc(
    [("nonce",`String(to_hex acct.nonce));
     ("balance",`String(to_hex acct.balance));
     ("code",`String(new_code));
     ("storage",`Assoc(update_storage storage acct.storage_updates))]))

let update_state (pre: (string * Basic.json) list) (mod_accts: modified_account list) del_accts =
  let del_accts_set = StringSet.of_list (List.map to_hex_unsigned del_accts) in
  let mod_accts_set = StringSet.of_list (List.map (fun (acct: modified_account) -> to_hex_unsigned acct.address) mod_accts) in
  let deleted = List.filter (fun (acct,data) -> not (StringSet.mem acct del_accts_set || StringSet.mem acct mod_accts_set)) pre in
  deleted @ (List.map (mod_acct_to_json pre) mod_accts)

let rec rlp_to_hex = function
  | Rlp.RlpData str -> Rlp.RlpData (Rope.of_string (to_hex (Bytes.of_string (Rope.to_string str))))
  | Rlp.RlpList l -> Rlp.RlpList (List.map rlp_to_hex l)

let input_framed in_chan decoder =
  let len = input_binary_int in_chan in
  let bytes = Bytes.create len in
  really_input in_chan bytes 0 len;
  decoder (Pbrt.Decoder.of_bytes bytes)

let output_framed out_chan encoder v =
  let enc = Pbrt.Encoder.create() in
  encoder v enc;
  let encoded = Pbrt.Encoder.to_bytes enc in
  output_binary_int out_chan (Bytes.length encoded);
  output_bytes out_chan encoded;
  flush out_chan

let send addr ctx =
  let chans = Unix.open_connection addr in
  let hello = {version="1.1";config=Iele_config} in
    output_framed (snd chans) Msg_pb.encode_hello hello;
    output_framed (snd chans) Msg_pb.encode_call_context ctx;
    let result = ref None in
    while !result = None do
      let query = input_framed (fst chans) Msg_pb.decode_vmquery in
      match query with
      | Get_account { address=addr } ->
        let account = InMemoryWorldState.get_account addr in
        output_framed (snd chans) Msg_pb.encode_account account
      | Get_storage_data { address=addr; offset=off } ->
        let data = InMemoryWorldState.get_storage_data addr off in
        output_framed (snd chans) Msg_pb.encode_storage_data {data=data}
      | Get_code { address=addr } ->
        let code = InMemoryWorldState.get_code addr in
        output_framed (snd chans) Msg_pb.encode_code {code=code}
      | Get_blockhash { offset=off } ->
        let hash = InMemoryWorldState.get_blockhash (Int32.to_int off) in
        output_framed (snd chans) Msg_pb.encode_blockhash {hash=hash}
      | Call_result res ->
        result := Some res
    done;
    Unix.shutdown_connection (fst chans);
    close_in (fst chans);
    match !result with
    | Some res -> res
    | None -> failwith "unreachable"

let send_request ctx =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback,(int_of_string Sys.argv.(2))) in
  send addr ctx

let exec_transaction danseBlock signed gasPrice gasLimit header (state: (string * Basic.json) list) (tx: Basic.json) : (string * Basic.json) list * call_result =
  let gas_price = of_hex ~signed:signed (tx |> member gasPrice |> to_string) in
  let gas_provided = of_hex ~signed:signed (tx |> member gasLimit |> to_string) in
  let owner = tx |> member "to" |> to_string in
  let txcreate = owner = "" || owner = "0x" in
  let from = tx |> member "from" |> to_string in
  let origin = of_hex from in
  let checkpoint_state = checkpoint state gas_price gas_provided origin in
  init_state checkpoint_state;
  let args = List.map (fun json -> of_hex ~signed:signed (json |> to_string)) (tx |> member "arguments" |> to_list) in
  let value = tx |> member "value" |> to_string in
  let data = if txcreate then
    let data_str = tx |> member "contractCode" |> to_string in
    Bytes.to_string (get_code data_str)
  else
    tx |> member "function" |> to_string
  in
  let txdata = pack_input args data in
  let g0 = g0 txdata txcreate in
  let gas_provided = Z.sub (to_z_unsigned gas_provided) g0 in
  let ctx = {recipient_addr=of_hex owner;caller_addr=origin;input_data=txdata;call_value=of_hex ~signed:signed value;gas_price=gas_price;gas_provided=of_z gas_provided;block_header=Some header;config=Iele_config} in
  let call_result = send_request ctx in
  let post_state = update_state checkpoint_state call_result.modified_accounts call_result.deleted_accounts in
  post_state, call_result
