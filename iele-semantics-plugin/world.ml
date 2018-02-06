open Msg_types

module type WorldState = sig
  val get_account : bytes -> account
  val get_storage_data : bytes -> bytes -> bytes
  val get_code : bytes -> bytes
  val get_blockhash : int -> bytes
end

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

let to_z b =
  let little_endian = rev_string (Bytes.to_string b) in
  let unsigned = Z.of_bits little_endian in
  if Bytes.length b > 0 && is_negative (Bytes.get b 0) then
  Z.signed_extract unsigned 0 (Bytes.length b * 8)
  else unsigned

let zero = of_z Z.zero

module type MockBlockhash = sig
  val hashes : bytes list
end

module StringMap = Map.Make(String)

module InMemoryWorldState = struct

  type mock_account = {balance:bytes;nonce:bytes;code:bytes;storage:bytes StringMap.t}

  let accounts = ref StringMap.empty

  let add_account ~id ~nonce ~balance ~code storage = 
    accounts := StringMap.add (Bytes.to_string id) {balance=balance;nonce=nonce;code=code;storage=storage} !accounts

  let hashes = ref []

  let add_blockhash hash =
    hashes := hash :: !hashes
 
  let get_account id = 
    let id = Bytes.to_string id in
    try 
      let acct = StringMap.find id !accounts  in
      {Msg_types.nonce=acct.nonce;Msg_types.balance=acct.balance;code_empty=Bytes.length acct.code = 0}
    with Not_found -> {Msg_types.nonce=zero;Msg_types.balance=zero;code_empty=true}

  let get_storage_data id offset = 
    let id = Bytes.to_string id in
    let offset = Bytes.to_string offset in
    try (StringMap.find offset (StringMap.find id !accounts).storage) with Not_found -> zero

  let get_code id = 
    let id = Bytes.to_string id in
    try (StringMap.find id !accounts).code with Not_found -> Bytes.empty

  let get_blockhash i = List.nth !hashes i
end

module NetworkWorldState = struct
  let get_account _ = failwith "unimplemented"
  let get_storage_data _ _ = failwith "unimplemented"
  let get_code _ = failwith "unimplemented"
  let get_blockhash _ = failwith "unimplemented"
end
