open Msg_types

module type WorldState = sig
  val get_account : bytes -> account
  val get_storage_data : bytes -> bytes -> bytes
  val get_code : bytes -> bytes
  val get_blockhash : int -> bytes
end

let of_z z = 
  let sign = if (Z.lt z Z.zero) then "\000" else "\001" in
  let str = sign ^ (Z.to_bits z) in
  Bytes.of_string str

let to_z b =
  let sign = if Bytes.get b 0 = '\000' then Z.neg Z.one else Z.one in
  let magnitude = Z.of_bits (Bytes.to_string (Bytes.sub b 1 ((Bytes.length b) - 1))) in
  Z.mul sign magnitude

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
