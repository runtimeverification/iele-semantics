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

module InMemoryWorldState ( Hashes : MockBlockhash ) = struct
  let get_account _ = {nonce=zero;balance=zero;code_empty=true}
  let get_storage_data _ _ = zero
  let get_code _ = Bytes.empty
  let get_blockhash i = List.nth Hashes.hashes i
end

module NetworkWorldState = struct
  let get_account _ = failwith "unimplemented"
  let get_storage_data _ _ = failwith "unimplemented"
  let get_code _ = failwith "unimplemented"
  let get_blockhash _ = failwith "unimplemented"
end
