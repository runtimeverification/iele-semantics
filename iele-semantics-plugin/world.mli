open Msg_types

module type WorldState = sig
  val get_account : bytes -> account
  val get_storage_data : bytes -> bytes -> bytes
  val get_code : bytes -> bytes
  val get_blockhash : int -> bytes
end

val of_z : Z.t -> bytes
val to_z : bytes -> Z.t

module StringMap : Map.S with type key = string

module InMemoryWorldState : sig
  include WorldState
  val add_account : id: bytes -> nonce: bytes -> balance: bytes -> code: bytes -> bytes StringMap.t -> unit
  val add_blockhash : bytes -> unit
end
module NetworkWorldState : WorldState
