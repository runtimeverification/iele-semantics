open Msg_types

module type WorldState = sig
  val get_account : bytes -> account
  val get_storage_data : bytes -> bytes -> bytes
  val get_code : bytes -> bytes
  val get_blockhash : int -> bytes
end

val of_z : Z.t -> bytes
val to_z : bytes -> Z.t

module type MockBlockhash = sig
  val hashes : bytes list
end

module InMemoryWorldState (Hashes : MockBlockhash) : WorldState
module NetworkWorldState : WorldState

val serve : Unix.sockaddr
            -> (Msg_types.call_context -> Msg_types.call_result)
            -> unit
