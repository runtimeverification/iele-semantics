open Constants
open Constants.K

module type KWorldState = sig
  val get_balance : Z.t -> Z.t
  val get_nonce : Z.t -> Z.t
  val get_storage_data : Z.t -> Z.t -> Z.t
  val get_code : Z.t -> string
  val get_blockhash : Z.t -> Z.t
  val is_code_empty : Z.t -> bool

  val clear : unit -> unit
end

module Make ( W : World.WorldState ) : KWorldState
