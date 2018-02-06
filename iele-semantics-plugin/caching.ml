open Constants
open Constants.K
open World
open Msg_types

module type KWorldState = sig
  val get_balance : Z.t -> Z.t
  val get_nonce : Z.t -> Z.t
  val get_storage_data : Z.t -> Z.t -> Z.t
  val get_code : Z.t -> string
  val get_blockhash : Z.t -> Z.t
  val is_code_empty : Z.t -> bool

  val clear : unit -> unit
end

module IntHash = Hashtbl.Make(Z)

module Make ( W : World.WorldState ) : KWorldState = struct
  let blockhashes = IntHash.create 10
  let accounts = IntHash.create 10
  let codes = IntHash.create 10
  let storages = IntHash.create 10

  let getOrUpdate map key default =
    try
      IntHash.find map key
    with Not_found ->
      let result = default () in
      IntHash.add map key result;
      result

  let get_account acct =
    getOrUpdate accounts acct (fun () -> 
      let data = W.get_account (of_z acct) in
      if data.code_empty then IntHash.add codes acct "";
      data)

  let get_balance acct = to_z (get_account acct).balance
  let get_nonce acct = to_z (get_account acct).nonce
  let is_code_empty acct = (get_account acct).code_empty

  let get_storage_data acct index =
    let map = getOrUpdate storages acct (fun () -> IntHash.create 10) in
    getOrUpdate map index (fun () -> to_z (W.get_storage_data (of_z acct) (of_z index)))
       
  let get_code acct =
    getOrUpdate codes acct (fun () -> Bytes.to_string (W.get_code (of_z acct)))

  let get_blockhash offset =
    getOrUpdate blockhashes offset (fun () -> to_z (W.get_blockhash (Z.to_int offset)))

  let clear () =
    IntHash.clear blockhashes;
    IntHash.clear accounts;
    IntHash.clear codes;
    IntHash.clear storages
end
