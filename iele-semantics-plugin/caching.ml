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
  let cache = IntHash.create 10

  let getOrUpdate map key default =
    try
      IntHash.find map key
    with Not_found ->
      let result = default () in
      IntHash.add map key result;
      result

  let thread_id () = Z.of_int (Thread.id (Thread.self()))

  let getOrUpdateLocal select key default =
    getOrUpdate (select (getOrUpdate cache (thread_id ()) (fun () -> (IntHash.create 10,IntHash.create 10,IntHash.create 10,IntHash.create 10)))) key default

  let accounts    (a,_,_,_) = a
  let storages    (_,s,_,_) = s
  let codes       (_,_,c,_) = c
  let blockhashes (_,_,_,b) = b

  let get_account acct =
    getOrUpdateLocal accounts acct (fun () -> 
      let data = W.get_account (of_z acct) in
      data)

  let get_balance acct = to_z (get_account acct).balance
  let get_nonce acct = to_z (get_account acct).nonce
  let is_code_empty acct = (get_account acct).code_empty

  let get_storage_data acct index =
    let map = getOrUpdateLocal storages acct (fun () -> IntHash.create 10) in
    getOrUpdate map index (fun () -> to_z (W.get_storage_data (of_z acct) (of_z index)))
       
  let get_code acct =
    getOrUpdateLocal codes acct (fun () -> Bytes.to_string (W.get_code (of_z acct)))

  let get_blockhash offset =
    getOrUpdateLocal blockhashes offset (fun () -> to_z (W.get_blockhash (Z.to_int offset)))

  let clear () =
    IntHash.remove cache (thread_id ())
end
