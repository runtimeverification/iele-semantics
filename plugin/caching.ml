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

let getOrUpdate hash key default =
  try Hashtbl.find hash key
  with Not_found -> let value = default () in Hashtbl.add hash key value; value
let getOrUpdateLocal local_hash key default =
  getOrUpdate (ThreadLocal.getOrUpdate local_hash (fun () -> Hashtbl.create 10))
    key default

module Make ( W : World.WorldState ) : KWorldState = struct
  module IntHash = Hashtbl.Make(Z)

  let accounts = ThreadLocal.create 10
  let storages = ThreadLocal.create 10
  let codes    = ThreadLocal.create 10
  let blockhashes = ThreadLocal.create 10

  let get_account acct =
    getOrUpdateLocal accounts acct (fun () -> W.get_account (of_z_width 20 acct))

  let get_balance acct = to_z_unsigned (get_account acct).balance
  let get_nonce acct = to_z_unsigned (get_account acct).nonce
  let is_code_empty acct = (get_account acct).code_empty

  let get_storage_data acct index =
    let map = getOrUpdateLocal storages acct (fun () -> Hashtbl.create 10) in
    getOrUpdate map index (fun () -> to_z (W.get_storage_data (of_z_width 20 acct) (of_z index)))

  let get_code acct =
    getOrUpdateLocal codes acct (fun () -> Bytes.to_string (W.get_code (of_z_width 20 acct)))

  let get_blockhash offset =
    getOrUpdateLocal blockhashes offset (fun () -> to_z_unsigned (W.get_blockhash (Z.to_int offset)))

  let clear () =
    ThreadLocal.remove accounts;
    ThreadLocal.remove storages;
    ThreadLocal.remove codes;
    ThreadLocal.remove blockhashes
end
