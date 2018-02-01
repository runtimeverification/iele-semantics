open Constants
open Constants.K
open Prelude
open Msg_types

let run_transaction (ctx: call_context) : call_result =
  (match ctx.blockchain_config with
  | Some Iele_config -> ()
  | _ -> invalid_arg "Must pass a IELEConfig message as blockchain_config");
  let block_header = (match ctx.block_header with
  | Some hdr -> hdr
  | _ -> invalid_arg "Must pass a BlockHeader message as block_header") in
  let iscreate = Bytes.length ctx.owner_addr = 0 in
  if iscreate && ctx.function_ <> "" then failwith "Contract creation transactions cannot have a function set."; 
  let z_to = World.to_z ctx.owner_addr in
  let z_from = World.to_z ctx.origin_addr in
  let str_code = Bytes.to_string ctx.contract_code in
  let z_args = List.map World.to_z ctx.input_data in
  let z_value = World.to_z ctx.call_value in
  let z_gasprice = World.to_z ctx.gas_price in
  let z_gas = World.to_z ctx.gas_provided in
  let z_beneficiary = World.to_z block_header.beneficiary in
  let z_difficulty = World.to_z block_header.difficulty in
  let z_number = World.to_z block_header.number in
  let z_gaslimit = World.to_z block_header.gas_limit in
  let z_timestamp = Z.of_int64 block_header.unix_timestamp in
  let mode = [KApply0(LblNORMAL_IELE'Hyph'INFRASTRUCTURE)] in
  let schedule = [KApply0(LblALBE_IELE'Hyph'GAS)] in
  let k_args = List.map (fun z -> [Int z]) z_args in
  let kcell = [KApply14(LblrunVM,[Bool iscreate],[Int z_to],[Int z_from],[String str_code],[List(SortList,Lbl_List_,k_args)],[Int z_value],[Int z_gasprice],[Int z_gas],[Int z_beneficiary],[Int z_difficulty],[Int z_number],[Int z_gaslimit],[Int z_timestamp],[String ctx.function_])] in
  let map = KMap.add [KToken(SortKConfigVar, "$PGM")] kcell (KMap.add [KToken(SortKConfigVar, "$MODE")] mode (KMap.singleton [KToken(SortKConfigVar, "$SCHEDULE")] schedule)) in
  let module Def = (val Plugin.get ()) in
  let init_config = Def.eval (KApply(LblinitGeneratedTopCell, [[Map(SortMap,Lbl_Map_,map)]])) [Bottom] in
  let final_config,_ = Run.run_no_thread_opt init_config (-1) in
