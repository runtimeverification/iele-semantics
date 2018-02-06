open Constants
open Constants.K
open Prelude
open Msg_types

let account_was_empty acctID =
  MANTIS.Cache.get_balance acctID = Z.zero &&
  MANTIS.Cache.get_nonce acctID = Z.zero &&
  MANTIS.Cache.is_code_empty acctID

let account_is_deleted _ acct = match acct with
  [KApply5(Lbl'_LT_'account'_GT_',[KApply1(Lbl'_LT_'acctID'_GT_',[Int acctID])],[KApply1(Lbl'_LT_'balance'_GT_',balance)],[KApply1(Lbl'_LT_'code'_GT_',code)],_,[KApply1(Lbl'_LT_'nonce'_GT_',nonce)])] ->
  let module Def = (val Plugin.get ()) in
  let result = Def.eval (KApply(LblaccountEmpty, [code;nonce;balance])) [Bottom] in
  (match result with [Bool isempty] ->
    isempty && not (account_was_empty acctID)
  | _ -> failwith "Invalid value where boolean was expected")
| _ -> failwith "Invalid value where account was expected"

let storage_key_is_modified acctID k v =
  match k,v with
  [Int key],[Int value] ->
  MANTIS.Cache.get_storage_data acctID key <> value
| _ -> failwith "Invalid values found where ints were expected"

let storage_is_modified acctID storage =
  KMap.fold (fun k v res -> res || storage_key_is_modified acctID k v) storage false

let code_is_modified acctID code =
  let module Def = (val Plugin.get ()) in
  let is_code_empty = match Def.eval (KApply(LblaccountEmpty, [code;[Int Z.zero];[Int Z.zero]])) [Bottom] with
  [Bool isempty] -> isempty
  | _ -> failwith "Invalid value where boolean was expected" in
  MANTIS.Cache.is_code_empty acctID &&
  not is_code_empty

let account_is_modified selfdestruct _ acct = 
match acct with
  [KApply5(Lbl'_LT_'account'_GT_',[KApply1(Lbl'_LT_'acctID'_GT_',[Int acctID])],[KApply1(Lbl'_LT_'balance'_GT_',[Int balance])],[KApply1(Lbl'_LT_'code'_GT_',code)],[KApply1(Lbl'_LT_'storage'_GT_',[Map(SortMap,Lbl_Map_,storage)])],[KApply1(Lbl'_LT_'nonce'_GT_',[Int nonce])])] ->
  not (List.mem acctID selfdestruct) && (
    MANTIS.Cache.get_balance acctID <> balance ||
    MANTIS.Cache.get_nonce acctID <> nonce ||
    code_is_modified acctID code ||
    storage_is_modified acctID storage
  )
| _ -> failwith "Invalid value where account was expected"

let k_to_z (k: k) : Z.t =
  match k with
  | [Int z] -> z
  | _ -> failwith "Invalid value where integer was expected"

let get_code_bytes code =
  let module Def = (val Plugin.get ()) in
  match code with
  | [KApply0(Lbl'Stop'List'LBraQuot'contractDefinitionList'QuotRBra')] -> Bytes.empty

  | _ ->
  match Def.eval (KApply(LblcontractBytes, [code])) [Bottom] with
  | [String bytes] -> Bytes.of_string bytes
  | _ -> failwith "Invalid value where string was expected"

let k_to_storage k v =
  match k, v with
  [Int key], [Int value] ->
  {offset=World.of_z key;data=World.of_z value}
| _ -> failwith "Invalid values found where ints were expected"

let k_to_mod_acct (acct: k) : modified_account = match acct with
  [KApply5(Lbl'_LT_'account'_GT_',[KApply1(Lbl'_LT_'acctID'_GT_',[Int acctID])],[KApply1(Lbl'_LT_'balance'_GT_',[Int balance])],[KApply1(Lbl'_LT_'code'_GT_',code)],[KApply1(Lbl'_LT_'storage'_GT_',[Map(SortMap,Lbl_Map_,storage)])],[KApply1(Lbl'_LT_'nonce'_GT_',[Int nonce])])] ->
  let address = World.of_z acctID in
  let nonce = World.of_z nonce in
  let balance = World.of_z balance in
  let code = 
  if code_is_modified acctID code then
    get_code_bytes code
  else
    Bytes.empty
  in
  let modified_storage_keys,_ = KMap.partition (storage_key_is_modified acctID) storage in
  let updates = KMap.fold (fun k v l -> (k_to_storage k v) :: l	) modified_storage_keys [] in
  {address=address;nonce=nonce;balance=balance;storage_updates=updates;code=code}
| _ -> failwith "Invalid value where account was expected"

let k_to_log (log: k) : log_entry = match log with
  [KApply3(LbllogEntry, [Int address], [List(SortList,Lbl_List_,topics)], [String data])] ->
  let z_topics = List.map k_to_z topics in
  {address=World.of_z_width 20 address;topics=List.map (World.of_z_width 32) z_topics;data=Bytes.of_string data}
| _ -> failwith "Invalid value found where SubstateLogEntry was expected"

let z_of_rlp rlp = 
  match rlp with
  Rlp.RlpData rope -> World.to_z (Bytes.of_string (Rope.to_string rope))
| Rlp.RlpList _ -> failwith "Invalid value where rlp-encoded string expected"

let unpack_input iscreate data code =
  let rlp = Rlp.decode (Rope.of_string (Bytes.to_string data)) in
  match rlp with
  Rlp.RlpList[Rlp.RlpData(rope);Rlp.RlpList(args)] ->
  let z_args = List.map z_of_rlp args in
  let str = Rope.to_string rope in
  if iscreate then
    str,z_args,""
  else
    Bytes.to_string code,z_args,str
| _ -> failwith "Invalid value where rlp-encoded args and function name expected"

let pack_output rets =
  let l = List.map (fun ret -> Rlp.RlpData (Rope.of_string (Bytes.to_string ret))) rets in
  let rlp = Rlp.RlpList l in
  Bytes.of_string (Rope.to_string (Rlp.encode rlp))

let run_transaction (ctx: call_context) : call_result =
  let block_header = (match ctx.block_header with
  | Some hdr -> hdr
  | _ -> invalid_arg "Must pass a BlockHeader message as block_header") in
  MANTIS.Cache.clear ();
  let iscreate = Bytes.length ctx.owner_addr = 0 in
  let z_to = World.to_z ctx.owner_addr in
  let z_from = World.to_z ctx.origin_addr in
  let str_code,z_args,function_ = unpack_input iscreate ctx.input_data ctx.contract_code in
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
  let kcell = [KApply14(LblrunVM,[Bool iscreate],[Int z_to],[Int z_from],[String str_code],[List(SortList,Lbl_List_,k_args)],[Int z_value],[Int z_gasprice],[Int z_gas],[Int z_beneficiary],[Int z_difficulty],[Int z_number],[Int z_gaslimit],[Int z_timestamp],[String function_])] in
  let map = KMap.add [KToken(SortKConfigVar, "$PGM")] kcell (KMap.add [KToken(SortKConfigVar, "$MODE")] mode (KMap.singleton [KToken(SortKConfigVar, "$SCHEDULE")] schedule)) in
  let module Def = (val Plugin.get ()) in
  let init_config = Def.eval (KApply(LblinitGeneratedTopCell, [[Map(SortMap,Lbl_Map_,map)]])) [Bottom] in
  let final_config,_ = Run.run_no_thread_opt init_config (-1) in
  let extracted = 
  try Def.eval (KApply(LblextractConfig, [final_config])) [Bottom] 
  with Stuck(k) -> prerr_endline (Prelude.print_k k); failwith "failed to execute extractConfig" in
  match extracted with
  [KApply7(LblvmResult,[List(SortList,Lbl_List_,k_rets)],[Int z_gas],[Int z_refund],[Int z_status],[List(SortList,Lbl_List_,k_selfdestruct)],[List(SortList,Lbl_List_,k_logs)],[KApply1(Lbl'_LT_'accounts'_GT_',[Map(SortAccountCellMap,Lbl_AccountCellMap_,k_accounts)])])] ->
  (let z_rets = List.map k_to_z k_rets in
  let rets = List.map World.of_z z_rets in
  let ret_data = pack_output rets in
  let gas = World.of_z z_gas in
  let refund = World.of_z z_refund in
  let error = World.of_z z_status in
  let z_selfdestruct = List.map k_to_z k_selfdestruct in
  let (del_accounts,keep_accounts) = KMap.partition account_is_deleted k_accounts in
  let del_acctkeys = match List.split (KMap.bindings del_accounts) with l,_ -> l in
  let z_alldeleted = z_selfdestruct @ (List.map (fun k -> match k with [KApply1(Lbl'_LT_'acctID'_GT_',[Int z])] -> z | _ -> failwith "Unexpected key not an acctID cell") del_acctkeys) in
  let deleted_accounts = List.map World.of_z z_alldeleted in
  let (k_mod_accounts,_) = KMap.partition (account_is_modified z_selfdestruct) keep_accounts in
  let mod_accounts = match List.split (KMap.bindings k_mod_accounts) with _,l -> List.map k_to_mod_acct l in
  let logs = List.map k_to_log k_logs in
  {return_data=ret_data;return_code=error;gas_remaining=gas;gas_refund=refund;error=not (Z.equal z_status Z.zero);modified_accounts=mod_accounts;deleted_accounts=deleted_accounts;touched_accounts=[];logs=logs})
| k -> prerr_endline (Prelude.print_k k); failwith "Unexpected value where vmResult expected"

let g0_byte res b = match b with
| '\000' -> res := Z.add !res (Z.of_int 4)
| _ -> res := Z.add !res (Z.of_int 68)

let g0 txdata txcreate =
  let res = ref (Z.of_int (if txcreate then 53000 else 21000)) in
  Bytes.iter (g0_byte res) txdata;
  !res
