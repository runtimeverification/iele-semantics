open Constants
open Constants.K
open World

module Cache = Caching.Make(World.NetworkWorldState)
(*
let get_blockhashes config = match config with
  [KApply7(Lbl'_LT_'generatedTop'_GT_', _, _, _, _, _, [KApply15(Lbl'_LT_'iele'_GT_', _, _, _, _, _, _, _, _, _, _, _, _, _, _, [KApply1(Lbl'_LT_'blockhash'_GT_', [List(_,_,hashes)])])], _)] ->
  Array.map (function [Int hash] -> of_z hash | _ -> failwith "invalid block hash") (Array.of_list hashes)
| _ -> failwith "get_blockhashes"
*)

let hook_getBalance c _ _ config _ = match c with
  [Int acct] ->
  [Int (Cache.get_balance acct)]
| _ -> failwith "getBalance"

let hook_getNonce c _ _ config _ = match c with
  [Int acct] ->
  [Int (Cache.get_nonce acct)]
| _ -> failwith "getNonce"

let hook_isCodeEmpty c _ _ config _ = match c with
  [Int acct] ->
  [Bool (Cache.is_code_empty acct)]
| _ -> failwith "isCodeEmpty"

let hook_getStorageData c _ _ config _ = match c with
  [Int acct], [Int index] ->
  [Int (Cache.get_storage_data acct index)]
| _ -> failwith "getStorageData"

let hook_getCode c _ _ config _ = match c with
  [Int acct] ->
  [String (Cache.get_code acct)]
| _ -> failwith "getCode"

let hook_getBlockhash c _ _ config _ = match c with
  [Int offset] ->
  [Int (Cache.get_blockhash offset)]
| _ -> failwith "getBlockhash"
