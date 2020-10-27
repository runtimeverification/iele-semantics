open Yojson.Basic

val eth_getCode : string -> string -> json
val eth_getBlockByNumber : string -> json
val eth_getTransactionReceipt : string -> json
val iele_sendTransaction : json -> json
val iele_call : json -> string -> json
val eth_getBalance : string -> string -> json
val eth_isStorageEmpty : string -> string -> bool
val personal_newAccount : unit -> json
val test_rewindToBlock : int -> json
val test_mineBlocks : int -> json
val test_modifyTimestamp : int -> json
val miner_setEtherbase : string -> json
val test_setChainParams : json -> json
