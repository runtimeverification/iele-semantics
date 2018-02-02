open Msg_types

val run_transaction : call_context -> call_result

val z_of_rlp : Rlp.t -> Z.t
