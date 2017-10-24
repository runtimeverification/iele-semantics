open Evm
open Iele

let pow256 = Z.shift_left Z.one 256
let _32 = Z.of_int 32
let _31 = Z.of_int 31

let compatibility = true

let rec preprocess_evm (evm: evm_op list) : intermediate_op list = match evm with
| [] -> []
| `SDIV :: tl -> `DIV :: preprocess_evm tl
| `SMOD :: tl -> `MOD :: preprocess_evm tl
| (`DIV | `MOD | `GT | `LT as op) :: tl when compatibility -> `PUSH(_32) :: `TWOS :: `SWAP(1) :: `PUSH(_32) :: `TWOS :: `SWAP(1) :: op :: preprocess_evm tl
| (`ADDMOD | `MULMOD as op) :: tl when compatibility -> `PUSH(_32) :: `TWOS :: `SWAP(1) :: `PUSH(_32) :: `TWOS :: `SWAP(1) :: `SWAP(2) :: `PUSH(_32) :: `TWOS :: `SWAP(2) :: op :: preprocess_evm tl
| `SLT :: tl -> `LT :: preprocess_evm tl
| `SGT :: tl -> `GT :: preprocess_evm tl
| (`JUMP|`JUMPI) :: tl -> `INVALID :: preprocess_evm tl
| `PUSH(_,pc) :: `JUMP :: tl when Z.lt pc (Z.of_int 65536) -> `JUMP(Z.to_int pc) :: preprocess_evm tl
| `PUSH(_,pc) :: `JUMPI :: tl when Z.lt pc (Z.of_int 65536) -> `JUMPI(Z.to_int pc) :: preprocess_evm tl
| `PUSH(_,byte) :: `SIGNEXTEND :: tl -> `PUSH(Z.min byte _31) :: `SIGNEXTEND :: preprocess_evm tl
| _ :: (`JUMP|`JUMPI) :: _ -> failwith "dynamic jumps detected"
| `PUSH(n,v) :: op2 :: tl -> `PUSH(v) :: preprocess_evm (op2 :: tl)
| `PUSH(n,v) :: [] -> `PUSH(v) :: []
| `PC(pc) :: tl when compatibility -> `PUSH(Z.of_int pc) :: preprocess_evm tl
| `PC(_) :: tl -> `PC :: preprocess_evm tl
| `LOG(_) | `CALL | `CALLCODE | `DELEGATECALL | `STATICCALL | `EXTCODECOPY | `CODECOPY | `CALLDATACOPY | `RETURNDATACOPY
| `RETURN | `REVERT | `SSTORE | `ADDMOD | `MULMOD | `CREATE | `POP | `SELFDESTRUCT | `MSTORE | `MSTORE8 | `ADD | `MUL 
| `SUB | `DIV | `EXP | `MOD | `BYTE | `SIGNEXTEND | `AND | `OR | `XOR | `LT | `GT | `EQ | `SHA3 | `SWAP(_) | `INVALID
| `STOP | `MLOAD | `ISZERO | `NOT | `BLOCKHASH | `CALLDATALOAD | `BALANCE | `EXTCODESIZE | `SLOAD | `DUP(_)
| `GAS | `GASPRICE | `GASLIMIT | `COINBASE | `TIMESTAMP | `NUMBER | `DIFFICULTY | `ADDRESS | `ORIGIN | `CALLER 
| `CALLVALUE | `MSIZE | `CODESIZE | `CALLDATASIZE | `RETURNDATASIZE | `JUMPDEST _ as op :: tl-> op :: preprocess_evm tl

let rec set_nth l i v = match i with
| 0 -> v :: List.tl l
| _ -> (List.hd l) :: set_nth (List.tl l) (i-1) v

type evm_graph = (int * intermediate_op list * bool * int option) list

let stack_needed op = match op with
| `LOG(n) -> n + 2
| `DUP(n) -> n
| `SWAP(n) -> n + 1
| `CALL | `CALLCODE -> 7
| `DELEGATECALL | `STATICCALL -> 6
| `EXTCODECOPY -> 4
| `CODECOPY | `CALLDATACOPY | `RETURNDATACOPY | `ADDMOD | `MULMOD | `CREATE -> 3
| `RETURN | `REVERT | `SSTORE | `MSTORE | `MSTORE8 | `ADD | `MUL | `SUB | `DIV | `EXP | `MOD | `BYTE | `SIGNEXTEND
| `TWOS | `AND | `OR | `XOR | `LT | `GT | `EQ | `SHA3 -> 2
| `SELFDESTRUCT | `JUMPI(_) | `MLOAD | `ISZERO | `NOT | `BLOCKHASH | `CALLDATALOAD | `BALANCE | `EXTCODESIZE 
| `SLOAD | `POP -> 1
| `INVALID | `STOP | `JUMPDEST(_) | `JUMP(_) | `PC | `GAS | `GASPRICE | `GASLIMIT | `COINBASE | `TIMESTAMP
| `NUMBER | `DIFFICULTY  | `ADDRESS | `ORIGIN | `CALLER | `CALLVALUE | `MSIZE | `CODESIZE | `CALLDATASIZE 
| `RETURNDATASIZE | `PUSH(_) -> 0

let compute_cfg (intermediate: intermediate_op list) : evm_graph =
  let output = ref [] in
  let rev_component = ref [] in
  let delta = ref 0 in
  let max_needed = ref 0 in
  List.iter (fun op ->
    let old_component = !rev_component in
    rev_component := op :: !rev_component;
    let total_needed = stack_needed op in
    let diff_needed = total_needed - !delta in
    max_needed := max !max_needed diff_needed;
    (match op with
    | `LOG(n) -> delta := !delta - 2 - n
    | `CALL | `CALLCODE -> delta := !delta - 6
    | `DELEGATECALL | `STATICCALL -> delta := !delta - 5
    | `EXTCODECOPY -> delta := !delta - 4
    | `CODECOPY | `CALLDATACOPY | `RETURNDATACOPY -> delta := !delta - 3
    | `RETURN | `REVERT | `SSTORE | `ADDMOD | `MULMOD | `CREATE | `MSTORE | `MSTORE8 -> delta := !delta - 2
    | `POP | `ADD | `MUL | `SUB | `DIV | `EXP | `MOD | `BYTE | `SIGNEXTEND | `TWOS
    | `AND | `OR | `XOR | `LT | `GT | `EQ | `SHA3  -> delta := !delta - 1
    | `SWAP(_) | `MLOAD | `ISZERO | `NOT | `BLOCKHASH | `CALLDATALOAD | `BALANCE
    | `EXTCODESIZE | `SLOAD -> ()
    | `DUP(_) | `PUSH(_) | `PC | `GAS | `GASPRICE | `GASLIMIT | `COINBASE | `TIMESTAMP | `NUMBER | `DIFFICULTY
    | `ADDRESS | `ORIGIN | `CALLER | `CALLVALUE | `MSIZE | `CODESIZE | `CALLDATASIZE 
    | `RETURNDATASIZE -> delta := !delta + 1
    | `JUMPDEST pc -> 
      let component = List.rev old_component in
      rev_component := [`JUMPDEST pc];
      output := (!max_needed,component,true,None) :: !output;
      max_needed := 0;
      delta := 0
    | `JUMP(pc) ->
      let component = List.rev !rev_component in
      rev_component := [];
      output := (!max_needed,component,false,Some pc) :: !output;
      max_needed := 0;
      delta := 0
    | `JUMPI(pc) ->
      let component = List.rev !rev_component in
      rev_component := [];
      delta := !delta - 1;
      output := (!max_needed,component,true,Some pc) :: !output;
      max_needed := 0;
      delta := 0
    | `STOP | `INVALID ->
      let component = List.rev !rev_component in
      rev_component := [];
      output := (!max_needed,component,false,None) :: !output;
      max_needed := 0;
      delta := 0
    | `SELFDESTRUCT ->
      let component = List.rev !rev_component in
      rev_component := [];
      delta := !delta - 1;
      output := (!max_needed,component,false,None) :: !output;
      max_needed := 0;
      delta := 0
    )) intermediate;
  let component = List.rev !rev_component in
  output := (!max_needed,component,true,None) :: !output;
  List.rev !output

type iele_graph = (int list * iele_op list * int list * bool * int option) list * int

let convert_to_ssa (cfg : evm_graph) : iele_graph =
  let regcount = ref 0 in
  let components = List.map (fun (max_needed,ops,fallthrough,jump) -> 
  let stack = ref [] in
  for i = 1 to max_needed do
    stack := !regcount :: !stack;
    regcount := !regcount + 1
  done;
  let pre_stack = !stack in
  let ssa_ops = List.map (fun op ->
  let curr_stack = !stack in
  match op with
  | `POP -> 
    (match curr_stack with
     | [] -> Op(`INVALID,[])
     | _ :: tl -> stack := tl; Nop)
  | `DUP(i) ->
    if List.length curr_stack < i then Op(`INVALID,[])
    else (stack := List.nth curr_stack (i-1) :: curr_stack; Nop)
  | `SWAP(i) ->
    if List.length curr_stack < i+1 then Op(`INVALID,[])
    else (stack := List.nth curr_stack i :: List.tl (set_nth curr_stack i (List.hd curr_stack)); Nop)
  | `PUSH(v) -> 
    let op = LiOp(`LOADPOS,!regcount,v) in
    stack := !regcount :: curr_stack;
    regcount := !regcount + 1;
    op
  | `INVALID | `STOP | `JUMPDEST(_) | `JUMP(_) as op -> Op(op,[]) (* nullary consumer *)
  | `PC | `GAS | `GASPRICE | `GASLIMIT | `COINBASE | `TIMESTAMP | `NUMBER | `DIFFICULTY | `ADDRESS | `ORIGIN
  | `CALLER | `CALLVALUE | `MSIZE | `CODESIZE | `CALLDATASIZE | `RETURNDATASIZE as op-> 
    let op = Op(op,[!regcount]) in
    stack := !regcount :: curr_stack;
    regcount := !regcount + 1;
    op (* nullary operator *)
  | `MLOAD | `ISZERO | `NOT | `BLOCKHASH | `CALLDATALOAD | `BALANCE | `EXTCODESIZE | `SLOAD as op ->
    (match curr_stack with [] -> Op(`INVALID,[])
    | hd :: tl -> let op = Op(op,[!regcount;hd]) in
    stack := !regcount :: tl;
    regcount := !regcount + 1;
    op) (* unary operator *)
  | `SELFDESTRUCT | `JUMPI(_) as op -> 
    (match curr_stack with [] -> Op(`INVALID,[])
    | hd :: tl -> let op = Op(op,[hd]) in
    stack := tl;
    op) (* unary consumer *)
  | `ADD | `MUL | `SUB | `DIV | `EXP | `MOD | `BYTE | `SIGNEXTEND | `TWOS | `AND | `OR | `XOR
  | `LT | `GT | `EQ | `SHA3 as op->
    (match curr_stack with [] | _ :: [] -> Op(`INVALID,[])
    | r1 :: r2 :: tl -> let op = Op(op,[!regcount;r1;r2]) in
    stack := !regcount :: tl;
    regcount := !regcount + 1;
    op) (* binary operator *)
  | `MSTORE | `MSTORE8 | `RETURN | `REVERT | `LOG(0) | `SSTORE as op-> 
    (match curr_stack with [] | _ :: [] -> Op(`INVALID,[])
    | r1 :: r2 :: tl -> let op = Op(op,[r1;r2]) in
    stack := tl;
    op) (* binary consumer *)
  | `ADDMOD | `MULMOD | `CREATE as op ->
    (match curr_stack with [] | _ :: [] | _ :: _ :: [] -> Op(`INVALID,[])
    | r1 :: r2 :: r3 :: tl -> let op = Op(op,[!regcount;r1;r2;r3]) in
    stack := !regcount :: tl;
    regcount := !regcount + 1;
    op) (* ternary operator *)
  | `CODECOPY | `CALLDATACOPY | `RETURNDATACOPY | `LOG(1) as op ->
    (match curr_stack with [] | _ :: [] | _ :: _ :: [] -> Op(`INVALID,[])
    | r1 :: r2 :: r3 :: tl -> let op = Op(op,[r1;r2;r3]) in
    stack := tl;
    op) (* ternary consumer *)
  | `EXTCODECOPY | `LOG(2) as op ->
    (match curr_stack with [] | _ :: [] | _ :: _ :: [] | _ :: _ :: _ :: [] -> Op(`INVALID,[])
    | r1 :: r2 :: r3 :: r4 :: tl -> let op = Op(op,[r1;r2;r3;r4]) in
    stack := tl;
    op) (* quaternary consumer *)
  | `LOG(3) as op ->
    (match curr_stack with [] | _ :: [] | _ :: _ :: [] | _ :: _ :: _ :: [] | _ :: _ :: _ :: _ :: [] -> Op(`INVALID,[])
    | r1 :: r2 :: r3 :: r4 :: r5 :: tl -> let op = Op(op,[r1;r2;r3;r4;r5]) in
    stack := tl;
    op) (* 5-ary consumer *)
  | `LOG(4) as op ->
    (match curr_stack with [] | _ :: [] | _ :: _ :: [] | _ :: _ :: _ :: [] | _ :: _ :: _ :: _ :: [] | _ :: _ :: _ :: _ :: _ :: [] -> Op(`INVALID,[])
    | r1 :: r2 :: r3 :: r4 :: r5 :: r6 :: tl -> let op = Op(op,[r1;r2;r3;r4;r5;r6]) in
    stack := tl;
    op) (* 6-ary consumer *)
  | `LOG(_) -> failwith "invalid LOG operand"
  | `DELEGATECALL | `STATICCALL as op ->
    (match curr_stack with []|_::[]|_::_::[]|_::_::_::[]|_::_::_::_::[]|_::_::_::_::_::[] -> Op(`INVALID,[])
    | r1 :: r2 :: r3 :: r4 :: r5 :: r6 :: tl -> let op = Op(op,[!regcount;r1;r2;r3;r4;r5;r6]) in
    stack := !regcount :: tl;
    regcount := !regcount + 1;
    op) (* 6-ary operator *)
  | `CALL | `CALLCODE as op ->
    (match curr_stack with []|_::[]|_::_::[]|_::_::_::[]|_::_::_::_::[]|_::_::_::_::_::[]|_::_::_::_::_::_::[] -> Op(`INVALID,[])
    | r1 :: r2 :: r3 :: r4 :: r5 :: r6 :: r7 :: tl -> let op = Op(op,[!regcount;r1;r2;r3;r4;r5;r6;r7]) in
    stack := !regcount :: tl;
    regcount := !regcount + 1;
    op) (* 7-ary operator *)
  ) ops
  in (pre_stack,ssa_ops,!stack,fallthrough,jump)) cfg
  in components,!regcount

let get_incoming_edges ((ssa,regcount) : iele_graph) (idx: int) (ops: iele_op list) : int list list =
  let previous = if idx = 0 then [] else match List.nth ssa (idx - 1) with
  | (_,_,post_stack,true,_) -> [post_stack]
  | (_,_,_,false,_) -> []
  in
  match ops with
  | Op(`JUMPDEST(i),[]) :: _ -> let targets = List.map (fun (_,_,post_stack,_,jump) -> match jump with Some pc when pc = i -> [post_stack] | _ -> []) ssa in previous @ (List.flatten targets)
  | _ -> previous

let annotate_graph_with_incoming_edges ((ssa,regcount) as graph: iele_graph) : (int list list * int list * iele_op list) list =
 List.mapi (fun idx (pre_stack,ops,_,_,_) -> (get_incoming_edges graph idx ops,pre_stack,ops)) ssa

let replace_registers (find: int -> int) (op: iele_op) : iele_op = match op with
| Nop -> Nop
| Op(opcode,regs) -> Op(opcode,List.map find regs)
| LiOp(opcode,reg,payload) -> LiOp(opcode,(find reg),payload)

let rec discover_phi phi_web pre_stack post_stack = match pre_stack,post_stack with
  | (phi :: phis), (item :: items) -> IeleUtil.UnionFind.union phi_web phi item; discover_phi phi_web phis items
  | [], _ -> ()
  | _::_, [] -> failwith "found a predecessor basic block where registers cannot be inferred"

let resolve_phi ((ssa,regcount) as graph: iele_graph) : iele_op list list =
  let annotated_graph = annotate_graph_with_incoming_edges graph in
  let phi_web = IeleUtil.UnionFind.create regcount in
  let preprocessed_graph = List.map (fun (incoming_edges,pre_stack,ops) ->
    match incoming_edges, pre_stack with
    | [], _::_ -> [Op(`INVALID,[])]
    | _ -> List.iter (discover_phi phi_web pre_stack) incoming_edges; ops) annotated_graph in
  List.map (fun ops -> List.map (replace_registers (IeleUtil.UnionFind.find phi_web)) ops) preprocessed_graph

let alloc_registers (ops: iele_op list) : iele_op list = 
  let regs = Hashtbl.create 32 in
  let regcount = ref 0 in
  let all_ops = List.map (replace_registers (fun reg -> try Hashtbl.find regs reg with Not_found -> let new_reg = !regcount in Hashtbl.add regs reg new_reg; regcount := new_reg + 1; new_reg)) ops in
  let regbits = ref 0 in
  regcount := !regcount - 1;
  while !regcount > 0 do
    regbits := !regbits + 1;
    regcount := !regcount asr 1
  done;
  Op(`REGISTERS !regbits,[]) :: all_ops

let max_val = Z.sub (Z.shift_left Z.one 255) Z.one

let rec postprocess_iele iele = match iele with
| Nop :: tl -> postprocess_iele tl
| Op(`BYTE, [reg;byte;v]) :: tl -> LiOp(`LOADPOS, -2, _31) :: Op(`SUB, [byte; -2; byte]) :: Op(`BYTE, [reg;byte;v]) :: postprocess_iele tl
| Op(`MSTORE, regs) :: tl -> Op(`MSTORE256, regs) :: postprocess_iele tl
| Op(`MLOAD, regs) :: tl -> Op(`MLOAD256, regs) :: postprocess_iele tl
| Op(`CALLDATALOAD, [reg;datastart]) :: tl -> LiOp(`LOADPOS, -1, _32) :: Op(`CALLDATALOAD, [reg;datastart; -1]) :: postprocess_iele tl
| Op(`EXP, [reg;v1;v2]) :: tl when compatibility -> LiOp(`LOADPOS, -3, pow256) :: Op(`EXPMOD, [reg;v1;v2;-3]) :: postprocess_iele tl
| LiOp(`LOADPOS, reg, z) :: tl when compatibility && Z.gt z max_val -> LiOp(`LOADNEG, reg, Z.signed_extract z 0 256) :: postprocess_iele tl
| hd :: tl -> hd :: postprocess_iele tl
| [] -> []

let evm_to_iele (evm:evm_op list) : iele_op list =
  let preprocessed = preprocess_evm evm in
  let cfg = compute_cfg preprocessed in
  let ssa = convert_to_ssa cfg in
  let resolved = resolve_phi ssa in
  let flattened = List.flatten resolved in
  let postprocessed = postprocess_iele flattened in
  match postprocessed with
  | [] -> []
  | _::_ -> alloc_registers postprocessed
