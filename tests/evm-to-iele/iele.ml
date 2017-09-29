type iele_opcode = [
  `STOP
| `ADD
| `MUL
| `SUB
| `DIV
| `MOD
| `ADDMOD
| `MULMOD
| `EXP
| `SIGNEXTEND
| `LT
| `GT
| `EQ
| `ISZERO
| `AND
| `OR
| `XOR
| `NOT
| `BYTE
| `SHA3
| `ADDRESS
| `BALANCE
| `ORIGIN
| `CALLER
| `CALLVALUE
| `CALLDATALOAD
| `CALLDATASIZE
| `CALLDATACOPY
| `CODESIZE
| `CODECOPY
| `GASPRICE
| `EXTCODESIZE
| `EXTCODECOPY
| `RETURNDATASIZE
| `RETURNDATACOPY
| `BLOCKHASH
| `COINBASE
| `TIMESTAMP
| `NUMBER
| `DIFFICULTY
| `GASLIMIT
| `MLOAD8
| `MLOAD256
| `MLOAD
| `MSTORE8
| `MSTORE256
| `MSTORE
| `SLOAD
| `SSTORE
| `JUMP of int
| `JUMPI of int
| `PC
| `MSIZE
| `GAS
| `JUMPDEST of int
| `LOADI
| `LOG of int
| `CREATE
| `CALL
| `CALLCODE
| `RETURN
| `DELEGATECALL
| `STATICCALL
| `REVERT
| `INVALID
| `SELFDESTRUCT
| `REGISTERS of int
]

type iele_op =
| Nop
| Op of iele_opcode * int list
| LiOp of iele_opcode * int * Z.t

let asm_iele_opcode op = match op with
| `STOP -> "\x00"
| `ADD -> "\x01"
| `MUL -> "\x02"
| `SUB -> "\x03"
| `DIV -> "\x04"
| `MOD -> "\x06"
| `ADDMOD -> "\x08"
| `MULMOD -> "\x09"
| `EXP -> "\x0a"
| `SIGNEXTEND -> "\x0b"
| `LT -> "\x10"
| `GT -> "\x11"
| `EQ -> "\x14"
| `ISZERO -> "\x15"
| `AND -> "\x16"
| `OR -> "\x17"
| `XOR -> "\x18"
| `NOT -> "\x19"
| `BYTE -> "\x1a"
| `SHA3 -> "\x20"
| `ADDRESS -> "\x30"
| `BALANCE -> "\x31"
| `ORIGIN -> "\x32"
| `CALLER -> "\x33"
| `CALLVALUE -> "\x34"
| `CALLDATALOAD -> "\x35"
| `CALLDATASIZE -> "\x36"
| `CALLDATACOPY -> "\x37"
| `CODESIZE -> "\x38"
| `CODECOPY -> "\x39"
| `GASPRICE -> "\x3a"
| `EXTCODESIZE -> "\x3b"
| `EXTCODECOPY -> "\x3c"
| `RETURNDATASIZE -> "\x3d"
| `RETURNDATACOPY -> "\x3e"
| `BLOCKHASH -> "\x40"
| `COINBASE -> "\x41"
| `TIMESTAMP -> "\x42"
| `NUMBER -> "\x43"
| `DIFFICULTY -> "\x44"
| `GASLIMIT -> "\x45"
| `MLOAD8 -> "\x50"
| `MLOAD256 -> "\x51"
| `MLOAD -> "\x52"
| `MSTORE8 -> "\x53"
| `MSTORE256 -> "\x54"
| `MSTORE -> "\x55"
| `SLOAD -> "\x56"
| `SSTORE -> "\x57"
| `JUMP i -> "\x58" ^ (IeleUtil.be_int_width (Z.of_int i) 16)
| `JUMPI i -> "\x59" ^ (IeleUtil.be_int_width (Z.of_int i) 16)
| `PC -> "\x5a"
| `MSIZE -> "\x5b"
| `GAS -> "\x5c"
| `JUMPDEST i -> "\x5d" ^ (IeleUtil.be_int_width (Z.of_int i) 16)
| `LOADI -> "\x60"
| `REGISTERS i -> "\x80" ^ (IeleUtil.string_of_char (Char.chr i))
| `LOG(n) ->
  let byte = 0xa0 + n in
  let ch = Char.chr byte in
  IeleUtil.string_of_char ch
| `CREATE -> "\xf0"
| `CALL -> "\xf1"
| `CALLCODE -> "\xf2"
| `RETURN -> "\xf3"
| `DELEGATECALL -> "\xf4"
| `STATICCALL -> "\xfa"
| `REVERT -> "\xfd"
| `INVALID -> "\xfe"
| `SELFDESTRUCT -> "\xff"

let asm_iele_regs regs buf nregs =
  let z = List.fold_right (fun reg accum -> Z.add (Z.shift_left accum nregs) (Z.of_int reg)) regs Z.zero in
  Buffer.add_string buf (IeleUtil.be_int_width z (List.length regs * nregs))

let asm_iele_op op buf nregs = match op with
| Nop -> ()
| Op(opcode,regs) -> 
  Buffer.add_string buf (asm_iele_opcode opcode);
  asm_iele_regs regs buf nregs
| LiOp(opcode,r,payload) ->
  Buffer.add_string buf (asm_iele_opcode opcode);
  asm_iele_regs [r] buf nregs;
  let payload_be = IeleUtil.be_int payload in
  Buffer.add_string buf (IeleUtil.rlp_encode_string payload_be)

let rec asm_iele_aux ops buf nregs = match ops with
| [] -> ()
| op :: ops -> asm_iele_op op buf nregs; asm_iele_aux ops buf nregs

let asm_iele ops =
  let nregs = match ops with
  | Op(`REGISTERS n,[]) :: tail -> n
  | _ -> 5
  in
  let buf = Buffer.create ((List.length ops) * 2) in
  asm_iele_aux ops buf nregs;
  Buffer.contents buf
