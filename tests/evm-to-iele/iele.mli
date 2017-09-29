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

val asm_iele : iele_op list -> string
val asm_iele_opcode : iele_opcode -> string
