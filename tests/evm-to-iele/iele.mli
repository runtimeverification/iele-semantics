type iele_opcode = [
  `STOP
| `ADD
| `MUL
| `SUB
| `DIV
| `MOD
| `EXP
| `ADDMOD
| `MULMOD
| `EXPMOD
| `SIGNEXTEND
| `TWOS
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
| `PC
| `MSIZE
| `GAS
| `LOADPOS
| `LOADNEG
| `JUMP of int
| `JUMPI of int
| `JUMPDEST of int
| `REGISTERS of int
| `LOG of int
| `CREATE
| `CALL
| `CALLCODE
| `DELEGATECALL
| `STATICCALL
| `LOCALCALL of int
| `RETURN
| `LOCALRETURN
| `REVERT
| `INVALID
| `SELFDESTRUCT
]

type iele_op =
| Nop
| Op of iele_opcode * int list
| LiOp of iele_opcode * int * Z.t

val asm_iele : iele_op list -> string
val asm_iele_opcode : iele_opcode -> string
