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
| `SHIFT
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
| `MLOADN
| `MLOAD
| `MSTOREN
| `MSTORE
| `SLOAD
| `SSTORE
| `MSIZE
| `GAS
| `MOVE
| `LOADPOS
| `LOADNEG
| `JUMP of int
| `JUMPI of int
| `JUMPDEST of int
| `REGISTERS of int
| `CALLDEST of int * int
| `EXTCALLDEST of int * int
| `FUNCTION of string
| `LOG of int
| `CREATE
| `COPYCREATE
| `CALL of int * int * int
| `CALLCODE of int * int * int
| `DELEGATECALL of int * int * int
| `STATICCALL of int * int * int
| `LOCALCALL of int * int * int
| `LOCALCALLI of int * int * int * int
| `RETURN of int
| `LOCALRETURN of int
| `REVERT of int
| `INVALID
| `SELFDESTRUCT
]

type iele_op =
| Nop
| Op of iele_opcode * int * int list
| VoidOp of iele_opcode * int list
| CallOp of iele_opcode * int list * int list
| LiOp of iele_opcode * int * Z.t

val asm_iele : iele_op list -> string
val asm_iele_opcode : iele_opcode -> string
