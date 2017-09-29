type evm_op = [
  `STOP
| `ADD
| `MUL
| `SUB
| `DIV
| `SDIV
| `MOD
| `SMOD
| `ADDMOD
| `MULMOD
| `EXP
| `SIGNEXTEND
| `LT
| `GT
| `SLT
| `SGT
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
| `POP
| `MLOAD
| `MSTORE
| `MSTORE8
| `SLOAD
| `SSTORE
| `JUMP
| `JUMPI
| `PC
| `MSIZE
| `GAS
| `JUMPDEST of int
| `PUSH of int * Z.t
| `DUP of int
| `SWAP of int
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
]

type intermediate_op = [
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
| `POP
| `MLOAD
| `MSTORE
| `MSTORE8
| `SLOAD
| `SSTORE
| `JUMP of int
| `JUMPI of int
| `PC
| `MSIZE
| `GAS
| `JUMPDEST of int
| `PUSH of Z.t
| `DUP of int
| `SWAP of int
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
]

val dasm_evm : string -> evm_op list
