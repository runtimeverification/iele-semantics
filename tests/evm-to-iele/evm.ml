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
| `PC of int
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
| `POP
| `MLOADN
| `MSTOREN
| `SLOAD
| `SSTORE
| `JUMP of int
| `JUMPI of int
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
| `LOCALRETURN
| `DELEGATECALL
| `STATICCALL
| `REVERT
| `INVALID
| `SELFDESTRUCT
]

let dasm_evm str =
  let strlen = String.length str in
  let rec dasm_evm_aux str strlen idx res =
    if idx >= String.length str then res else
    let op = String.get str idx in
    let dasm_op = match op with
    | '\x00' -> `STOP
    | '\x01' -> `ADD
    | '\x02' -> `MUL
    | '\x03' -> `SUB
    | '\x04' -> `DIV
    | '\x05' -> `SDIV
    | '\x06' -> `MOD
    | '\x07' -> `SMOD
    | '\x08' -> `ADDMOD
    | '\x09' -> `MULMOD
    | '\x0a' -> `EXP
    | '\x0b' -> `SIGNEXTEND
    | '\x0c'..'\x0f' -> `INVALID
    | '\x10' -> `LT
    | '\x11' -> `GT
    | '\x12' -> `SLT
    | '\x13' -> `SGT
    | '\x14' -> `EQ
    | '\x15' -> `ISZERO
    | '\x16' -> `AND
    | '\x17' -> `OR
    | '\x18' -> `XOR
    | '\x19' -> `NOT
    | '\x1a' -> `BYTE
    | '\x1b'..'\x1f' -> `INVALID
    | '\x20' -> `SHA3
    | '\x21'..'\x2f' -> `INVALID
    | '\x30' -> `ADDRESS
    | '\x31' -> `BALANCE
    | '\x32' -> `ORIGIN
    | '\x33' -> `CALLER
    | '\x34' -> `CALLVALUE
    | '\x35' -> `CALLDATALOAD
    | '\x36' -> `CALLDATASIZE
    | '\x37' -> `CALLDATACOPY
    | '\x38' -> `CODESIZE
    | '\x39' -> `CODECOPY
    | '\x3a' -> `GASPRICE
    | '\x3b' -> `EXTCODESIZE
    | '\x3c' -> `EXTCODECOPY
    | '\x3d' -> `RETURNDATASIZE
    | '\x3e' -> `RETURNDATACOPY
    | '\x3f' -> `INVALID
    | '\x40' -> `BLOCKHASH
    | '\x41' -> `COINBASE
    | '\x42' -> `TIMESTAMP
    | '\x43' -> `NUMBER
    | '\x44' -> `DIFFICULTY
    | '\x45' -> `GASLIMIT
    | '\x46'..'\x4f' -> `INVALID
    | '\x50' -> `POP
    | '\x51' -> `MLOAD
    | '\x52' -> `MSTORE
    | '\x53' -> `MSTORE8
    | '\x54' -> `SLOAD
    | '\x55' -> `SSTORE
    | '\x56' -> `JUMP
    | '\x57' -> `JUMPI
    | '\x58' -> `PC idx
    | '\x59' -> `MSIZE
    | '\x5a' -> `GAS
    | '\x5b' -> `JUMPDEST idx
    | '\x5c'..'\x5f' -> `INVALID
    | '\x60'..'\x7f' -> 
      let byte = Char.code op in
      let len = byte - 0x60 + 1 in
      let min_len = if idx + 1 + len > strlen then strlen - idx - 1 else len in
      let payload = String.sub str (idx+1) min_len in
      let real_payload = if min_len <> len then payload ^ (String.make (len - min_len) '\000') else payload in
      let payload_le = IeleUtil.rev_string real_payload in
      let payload_z = Z.of_bits payload_le in
      `PUSH(len, payload_z)
    | '\x80'..'\x8f' ->
      let byte = Char.code op in
      let len = byte - 0x80 + 1 in
      `DUP len
    | '\x90'..'\x9f' ->
      let byte = Char.code op in
      let len = byte - 0x90 + 1 in
      `SWAP len
    | '\xa0'..'\xa4' ->
      let byte = Char.code op in
      let len = byte - 0xa0 in
      `LOG len
    | '\xa5'..'\xef' -> `INVALID
    | '\xf0' -> `CREATE
    | '\xf1' -> `CALL
    | '\xf2' -> `CALLCODE
    | '\xf3' -> `RETURN
    | '\xf4' -> `DELEGATECALL
    | '\xf5'..'\xf9' -> `INVALID
    | '\xfa' -> `STATICCALL
    | '\xfb'|'\xfc' -> `INVALID
    | '\xfd' -> `REVERT
    | '\xfe' -> `INVALID
    | '\xff' -> `SELFDESTRUCT
    in match dasm_op with
    | `PUSH(n,_) -> dasm_evm_aux str strlen (idx+1+n) (dasm_op::res)
    | _ -> dasm_evm_aux str strlen (idx+1) (dasm_op::res)
  in
  let res = List.rev (dasm_evm_aux str strlen 0 []) in
  res

let dasm_hex_string evm =
  let bytes = Hex.to_string (`Hex evm) in
  dasm_evm bytes
