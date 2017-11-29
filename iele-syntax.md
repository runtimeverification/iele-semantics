```{.k .uiuck .rvk}
module IELE-SYNTAX
  imports IELE-COMMON
  // Identifiers
  syntax IeleName ::= r"(?<![A-Za-z0-9\\_\\.\\-\\$])[a-zA-Z\\.\\_\\-\\$][0-9a-zA-Z\\.\\_\\-\\$]*" [token]

  syntax NumericIeleName ::= r"[0-9]+" [token]
endmodule

module IELE-COMMON
  syntax NumericIeleName ::= Int
  syntax IeleName ::= NumericIeleName

  syntax GlobalName ::= "@" IeleName

  syntax LocalName ::= "%" IeleName

  syntax LocalNames ::= List{LocalName, ","} [klabel(localNameList)]

  // Constants
  syntax Constant ::= Int

  // Operands
  syntax LValue ::= LocalName | GlobalName
  
  syntax LValues ::= NeList{LValue, ","} [klabel(lvalueList)]

  syntax Operand ::= LValue | Constant

  syntax Operands ::= List{Operand, ","} [klabel(operandList), hybrid, strict]

  syntax NonEmptyOperands ::= NeList{Operand, ","} [klabel(operandList)]

  syntax Ints ::= List{Int, ","} [klabel(operandList)]

  // Assignment
  syntax AssignInst ::= LValue "=" Operand

  // Local Memory
  syntax LoadInst ::= LValue "=" "load" /* index */ Operand [hybrid, strict(2)]
  syntax LoadInst ::= LValue "=" "load" /* index */ Operand "," /* offset in bytes */ Operand "," /* size in bytes */ Operand [hybrid, seqstrict(2,3,4)]
  syntax StoreInst ::= "store" /* value */ Operand "," /* index */ Operand [hybrid, seqstrict(1,2)]
  syntax StoreInst ::= "store" /* value */ Operand "," /* index */ Operand "," /* offset in bytes */ Operand "," /* size in bytes */ Operand [hybrid, seqstrict(1,2,3,4)]

  // Account Storage
  syntax SLoadInst ::= LValue "=" "sload" /* index */ Operand [hybrid, strict(2)]
  syntax SStoreInst ::= "sstore" /* value */ Operand "," /* index */ Operand [hybrid, seqstrict(1,2)]

  // Expressions
  syntax IsZeroInst ::= LValue "=" "iszero" Operand [hybrid, strict(2)]
  syntax NotInst    ::= LValue "=" "not"    Operand [hybrid, strict(2)]

  syntax AddInst ::= LValue "=" "add" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax MulInst ::= LValue "=" "mul" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax SubInst ::= LValue "=" "sub" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax DivInst ::= LValue "=" "div" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax ExpInst ::= LValue "=" "exp" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax ModInst ::= LValue "=" "mod" Operand "," Operand [hybrid, seqstrict(2,3)]

  syntax AddModInst ::= LValue "=" "addmod" Operand "," Operand "," Operand [hybrid, seqstrict(2,3,4)]
  syntax MulModInst ::= LValue "=" "mulmod" Operand "," Operand "," Operand [hybrid, seqstrict(2,3,4)]
  syntax ExpModInst ::= LValue "=" "expmod" Operand "," Operand "," Operand [hybrid, seqstrict(2,3,4)]

  syntax ByteInst ::= LValue "=" "byte" /* byte index, 0 being the LSB */ Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax SExtInst ::= LValue "=" "sext" /* width in bytes */              Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax TwosInst ::= LValue "=" "twos" /* width in bytes */              Operand "," Operand [hybrid, seqstrict(2,3)]

  syntax AndInst ::= LValue "=" "and" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax OrInst  ::= LValue "=" "or"  Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax XorInst ::= LValue "=" "xor" Operand "," Operand [hybrid, seqstrict(2,3)]

  syntax Predicate ::= "lt" | "le" | "gt" | "ge" | "eq" | "ne"
  syntax CmpInst ::= LValue "=" "cmp" Predicate Operand "," Operand [hybrid, seqstrict(3,4)]

  syntax SHA3Inst ::= LValue "=" "sha3" Operand [hybrid, strict(2)]

  // Jumps, Calls, and Returns
  syntax JumpInst     ::= "br" IeleName
  syntax CondJumpInst ::= "br" Operand "," IeleName [hybrid, strict(1)]

  syntax LocalCallInst   ::= "call" GlobalName "(" Operands ")"
                           | LValues "=" "call" GlobalName "(" Operands ")" [hybrid, strict(3)]
  syntax AccountCallInst ::= "call" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand
                           | LValues "=" "call" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand [hybrid, seqstrict(3,4,5,6)]
  syntax AccountCallInst ::= "callcode" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand
                           | LValues "=" "callcode" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand [hybrid, seqstrict(3,4,5,6)]
  syntax AccountCallInst ::= "delegatecall" GlobalName "at" Operand "(" Operands ")" "gaslimit" Operand
                           | LValues "=" "delegatecall" GlobalName "at" Operand "(" Operands ")" "gaslimit" Operand [hybrid, seqstrict(3,4,5)]
  syntax AccountCallInst ::= "staticcall" GlobalName "at" Operand "(" Operands ")" "gaslimit" Operand
                           | LValues "=" "staticcall" GlobalName "at" Operand "(" Operands ")" "gaslimit" Operand [hybrid, seqstrict(3,4,5)]
  syntax SendInst        ::= "send" /* value */ Operand "to" /* account */ Operand [hybrid, seqstrict(1,2)]

  syntax ReturnInst ::= "ret" NonEmptyOperands [hybrid, strict(1)]
                      | "ret" "void"
  syntax RevertInst ::= "revert" NonEmptyOperands [hybrid, strict(1)]
                      | "revert" "void"

  // Logging
  syntax LogInst ::= "log" /* index */ Operand [hybrid, strict(1)]
                   | "log" /* index */ Operand "," NonEmptyOperands [hybrid, seqstrict(1,2)]

  // Account creation/deletion
  syntax CreateInst ::= LValue "=" "create" /* contract name */ IeleName "(" Operands ")" "send" Operand [hybrid, seqstrict(3,4)]
  syntax CreateInst ::= LValue "=" "copycreate" /* contract address */ Operand "(" Operands ")" "send" Operand [hybrid, seqstrict(2,3)]

  syntax SelfdestructInst ::= "selfdestruct" /* account to send balance */ Operand [hybrid, strict(1)]

  // Local and network state accessors
  //
  // For these opcodes, I chose to represent them as iele builtins that can be
  // called using the same syntax as in a local call, e.g
  //   %pc = call @iele.pc()
  //   %balance = call @iele.balance(%bank.account)
  //
  // The names of the buildins follow the llvm convention for intrinsics: The
  // name is a valid global name that starts with the prefix "iele.". This means
  // that no user-defined global name should start with the prefix "iele.".
  // TODO:The regexp for iele names doesn't reflect that currently, because I
  // don't know of a way to express that as a regexp and keep the regexp readable.

  syntax IeleName ::= "iele.invalid"     [token]
  // local state queries
  syntax IeleName ::= "iele.gas"         [token]
  syntax IeleName ::= "iele.gasprice"    [token]
  syntax IeleName ::= "iele.gaslimit"    [token]
  syntax IeleName ::= "iele.coinbase"    [token]
  syntax IeleName ::= "iele.timestamp"   [token]
  syntax IeleName ::= "iele.number"      [token]
  syntax IeleName ::= "iele.difficulty"  [token]
  syntax IeleName ::= "iele.address"     [token]
  syntax IeleName ::= "iele.origin"      [token]
  syntax IeleName ::= "iele.caller"      [token]
  syntax IeleName ::= "iele.callvalue"   [token]
  syntax IeleName ::= "iele.msize"       [token]
  syntax IeleName ::= "iele.codesize"    [token]
  syntax IeleName ::= "iele.blockhash"   [token]
  // account queries
  syntax IeleName ::= "iele.balance"     [token]
  syntax IeleName ::= "iele.extcodesize" [token]
  // precompiled contracts
  syntax IeleName ::= "iele.ecrec"     [token]
  syntax IeleName ::= "iele.sha256"    [token]
  syntax IeleName ::= "iele.rip160"    [token]
  syntax IeleName ::= "iele.id"        [token]
  syntax IeleName ::= "iele.ecadd"     [token]
  syntax IeleName ::= "iele.ecmul"     [token]
  syntax IeleName ::= "iele.ecpairing" [token]

  // Instructions
  syntax Instruction ::=
    AssignInst
  | LoadInst
  | StoreInst
  | SLoadInst
  | SStoreInst
  | IsZeroInst
  | NotInst
  | AddInst
  | MulInst
  | SubInst
  | DivInst
  | ExpInst
  | ModInst
  | AddModInst
  | MulModInst
  | ExpModInst
  | ByteInst
  | SExtInst
  | TwosInst
  | AndInst
  | OrInst
  | XorInst
  | CmpInst
  | SHA3Inst
  | JumpInst
  | CondJumpInst
  | LocalCallInst
  | AccountCallInst
  | SendInst
  | ReturnInst
  | RevertInst
  | LogInst
  | CreateInst
  | SelfdestructInst

  syntax Instructions ::= List{Instruction, ""} [klabel(instructionList)]

  // Blocks
  syntax LabeledBlock ::= IeleName ":" Instructions

  syntax LabeledBlocks ::= List{LabeledBlock, ""} [klabel(labeledBlockList)]

  syntax UnlabeledBlock ::= Instructions

  syntax Blocks [flatPredicate]
  syntax Blocks ::= UnlabeledBlock LabeledBlocks | LabeledBlocks

  // Functions
  syntax FunctionSignature ::= GlobalName "(" FunctionParameters ")"

  syntax FunctionParameters ::= LocalNames
                              | Int /* when desugared to just the number of parameters */

  syntax FunctionDefinition ::= 
    "define" FunctionSignature "{" Blocks "}"
  | "define" "public" FunctionSignature "{" Blocks "}"

  // Contract declarations
  syntax ContractDeclaration ::= "external" "contract" IeleName

  // Top-level definitions
  syntax TopLevelDefinition ::=
    FunctionDefinition
  | ContractDeclaration

  syntax TopLevelDefinitions ::= List{TopLevelDefinition, ""} [klabel(topLevelDefinitionList)]

  // Contracts
  syntax ContractDefinition ::= "contract" IeleName "!" /* size in bytes */ Int "{" TopLevelDefinitions "}"

  syntax Contract ::= List{ContractDefinition, ""} [klabel(contractDefinitionList)]

  // macros for empty return operand lists in calls and returns
  rule call NAME ( ARGS ) => .LValues = call NAME ( ARGS ) [macro]
  rule call NAME at CONTRACT ( ARGS ) send VALUE , gaslimit GLIMIT => .LValues = call NAME at CONTRACT ( ARGS ) send VALUE , gaslimit GLIMIT [macro]
  rule callcode NAME at CONTRACT ( ARGS ) send VALUE , gaslimit GLIMIT => .LValues = callcode NAME at CONTRACT ( ARGS ) send VALUE , gaslimit GLIMIT [macro]
  rule delegatecall NAME at CONTRACT ( ARGS ) gaslimit GLIMIT => .LValues = delegatecall NAME at CONTRACT ( ARGS ) gaslimit GLIMIT [macro]
  rule staticcall NAME at CONTRACT ( ARGS ) gaslimit GLIMIT => .LValues = staticcall NAME at CONTRACT ( ARGS ) gaslimit GLIMIT [macro]
  rule ret void => ret .NonEmptyOperands [macro]
  rule revert void => revert .NonEmptyOperands [macro]
endmodule
```
