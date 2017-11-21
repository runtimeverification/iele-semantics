module IELE-SYNTAX
  // Identifiers
  syntax IeleName ::=
     r"(?<![A-Za-z0-9\\_\\.\\-\\$])[a-zA-Z\\.\\_\\-\\$][0-9a-zA-Z\\.\\_\\-\\$]*" [token, notInRules]
  |  r"[0-9]+" [token, notInRules]

  syntax GlobalName ::= "@" IeleName

  syntax LocalName ::= "%" IeleName

  syntax LocalNames ::= List{LocalName, ","} [klabel(localNameList)]

  // Constants
  syntax Constant ::= Int

  // Operands
  syntax LValue ::= LocalName | GlobalName
  
  syntax NonEmptyLValues ::= LValue | LValue "," NonEmptyLValues

  syntax Operand ::= LValue | Constant

  syntax Operands ::= List{Operand, ","} [klabel(operandList)]

  syntax NonEmptyOperands ::= Operand | Operand "," NonEmptyOperands

  // Assignment
  syntax AssignInst ::= LValue "=" Operand

  // Local Memory
  syntax LoadInst ::= LValue "=" "load" /* index */ Operand "," /* size in bytes */ Operand
  syntax StoreInst ::= "store" /* value */ Operand "," /* index */ Operand "," /* size in bytes */ Operand

  // Account Storage
  syntax SLoadInst ::= LValue "=" "sload" /* index */ Operand "," /* size in bytes */ Operand
  syntax SStoreInst ::= "sstore" /* value */ Operand "," /* index */ Operand "," /* size in bytes */ Operand

  // Expressions
  syntax IsZeroInst ::= LValue "=" "iszero" Operand
  syntax NotInst ::= LValue "=" "not" Operand

  syntax AddInst ::= LValue "=" "add" Operand "," Operand
  syntax MulInst ::= LValue "=" "mul" Operand "," Operand
  syntax SubInst ::= LValue "=" "sub" Operand "," Operand
  syntax DivInst ::= LValue "=" "div" Operand "," Operand
  syntax ExpInst ::= LValue "=" "exp" Operand "," Operand
  syntax ModInst ::= LValue "=" "mod" Operand "," Operand

  syntax AddModInst ::= LValue "=" "addmod" Operand "," Operand "," Operand
  syntax MulModInst ::= LValue "=" "mulmod" Operand "," Operand "," Operand
  syntax ExpModInst ::= LValue "=" "expmod" Operand "," Operand "," Operand

  syntax ByteInst ::= LValue "=" "byte" /* byte index, 0 being the LSB */ Operand "," Operand
  syntax SExtInst ::= LValue "=" "sext" /* width in bytes */ Operand "," Operand
  syntax TwosInst ::= LValue "=" "twos" /* width in bytes */ Operand "," Operand

  syntax AndInst ::= LValue "=" "and" Operand "," Operand
  syntax OrInst ::= LValue "=" "or" Operand "," Operand
  syntax XorInst ::= LValue "=" "xor" Operand "," Operand

  syntax Predicate ::= "lt" | "le" | "gt" | "ge" | "eq" | "ne"
  syntax CmpInst ::= LValue "=" "cmp" Predicate Operand "," Operand

  syntax SHA3Inst ::= LValue "=" "sha3" Operand "," Operand

  // Jumps, Calls, and Returns
  syntax JumpInst ::= "br" IeleName
  syntax CondJumpInst ::= "br" Operand "," IeleName
  syntax LocalCallInst ::= "call" GlobalName "(" Operands ")"
                         | NonEmptyLValues "=" "call" GlobalName "(" Operands ")"
  syntax AccountCallInst ::= "call" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand
                           | NonEmptyLValues "=" "call" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand
  syntax SendInst ::= "send" /* value */ Operand "to" /* account */ Operand
  syntax ReturnInst ::= "ret" Operands | "ret" "void"
  syntax RevertInst ::= "revert" Operands | "revert" "void"
  syntax StopInst ::= "stop"

  // Logging
  syntax LogInst ::= "log" /* index */ Operand "," /* size in bytes */ Operand
                   | "log" /* index */ Operand "," /* size in bytes */ Operand "," NonEmptyOperands

  // Account creation/deletion
  //
  // TODO: I think that what we currently have in the iele semantics repo for
  // CREATE and its arguments is going to change.
  // (https://github.com/runtimeverification/iele-semantics/blob/call/iele.md#account-creationdeletion)
  syntax CreateInst ::= LValue "=" "create" /* contract name */ IeleName "(" Operands ")" "send" Operand
  syntax SelfdestructInst ::= "selfdesctruct" /* account to send balance */ Operand

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
  syntax IeleName ::= "iele.invalid" [token]
  syntax IeleName ::= "iele.pc" [token]
  syntax IeleName ::= "iele.gas" [token]
  syntax IeleName ::= "iele.gasprice" [token]
  syntax IeleName ::= "iele.gaslimit" [token]
  syntax IeleName ::= "iele.coinbase" [token]
  syntax IeleName ::= "iele.timestamp" [token]
  syntax IeleName ::= "iele.number" [token]
  syntax IeleName ::= "iele.difficulty" [token]
  syntax IeleName ::= "iele.address" [token]
  syntax IeleName ::= "iele.origin" [token]
  syntax IeleName ::= "iele.caller" [token]
  syntax IeleName ::= "iele.callvalue" [token]
  syntax IeleName ::= "iele.msize" [token]
  syntax IeleName ::= "iele.codesize" [token]
  syntax IeleName ::= "iele.blockhash" [token]
  syntax IeleName ::= "iele.balance" [token]
  syntax IeleName ::= "iele.extcodesize" [token]

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
  | StopInst
  | LogInst
  | CreateInst
  | SelfdestructInst

  syntax Instructions ::= List{Instruction, ""} [klabel(instructionList)]

  // Labeled blocks
  syntax LabeledBlock ::= IeleName ":" Instructions

  syntax LabeledBlocks ::= List{LabeledBlock, ""} [klabel(labeledBlockList)]

  // Functions
  syntax FunctionSignature ::= GlobalName "(" FunctionParameters ")"

  syntax FunctionParameters ::= LocalNames

  syntax FunctionDefinition ::= "define" FunctionSignature "{" LabeledBlocks "}"

  // Global variables
  //syntax GlobalModifier ::= "global" | "constant" */

  syntax GlobalVariableDefinition ::= GlobalName "=" Constant

  // Iele contracts
  syntax TopLevelDefinition ::=
    GlobalVariableDefinition
  | FunctionDefinition

  syntax TopLevelDefinitions ::= List{TopLevelDefinition, ""} [klabel(topLevelDefinitionList)]

  syntax Contract ::= TopLevelDefinitions

  // The following opcodes are not currently represented in the textual syntax:
  //  REGS (not needed in this level)
  //  CODECOPY, EXTCODECOPY, DELEGATECALL, CALLCODE, STATICCALL (should we drop those?)
endmodule
