IELE Binary Encoding (Work In Progress)
=======================================

Here we define an ad-hoc binary encoding for IELE. This encoding is subject to change and should not be viewed as final. The actual semantics of IELE is defined in terms of a fragment of its textual representation.
You can use the IELE assembler provided with the semantics to convert from the textual encoding into this binary encoding.

```k
requires "iele.md"
requires "iele-syntax.md"

module IELE-BINARY
    imports IELE
    imports IELE-COMMON
```

Here we define the OpCode sort. An OpCode is the atomic unit of a IELE program in binary representation and a IELE contract can be expressed as a sequence of operations.
Each operation consists of its OpCode plus zero or more bytes containing the register operands to the operation. The OpCode consists of between one and seven bytes.
The first byte represents the operation in question, and contains enough information to determine and decode the rest of the OpCode. The remainder of the OpCode then
contains enough information to determine and decode the rest of the operation.

```k

    syntax NullOp ::= BRLABEL ( Int )
                    | BR ( Int )
                    | INVALID ()

    syntax UnOp   ::= LOADPOS ( Int , Int )
                    | LOADNEG ( Int , Int )
                    | BRC ( Int )
                    | GASLIMIT ()
                    | GASPRICE ()
                    | GAS ()
                    | ADDRESS ()
                    | ORIGIN ()
                    | CALLER ()
                    | CALLVALUE ()
                    | CODESIZE ()
                    | BENEFICIARY ()
                    | TIMESTAMP ()
                    | NUMBER ()
                    | DIFFICULTY ()
                    | MSIZE ()
                    | SELFDESTRUCT ()
                    | LOG0 ()
                    | REVERT()

    syntax BinOp ::= ISZERO ()
                   | NOT ()
                   | SHA3 ()
                   | MLOAD ()
                   | MSTORE ()
                   | SLOAD ()
                   | LOGARITHM2 ()
                   | EXTCODESIZE ()
                   | BLOCKHASH ()
                   | BALANCE ()
                   | SSTORE ()
                   | MOVE ()
                   | LOG1 ()

    syntax TernOp ::= ADD ()
                    | MUL ()
                    | SUB ()
                    | DIV ()
                    | MOD ()
                    | EXP ()
                    | SIGNEXTEND ()
                    | TWOS ()
                    | BSWAP ()
                    | LT ()
                    | LE ()
                    | GT ()
                    | GE ()
                    | EQ ()
                    | NE ()
                    | AND ()
                    | OR ()
                    | XOR ()
                    | SHIFT ()
                    | BYTE ()
                    | LOG2 ()

    syntax QuadOp ::= ADDMOD ()
                    | MULMOD ()
                    | EXPMOD ()
                    | MLOADN ()
                    | MSTOREN ()
                    | LOG3 ()

    syntax FiveOp ::= LOG4 ()

    syntax CreateOp ::= CREATE ( Int , Int )

    syntax CopyCreateOp ::= COPYCREATE ( Int )

    syntax CallOp ::= CALL ( Int , Int , Int )
                    | CALLDYN ( Int , Int )
                    | STATICCALL ( Int , Int , Int )
                    | STATICCALLDYN ( Int , Int )
                    | CALLADDRESS ( Int )

    syntax ReturnOp ::= RETURN ( Int )

    syntax LocalCallOp ::= LOCALCALL ( Int , Int , Int )
                         | LOCALCALLDYN ( Int , Int )

    syntax OpCode ::= NullOp
                    | UnOp
                    | CreateOp
                    | CopyCreateOp
                    | CallOp
                    | ReturnOp
                    | LocalCallOp
                    | TernOp
                    | QuadOp
                    | BinOp
                    | FiveOp
                    | encodingError()
```

After interpreting the strings representing programs as a `WordStack`, it should be changed into a `Contract` for use by the IELE semantics.

-   `#dasmContract` interperets `WordStack` as a `Contract`.
-   `#dasmFunction` interprets a single function of a contract represented as a `WordStack` into a `TopLevelDefinition`
-   `#dasmInstruction` disassembles the registers for a single instruction.
-   `#dasmOpCode` interperets a `Int` as an `OpCode`.

```{.k .bytes}
    syntax Contract ::= #dasmContract ( Bytes , IeleName )                                                                  [function]
                      | #dasmContract ( Int , Int , Bytes , IeleName )                                                      [function]
                      | #dasmContract ( Int , Int , Bytes , Int , Map, IeleName , TopLevelDefinitions, Int , Int , String ) [function]
 // ----------------------------------------------------------------------------------------------------------------------------------
    rule #dasmContract( BS, NAME ) => #dasmContract( 0, lengthBytes(BS), BS, NAME )
    rule #dasmContract( _, 0, BS, NAME ) => #emptyCode
    rule #dasmContract( I, N, BS, NAME ) => #dasmContract(I +Int 6, #asUnsigned(I, 4, BS) -Int 2, BS, BS[I +Int 5], 0 |-> init, NAME, .TopLevelDefinitions, 1, #asUnsigned(I, 4, BS) +Int 4, #unparseByteStack(BS[I .. N]))
      requires N >=Int 5 andBool BS[I +Int 4] ==Int 99

    rule #dasmContract( I, N, BS, NBITS, FUNCS, NAME, DEFS, M, SIZE, BYTES )
      => #dasmContract( I +Int 3 +Int #asUnsigned(I +Int 1, 2, BS), N -Int (3 +Int #asUnsigned(I +Int 1, 2, BS)), BS, NBITS, M |-> String2IeleName(#unparseByteStack(BS[I +Int 3 .. #asUnsigned(I +Int 1, 2, BS)])) FUNCS, NAME, DEFS, M +Int 1, SIZE, BYTES )
      requires N >=Int 3 andBool BS[I] ==Int 105

    rule #dasmContract( I, N, BS, NBITS, FUNCS, NAME, DEFS, M, SIZE, BYTES )
      => #dasmContract( I +Int 3, #asUnsigned(I +Int 1, 2, BS), BS, NAME +.+IeleName M) ++Contract #dasmContract(I +Int (3 +Int #asUnsigned(I +Int 1, 2, BS)), N -Int (3 +Int #asUnsigned(I +Int 1, 2, BS)), BS, NBITS, FUNCS, NAME, external contract NAME +.+IeleName M DEFS, M +Int 1, SIZE, BYTES)
      requires N >=Int 3 andBool BS[I] ==Int 106

    rule #dasmContract( I, N, BS, NBITS, FUNCS, NAME, DEFS, M, SIZE, BYTES ) => contract NAME ! SIZE BYTES { DEFS ++TopLevelDefinitions #dasmFunctions(I, N, BS, NBITS, FUNCS, NAME) } .Contract [owise]

    syntax Bool ::= #isValidContract(Bytes)                   [function]
                  | #isValidContract(Int, Int, Bytes)         [function, klabel(isValidContractAux)]
                  | #isValidStringTable(Int, Int, Bytes, Int) [function]
 // --------------------------------------------------------------------
    rule #isValidContract(CODE) => #isValidContract(0, lengthBytes(CODE), CODE)
    rule #isValidContract(_, 0, BS) => true
    rule #isValidContract(I, N, BS) => N -Int 4 >=Int #asUnsigned(I, 4, BS) andBool #isValidStringTable(I +Int 6, #asUnsigned(I, 4, BS) -Int 2, BS, BS[I +Int 5])
      requires N >=Int 6 andBool BS[I +Int 4] ==Int 99
    rule #isValidContract(_, _, _) => false [owise]

    rule #isValidStringTable(I, N, BS, NBITS) => N -Int 3 >=Int #asUnsigned(I +Int 1, 2, BS) andBool #isValidStringTable(I +Int (3 +Int #asUnsigned(I +Int 1, 2, BS)), N -Int (3 +Int #asUnsigned(I +Int 1, 2, BS)), BS, NBITS)
      requires N >=Int 3 andBool BS[I] ==Int 105

    rule #isValidStringTable(I, N, BS, NBITS) => N -Int 3 >=Int #asUnsigned(I +Int 1, 2, BS) andBool #isValidContract(I +Int 3, #asUnsigned(I +Int 1, 2, BS), BS) andBool #isValidStringTable(I +Int (3 +Int #asUnsigned(I +Int 1, 2, BS)), N -Int (3 +Int #asUnsigned(I +Int 1, 2, BS)), BS, NBITS)
      requires N >=Int 3 andBool BS[I] ==Int 106 andBool #asUnsigned(I +Int 1, 2, BS) =/=Int 0

    rule #isValidStringTable(I, N, BS, _) => false
      requires N >=Int 3 andBool BS[I] ==Int 106 andBool #asUnsigned(I +Int 1, 2, BS) ==Int 0
    rule #isValidStringTable(I, N, BS, NBITS) => #isValidFunctions(I, N, BS, NBITS) [owise]

    syntax priorities contractDefinitionList > contractAppend
    syntax priorities topLevelDefinitionList > topLevelAppend
    syntax IeleName ::= IeleName "+.+IeleName" IeleName [function]
    syntax Contract ::= Contract "++Contract" Contract [function, klabel(contractAppend)]
    syntax TopLevelDefinitions ::= TopLevelDefinitions "++TopLevelDefinitions" TopLevelDefinitions [function, klabel(topLevelAppend)]
 // ---------------------------------------------------------------------------------------------------------------------------------
    rule .Contract ++Contract Cs => Cs
    rule C Cs ++Contract Cs' => C (Cs ++Contract Cs')
    rule .TopLevelDefinitions ++TopLevelDefinitions Ds => Ds
    rule D Ds ++TopLevelDefinitions Ds' => D (Ds ++TopLevelDefinitions Ds')
    rule N +.+IeleName M => String2IeleName(IeleName2String(N) +String "." +String IeleName2String(M)) 

    syntax TopLevelDefinitions ::= #dasmFunctions ( Int, Int , Bytes , Int , Map , IeleName ) [function]
    syntax TopLevelDefinitions ::= #dasmFunction ( Bool , IeleName , IeleName , Int , Int , Int , Bytes , Int , Map , Instructions , K ) [function]
 // -----------------------------------------------------------------------------------------------------------------------------------------------
    rule #dasmFunctions(I, N, BS, NBITS, FUNCS, NAME) => #dasmFunction(false, getIeleName(FUNCS [ #asUnsigned(I +Int 1, 2, BS) ] orDefault #asUnsigned(I +Int 1, 2, BS)), NAME, #asUnsigned(I +Int 3, 2, BS), I +Int 5, N -Int 5, BS, NBITS, FUNCS, .Instructions, .K)
      requires N >=Int 5 andBool BS[I] ==Int 103
    rule #dasmFunctions(I, N, BS, NBITS, FUNCS, NAME) => #dasmFunction(true , getIeleName(FUNCS [ #asUnsigned(I +Int 1, 2, BS) ] orDefault #asUnsigned(I +Int 1, 2, BS)), NAME, #asUnsigned(I +Int 3, 2, BS), I +Int 5, N -Int 5, BS, NBITS, FUNCS, .Instructions, .K)
      requires N >=Int 5 andBool BS[I] ==Int 104
    rule #dasmFunctions(_, 0, _, _, _, _) => .TopLevelDefinitions

    syntax Bool ::= #isValidFunctions(Int, Int, Bytes, Int)           [function]
                  | #isValidFunction(Int, Int, Bytes, Int)            [function]
                  | #isValidInstruction(OpCode, Int, Int, Bytes, Int) [function]
                  | #isValidLoad(Int, Int, Bytes)                     [function]
 // ----------------------------------------------------------------------------
    rule #isValidFunctions(I, N, BS, NBITS) => #isValidFunction(I +Int 5, N -Int 5, BS, NBITS)
      requires N >=Int 5 andBool (BS[I] ==Int 103 orBool BS[I] ==Int 104)
    rule #isValidFunctions(_, 0, BS, _) => true
    rule #isValidFunctions(_, _, _, _) => false [owise]

    rule #isValidFunction(I, N, BS, NBITS) => #isValidFunctions(I, N, BS, NBITS)
      requires N >=Int 1 andBool (BS[I] ==Int 103 orBool BS[I] ==Int 104)
    rule #isValidFunction(_, 0, BS, _) => true
    rule #isValidFunction(I, N, BS, NBITS) => #isValidLoad(I +Int 1, N -Int 1, BS) andBool #isValidInstruction(#dasmOpCode(I, N, BS), I, N, BS, NBITS)
      requires N >=Int 1 andBool (BS[I] ==Int 97 orBool BS[I] ==Int 98)
    rule #isValidFunction(I, N, BS, NBITS) => #isValidInstruction(#dasmOpCode(I, N, BS), I, N, BS, NBITS) [owise]

    rule #isValidInstruction(encodingError(), _, _, _, _) => false
    rule #isValidInstruction(OP:OpCode, I, N, BS, NBITS) => N >=Int #opWidth(OP, NBITS) andBool #isValidFunction(I +Int #opWidth(OP, NBITS), N -Int #opWidth(OP, NBITS), BS, NBITS) [owise]

    rule #isValidLoad(I, N, BS) => N >=Int #loadLen(BS[I .. N]) +Int #loadOffset(BS[I .. N])

    rule #dasmFunction(false, NAME, CNAME, SIG, I, N, BS, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(I, N, BS, NBITS, FUNCS, CNAME)
      requires N >=Int 1 andBool (BS[I] ==Int 103 orBool BS[I] ==Int 104)
    rule #dasmFunction(true, NAME, CNAME, SIG, I, N, BS, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(I, N, BS, NBITS, FUNCS, CNAME)
      requires N >=Int 1 andBool (BS[I] ==Int 103 orBool BS[I] ==Int 104)

    rule #dasmFunction(false, NAME, CNAME, SIG, I, 0, BS, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions
    rule #dasmFunction(true, NAME, CNAME, SIG, I, 0, BS, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, I, N, BS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, I, N, BS, NBITS, FUNCS, INSTRS, #dasmOpCode(I, N, BS)) [owise]

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, I, N, BS, NBITS, FUNCS, INSTRS, OP:OpCode) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, I +Int #opWidth(OP, NBITS), N -Int #opWidth(OP, NBITS), BS, NBITS, FUNCS, #dasmInstruction(OP, I +Int #opCodeWidth(OP), #opWidth(OP, NBITS) -Int #opCodeWidth(OP), BS, NBITS, FUNCS, CNAME) INSTRS, .K)

    syntax Blocks ::= #toBlocks ( Instructions ) [function]
                    | #toBlocks ( Instructions , Blocks ) [function, klabel(#toBlockAux)]
 // -------------------------------------------------------------------------------------
    rule #toBlocks(INSTRS) => #toBlocks(INSTRS, .LabeledBlocks)
    rule #toBlocks(.Instructions, BLOCKS) => BLOCKS
    rule #toBlocks(label(LABEL) INSTRS, UNLABELEDBLOCK::Instructions BLOCKS) => #toBlocks(INSTRS, LABEL : UNLABELEDBLOCK BLOCKS)
    rule #toBlocks(label(LABEL) INSTRS, LABELEDBLOCK::LabeledBlock BLOCKS) => #toBlocks(INSTRS, LABEL : .Instructions LABELEDBLOCK BLOCKS)
    rule #toBlocks(label(LABEL) INSTRS, .LabeledBlocks) => #toBlocks(INSTRS, LABEL : .Instructions .LabeledBlocks)
    rule #toBlocks(INSTR INSTRS, UNLABELEDBLOCK::Instructions BLOCKS) => #toBlocks(INSTRS, INSTR UNLABELEDBLOCK BLOCKS)
      requires notBool isPseudoInstruction(INSTR)
    rule #toBlocks(INSTR INSTRS, LABELEDBLOCK::LabeledBlock BLOCKS) => #toBlocks(INSTRS, INSTR .Instructions LABELEDBLOCK BLOCKS)
      requires notBool isPseudoInstruction(INSTR)
    rule #toBlocks(INSTR INSTRS, .LabeledBlocks) => #toBlocks(INSTRS, INSTR .Instructions .LabeledBlocks)
      requires notBool isPseudoInstruction(INSTR)

    syntax PseudoInstruction ::= label ( Int )
    syntax Instruction ::= PseudoInstruction
    syntax Instruction ::= #dasmInstruction ( OpCode , Int , Int , Bytes , Int , Map , IeleName ) [function]
                         | #dasmInstruction ( OpCode , Int , Int , Int , Map , IeleName ) [function, klabel(#dasmInstructionAux)]
 // -----------------------------------------------------------------------------------------------------------------------------
    rule #dasmInstruction ( LOADPOS(M, W), I, N, BS, NBITS, FUNCS, NAME ) => #dasmInstruction(LOADPOS(M, W), #asUnsigned(I, NBITS up/Int 8, BS),                            NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME)
    rule #dasmInstruction ( LOADNEG(M, W), I, N, BS, NBITS, FUNCS, NAME ) => #dasmInstruction(LOADNEG(M, W), #asUnsigned(I, NBITS up/Int 8, BS),                            NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME)
    rule #dasmInstruction ( OP,            I, N, BS, NBITS, FUNCS, NAME ) => #dasmInstruction(OP,            #asUnsigned(I, #opWidth(OP, NBITS) -Int #opCodeWidth(OP), BS), NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME) [owise]

    rule #dasmInstruction ( LOADPOS ( _, I ),  R, W, M, _, _ ) => %(R, W, M, 0) = I
    rule #dasmInstruction ( LOADNEG ( _, I ),  R, W, M, _, _ ) => %(R, W, M, 0) = (0 -Int I)
    rule #dasmInstruction ( BR ( LABEL ),      _, _, _, _, _ ) => br LABEL 
    rule #dasmInstruction ( INVALID (),        R, W, M, _, _ ) => .LValues = call @iele.invalid ( .Operands )
    rule #dasmInstruction ( BRLABEL ( LABEL ), _, _, _, _, _ ) => label ( LABEL )

    rule #dasmInstruction ( SELFDESTRUCT (), R, W, M, _, _ ) => selfdestruct %(R, W, M, 0)
    rule #dasmInstruction ( BRC ( LABEL ),   R, W, M, _, _ ) => br %(R, W, M, 0) , LABEL
    rule #dasmInstruction ( LOG0 (),         R, W, M, _, _ ) => log %(R, W, M, 0)
    rule #dasmInstruction ( CALLVALUE (),    R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.callvalue   ( .Operands )
    rule #dasmInstruction ( GASLIMIT (),     R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.gaslimit    ( .Operands ) 
    rule #dasmInstruction ( GASPRICE (),     R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.gasprice    ( .Operands ) 
    rule #dasmInstruction ( GAS (),          R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.gas         ( .Operands ) 
    rule #dasmInstruction ( ADDRESS (),      R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.address     ( .Operands ) 
    rule #dasmInstruction ( ORIGIN (),       R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.origin      ( .Operands ) 
    rule #dasmInstruction ( CALLER (),       R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.caller      ( .Operands ) 
    rule #dasmInstruction ( CODESIZE (),     R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.codesize    ( .Operands ) 
    rule #dasmInstruction ( BENEFICIARY (),  R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.beneficiary ( .Operands ) 
    rule #dasmInstruction ( TIMESTAMP (),    R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.timestamp   ( .Operands ) 
    rule #dasmInstruction ( NUMBER (),       R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.number      ( .Operands ) 
    rule #dasmInstruction ( DIFFICULTY (),   R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.difficulty  ( .Operands ) 
    rule #dasmInstruction ( MSIZE (),        R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.msize       ( .Operands ) 

    rule #dasmInstruction ( MOVE (),        R, W, M, _, _ ) => %(R, W, M, 0) = %(R, W, M, 1)
    rule #dasmInstruction ( ISZERO (),      R, W, M, _, _ ) => %(R, W, M, 0) = iszero %(R, W, M, 1)
    rule #dasmInstruction ( NOT (),         R, W, M, _, _ ) => %(R, W, M, 0) = not    %(R, W, M, 1)
    rule #dasmInstruction ( SHA3 (),        R, W, M, _, _ ) => %(R, W, M, 0) = sha3   %(R, W, M, 1)
    rule #dasmInstruction ( MLOAD (),       R, W, M, _, _ ) => %(R, W, M, 0) = load   %(R, W, M, 1)
    rule #dasmInstruction ( SLOAD (),       R, W, M, _, _ ) => %(R, W, M, 0) = sload  %(R, W, M, 1)
    rule #dasmInstruction ( LOGARITHM2 (),   R, W, M, _, _ ) => %(R, W, M, 0) = log2   %(R, W, M, 1)
    rule #dasmInstruction ( MSTORE (),      R, W, M, _, _ ) => store  %(R, W, M, 0) , %(R, W, M, 1)
    rule #dasmInstruction ( SSTORE (),      R, W, M, _, _ ) => sstore %(R, W, M, 0) , %(R, W, M, 1)
    rule #dasmInstruction ( LOG1 (),        R, W, M, _, _ ) => log    %(R, W, M, 0) , %(R, W, M, 1)
    rule #dasmInstruction ( EXTCODESIZE (), R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.extcodesize ( %(R, W, M, 1) )
    rule #dasmInstruction ( BLOCKHASH (),   R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.blockhash   ( %(R, W, M, 1) )
    rule #dasmInstruction ( BALANCE (),     R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.balance     ( %(R, W, M, 1) ) 

    rule #dasmInstruction ( ADD (),        R, W, M, _, _ ) => %(R, W, M, 0) = add    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( MUL (),        R, W, M, _, _ ) => %(R, W, M, 0) = mul    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( SUB (),        R, W, M, _, _ ) => %(R, W, M, 0) = sub    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( DIV (),        R, W, M, _, _ ) => %(R, W, M, 0) = div    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( MOD (),        R, W, M, _, _ ) => %(R, W, M, 0) = mod    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( EXP (),        R, W, M, _, _ ) => %(R, W, M, 0) = exp    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( SIGNEXTEND (), R, W, M, _, _ ) => %(R, W, M, 0) = sext   %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( TWOS (),       R, W, M, _, _ ) => %(R, W, M, 0) = twos   %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( BSWAP (),      R, W, M, _, _ ) => %(R, W, M, 0) = bswap  %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( AND (),        R, W, M, _, _ ) => %(R, W, M, 0) = and    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( OR (),         R, W, M, _, _ ) => %(R, W, M, 0) = or     %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( XOR (),        R, W, M, _, _ ) => %(R, W, M, 0) = xor    %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( SHIFT (),      R, W, M, _, _ ) => %(R, W, M, 0) = shift  %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( BYTE (),       R, W, M, _, _ ) => %(R, W, M, 0) = byte   %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( LT (),         R, W, M, _, _ ) => %(R, W, M, 0) = cmp lt %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( LE (),         R, W, M, _, _ ) => %(R, W, M, 0) = cmp le %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( GT (),         R, W, M, _, _ ) => %(R, W, M, 0) = cmp gt %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( GE (),         R, W, M, _, _ ) => %(R, W, M, 0) = cmp ge %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( EQ (),         R, W, M, _, _ ) => %(R, W, M, 0) = cmp eq %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( NE (),         R, W, M, _, _ ) => %(R, W, M, 0) = cmp ne %(R, W, M, 1) , %(R, W, M, 2)
    rule #dasmInstruction ( LOG2 (),       R, W, M, _, _ ) => log %(R, W, M, 0) , %(R, W, M, 1) , %(R, W, M, 2)

    rule #dasmInstruction ( ADDMOD (),  R, W, M, _, _ ) => %(R, W, M, 0) = addmod %(R, W, M, 1) , %(R, W, M, 2) , %(R, W, M, 3)
    rule #dasmInstruction ( MULMOD (),  R, W, M, _, _ ) => %(R, W, M, 0) = mulmod %(R, W, M, 1) , %(R, W, M, 2) , %(R, W, M, 3)
    rule #dasmInstruction ( EXPMOD (),  R, W, M, _, _ ) => %(R, W, M, 0) = expmod %(R, W, M, 1) , %(R, W, M, 2) , %(R, W, M, 3)
    rule #dasmInstruction ( MLOADN (),  R, W, M, _, _ ) => %(R, W, M, 0) = load   %(R, W, M, 1) , %(R, W, M, 2) , %(R, W, M, 3)
    rule #dasmInstruction ( MSTOREN (), R, W, M, _, _ ) => store %(R, W, M, 0) , %(R, W, M, 1) , %(R, W, M, 2) , %(R, W, M, 3)
    rule #dasmInstruction ( LOG3 (),    R, W, M, _, _ ) => log %(R, W, M, 0) , %(R, W, M, 1) , %(R, W, M, 2) , %(R, W, M, 3)

    rule #dasmInstruction ( LOG4 (),    R, W, M, _, _ ) => log %(R, W, M, 0) , %(R, W, M, 1) , %(R, W, M, 2) , %(R, W, M, 3) , %(R, W, M, 4)

    rule #dasmInstruction ( STATICCALL (LABEL, ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS +Int 1) = staticcall @ getIeleName(F [ LABEL ]) at %(R, W, M, 2 +Int RETS) ( %o(R, W, M, 3 +Int RETS, ARGS) ) gaslimit %(R, W, M, 1 +Int RETS)
    rule #dasmInstruction ( STATICCALLDYN (ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS +Int 1) = staticcall %(R, W, M, 1 +Int RETS) at %(R, W, M, 3 +Int RETS) ( %o(R, W, M, 4 +Int RETS, ARGS) ) gaslimit %(R, W, M, 2 +Int RETS)
    rule #dasmInstruction ( CALL (LABEL, ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS +Int 1) = call @ getIeleName(F [ LABEL ]) at %(R, W, M, 2 +Int RETS) ( %o(R, W, M, 4 +Int RETS, ARGS) ) send %(R, W, M, 3 +Int RETS) , gaslimit %(R, W, M, 1 +Int RETS)
    rule #dasmInstruction ( CALLDYN (ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS +Int 1) = call %(R, W, M, 1 +Int RETS) at %(R, W, M, 3 +Int RETS) ( %o(R, W, M, 5 +Int RETS, ARGS) ) send %(R, W, M, 4 +Int RETS) , gaslimit %(R, W, M, 2 +Int RETS)
    rule #dasmInstruction ( LOCALCALL (LABEL, ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS) = call @ getIeleName(F [ LABEL ] orDefault LABEL) ( %o(R, W, M, RETS, ARGS) )
    rule #dasmInstruction ( LOCALCALLDYN (ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS) = call %(R, W, M, RETS) ( %o(R, W, M, 1 +Int RETS, ARGS) )
    rule #dasmInstruction ( CALLADDRESS (LABEL), R, W, M, F, _ ) => %(R, W, M, 0) = calladdress @ getIeleName(F [ LABEL ]) at %(R, W, M, 1)

    rule #dasmInstruction ( CREATE (LABEL, ARGS), R, W, M, _, NAME ) => %(R, W, M, 0) , %(R, W, M, 1) = create NAME +.+IeleName String2IeleName(Int2String(LABEL)) ( %o(R, W, M, 3, ARGS) ) send %(R, W, M, 2)
    rule #dasmInstruction ( COPYCREATE (ARGS), R, W, M, _, _ ) => %(R, W, M, 0) , %(R, W, M, 1) = copycreate %(R, W, M, 3) ( %o(R, W, M, 4, ARGS) ) send %(R, W, M, 2)

    rule #dasmInstruction ( REVERT(), R, W, M, _, _ ) => revert %(R, W, M, 0)
    rule #dasmInstruction ( RETURN(RETS), R, W, M, _, _ ) => ret %o(R, W, M, 0, RETS)
      requires RETS =/=Int 0
    rule #dasmInstruction ( RETURN(0), R, W, M, _, _ ) => ret void

    syntax LValue ::= "%" "(" Int "," Int "," Int "," Int ")" [function]
 // --------------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX) => % ((REGS >>Int (IDX *Int WIDTH)) &Int MASK)

    syntax NonEmptyOperands ::= "%o" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // -------------------------------------------------------------------------------
    rule %o(REGS, WIDTH, MASK, IDX, 0) => .NonEmptyOperands
    rule %o(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX) , %o(REGS, WIDTH, MASK, IDX +Int 1, COUNT -Int 1) [owise]

    syntax LValues ::= "%l" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // ------------------------------------------------------------------------------
    rule %l(REGS, WIDTH, MASK, IDX, 0) => .LValues
    rule %l(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX) , %l(REGS, WIDTH, MASK, IDX +Int 1, COUNT -Int 1) [owise]

    syntax Int ::= #opWidth ( OpCode , Int ) [function]
 // ---------------------------------------------------
    rule #opWidth ( OP, NBITS ) => #opCodeWidth(OP) +Int ((NBITS *Int #numArgs(OP)) up/Int 8) [owise]

    syntax Int ::= #opCodeWidth ( OpCode ) [function]
 // -------------------------------------------------
    rule #opCodeWidth( BRLABEL(_) )         => 3
    rule #opCodeWidth( BR(_) )              => 3
    rule #opCodeWidth( BRC(_) )             => 3
    rule #opCodeWidth( LOCALCALL(_,_,_) )   => 7
    rule #opCodeWidth( LOCALCALLDYN(_,_) )  => 5
    rule #opCodeWidth( RETURN(_) )          => 3
    rule #opCodeWidth( CALL(_,_,_) )        => 7
    rule #opCodeWidth( CALLDYN(_,_) )       => 5
    rule #opCodeWidth( STATICCALL(_,_,_) )  => 7
    rule #opCodeWidth( STATICCALLDYN(_,_) ) => 5
    rule #opCodeWidth( CALLADDRESS(_) )     => 3
    rule #opCodeWidth( _:CreateOp )         => 5
    rule #opCodeWidth( _:CopyCreateOp )     => 3
    rule #opCodeWidth ( LOADPOS(N, _) )     => 1 +Int N
    rule #opCodeWidth ( LOADNEG(N, _) )     => 1 +Int N
    rule #opCodeWidth( OP )                 => 1 [owise]

    syntax Int ::= #numArgs ( OpCode ) [function]
 // ---------------------------------------------
    rule #numArgs ( _:NullOp ) => 0
    rule #numArgs ( _:UnOp )   => 1
    rule #numArgs ( _:BinOp )  => 2
    rule #numArgs ( _:TernOp ) => 3
    rule #numArgs ( _:QuadOp ) => 4
    rule #numArgs ( _:FiveOp ) => 5
    rule #numArgs ( STATICCALL   (_, ARGS, RETS) ) => 3 +Int ARGS +Int RETS
    rule #numArgs ( STATICCALLDYN(   ARGS, RETS) ) => 4 +Int ARGS +Int RETS
    rule #numArgs ( CALL         (_, ARGS, RETS) ) => 4 +Int ARGS +Int RETS
    rule #numArgs ( CALLDYN      (   ARGS, RETS) ) => 5 +Int ARGS +Int RETS
    rule #numArgs ( LOCALCALL    (_, ARGS, RETS) ) => ARGS +Int RETS
    rule #numArgs ( LOCALCALLDYN (   ARGS, RETS) ) => 1 +Int ARGS +Int RETS
    rule #numArgs ( CALLADDRESS(_) )               => 2
    rule #numArgs ( RETURN(RETS) )                 => RETS
    rule #numArgs ( CREATE(_, ARGS) )              => 3 +Int ARGS
    rule #numArgs ( COPYCREATE(ARGS) )             => 4 +Int ARGS

    syntax OpCode ::= #dasmOpCode ( Int, Int, Bytes )      [function]
                    | #dasmOpCode ( Int, Int, Int, Bytes ) [function]
 // -----------------------------------------------------------------
    rule #dasmOpCode( I, N, BS ) => #dasmOpCode( BS[I], I +Int 1, N -Int 1, BS ) requires N >=Int 1
    rule #dasmOpCode( _, _, _  ) => encodingError() [owise]

    rule #dasmOpCode(   1 , _, _,  _ ) => ADD ()
    rule #dasmOpCode(   2 , _, _,  _ ) => MUL ()
    rule #dasmOpCode(   3 , _, _,  _ ) => SUB ()
    rule #dasmOpCode(   4 , _, _,  _ ) => DIV ()
    rule #dasmOpCode(   6 , _, _,  _ ) => MOD ()
    rule #dasmOpCode(   7 , _, _,  _ ) => EXP ()
    rule #dasmOpCode(   8 , _, _,  _ ) => ADDMOD ()
    rule #dasmOpCode(   9 , _, _,  _ ) => MULMOD ()
    rule #dasmOpCode(  10 , _, _,  _ ) => EXPMOD ()
    rule #dasmOpCode(  11 , _, _,  _ ) => SIGNEXTEND ()
    rule #dasmOpCode(  12 , _, _,  _ ) => TWOS ()
    rule #dasmOpCode(  13 , _, _,  _ ) => BSWAP ()
    rule #dasmOpCode(  15 , _, _,  _ ) => NE ()
    rule #dasmOpCode(  16 , _, _,  _ ) => LT ()
    rule #dasmOpCode(  17 , _, _,  _ ) => GT ()
    rule #dasmOpCode(  18 , _, _,  _ ) => LE ()
    rule #dasmOpCode(  19 , _, _,  _ ) => GE ()
    rule #dasmOpCode(  20 , _, _,  _ ) => EQ ()
    rule #dasmOpCode(  21 , _, _,  _ ) => ISZERO ()
    rule #dasmOpCode(  22 , _, _,  _ ) => AND ()
    rule #dasmOpCode(  23 , _, _,  _ ) => OR ()
    rule #dasmOpCode(  24 , _, _,  _ ) => XOR ()
    rule #dasmOpCode(  25 , _, _,  _ ) => NOT ()
    rule #dasmOpCode(  26 , _, _,  _ ) => BYTE ()
    rule #dasmOpCode(  27 , _, _,  _ ) => SHIFT ()
    rule #dasmOpCode(  28 , _, _,  _ ) => LOGARITHM2 ()
    rule #dasmOpCode(  32 , _, _,  _ ) => SHA3 ()
    rule #dasmOpCode(  48 , _, _,  _ ) => ADDRESS ()
    rule #dasmOpCode(  49 , _, _,  _ ) => BALANCE ()
    rule #dasmOpCode(  50 , _, _,  _ ) => ORIGIN ()
    rule #dasmOpCode(  51 , _, _,  _ ) => CALLER ()
    rule #dasmOpCode(  52 , _, _,  _ ) => CALLVALUE ()
    rule #dasmOpCode(  56 , _, _,  _ ) => CODESIZE ()
    rule #dasmOpCode(  58 , _, _,  _ ) => GASPRICE ()
    rule #dasmOpCode(  59 , _, _,  _ ) => EXTCODESIZE ()
    rule #dasmOpCode(  64 , _, _,  _ ) => BLOCKHASH ()
    rule #dasmOpCode(  65 , _, _,  _ ) => BENEFICIARY ()
    rule #dasmOpCode(  66 , _, _,  _ ) => TIMESTAMP ()
    rule #dasmOpCode(  67 , _, _,  _ ) => NUMBER ()
    rule #dasmOpCode(  68 , _, _,  _ ) => DIFFICULTY ()
    rule #dasmOpCode(  69 , _, _,  _ ) => GASLIMIT ()
    rule #dasmOpCode(  80 , _, _,  _ ) => MLOADN ()
    rule #dasmOpCode(  81 , _, _,  _ ) => MLOAD ()
    rule #dasmOpCode(  82 , _, _,  _ ) => MSTOREN ()
    rule #dasmOpCode(  83 , _, _,  _ ) => MSTORE ()
    rule #dasmOpCode(  84 , _, _,  _ ) => SLOAD ()
    rule #dasmOpCode(  85 , _, _,  _ ) => SSTORE ()
    rule #dasmOpCode(  86 , _, _,  _ ) => MSIZE ()
    rule #dasmOpCode(  87 , _, _,  _ ) => GAS ()
    rule #dasmOpCode(  96 , _, _,  _ ) => MOVE ()
    rule #dasmOpCode(  97 , I, N, BS ) => #dasmLoad(97, #loadLen(BS[I .. N]), #loadOffset(BS[I .. N]), I, N, BS)
    rule #dasmOpCode(  98 , I, N, BS ) => #dasmLoad(98, #loadLen(BS[I .. N]), #loadOffset(BS[I .. N]), I, N, BS)
    rule #dasmOpCode( 100 , I, N, BS ) => BR     ( #asUnsigned(I, 2, BS) ) requires N >=Int 2
    rule #dasmOpCode( 101 , I, N, BS ) => BRC    ( #asUnsigned(I, 2, BS) ) requires N >=Int 2
    rule #dasmOpCode( 102 , I, N, BS ) => BRLABEL( #asUnsigned(I, 2, BS) ) requires N >=Int 2
    rule #dasmOpCode( 160 , _, _,  _ ) => LOG0 ()
    rule #dasmOpCode( 161 , _, _,  _ ) => LOG1 ()
    rule #dasmOpCode( 162 , _, _,  _ ) => LOG2 ()
    rule #dasmOpCode( 163 , _, _,  _ ) => LOG3 ()
    rule #dasmOpCode( 164 , _, _,  _ ) => LOG4 ()
    rule #dasmOpCode( 240 , I, N, BS ) => CREATE        ( #asUnsigned(I, 2, BS), #asUnsigned(I +Int 2, 2, BS)                               ) requires N >=Int 4
    rule #dasmOpCode( 241 , I, N, BS ) => COPYCREATE    ( #asUnsigned(I, 2, BS)                                                             ) requires N >=Int 2
    rule #dasmOpCode( 242 , I, N, BS ) => CALL          ( #asUnsigned(I, 2, BS), #asUnsigned(I +Int 2, 2, BS), #asUnsigned(I +Int 4, 2, BS) ) requires N >=Int 6
    rule #dasmOpCode( 243 , I, N, BS ) => CALLDYN       ( #asUnsigned(I, 2, BS), #asUnsigned(I +Int 2, 2, BS)                               ) requires N >=Int 4
    rule #dasmOpCode( 244 , I, N, BS ) => STATICCALL    ( #asUnsigned(I, 2, BS), #asUnsigned(I +Int 2, 2, BS), #asUnsigned(I +Int 4, 2, BS) ) requires N >=Int 6
    rule #dasmOpCode( 245 , I, N, BS ) => STATICCALLDYN ( #asUnsigned(I, 2, BS), #asUnsigned(I +Int 2, 2, BS)                               ) requires N >=Int 4
    rule #dasmOpCode( 246 , I, N, BS ) => RETURN        ( #asUnsigned(I, 2, BS)                                                             ) requires N >=Int 2
    rule #dasmOpCode( 247 , _, _,  _ ) => REVERT()
    rule #dasmOpCode( 248 , I, N, BS ) => LOCALCALL    ( #asUnsigned(I, 2, BS), #asUnsigned(I +Int 2, 2, BS), #asUnsigned(I +Int 4, 2, BS)  ) requires N >=Int 6
    rule #dasmOpCode( 249 , I, N, BS ) => LOCALCALLDYN ( #asUnsigned(I, 2, BS), #asUnsigned(I +Int 2, 2, BS)                                ) requires N >=Int 4
    rule #dasmOpCode( 250 , I, N, BS ) => CALLADDRESS  ( #asUnsigned(I, 2, BS)                                                              ) requires N >=Int 2
    rule #dasmOpCode( 254 , _, _,  _ ) => INVALID ()
    rule #dasmOpCode( 255 , _, _,  _ ) => SELFDESTRUCT ()
    rule #dasmOpCode(   _ , _, _,  _ ) => encodingError() [owise]

    syntax OpCode ::= #dasmLoad ( Int , Int , Int , Int , Int , Bytes ) [function]
 // ------------------------------------------------------------------------------
    rule #dasmLoad(97, LEN, POS, I, N, BS) => LOADPOS(LEN +Int POS, #asUnsigned(I +Int POS, LEN , BS))
    rule #dasmLoad(98, LEN, POS, I, N, BS) => LOADNEG(LEN +Int POS, #asUnsigned(I +Int POS, LEN , BS))

endmodule
```

