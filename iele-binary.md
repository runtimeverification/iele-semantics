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
                      | #dasmContract ( Int , Int , Bytes , IeleName , Bytes )                                              [function]
                      | #dasmContract ( Int , Int , Bytes , Int , Map, IeleName , TopLevelDefinitions, Int , Int , String ) [function]
 // ----------------------------------------------------------------------------------------------------------------------------------
    rule #dasmContract( BS, NAME ) => #dasmContract( 0, lengthBytes(BS), BS, NAME )
    rule #dasmContract( _, 0, BS, NAME ) => #emptyCode
    rule #dasmContract( I, N, BS, NAME ) => #dasmContract(I +Int 4, #asUnsigned(BS[I .. 4]), BS, NAME, BS[I .. 4])
      requires N >=Int 5 andBool BS[I +Int 4] ==Int 99

    rule #dasmContract( I, N, BS, NAME, BS2 ) => #dasmContract(I +Int 2, N -Int 2, BS, BS[I +Int 1], 0 |-> init, NAME, .TopLevelDefinitions, 1, N +Int 4 , #unparseByteStack(BS2 +Bytes (BS[I .. N])))
      requires N >=Int 2 andBool BS[I] ==Int 99 andBool lengthBytes(BS2) ==Int 4

    rule #dasmContract( I, N, BS, NBITS, FUNCS, NAME, DEFS, M, SIZE, BYTES )
      => #dasmContract( I +Int 3 +Int #asUnsigned(BS[I +Int 1 .. 2]), N -Int (3 +Int #asUnsigned(BS[I +Int 1 .. 2])), BS, NBITS, M |-> String2IeleName(#unparseByteStack(BS[I +Int 3 .. #asUnsigned(BS[I +Int 1 .. 2])])) FUNCS, NAME, DEFS, M +Int 1, SIZE, BYTES )
      requires N >=Int 3 andBool BS[I] ==Int 105

    rule #dasmContract( I, N, BS, NBITS, FUNCS, NAME, DEFS, M, SIZE, BYTES )
      => #dasmContract( I +Int 3, #asUnsigned(BS[I +Int 1 .. 2]), BS, NAME +.+IeleName M) ++Contract #dasmContract(I +Int (3 +Int #asUnsigned(BS[I +Int 1 .. 2])), N -Int (3 +Int #asUnsigned(BS[I +Int 1 .. 2])), BS, NBITS, FUNCS, NAME, external contract NAME +.+IeleName M DEFS, M +Int 1, SIZE, BYTES)
      requires N >=Int 3 andBool BS[I] ==Int 106

    rule #dasmContract( I, N, BS, NBITS, FUNCS, NAME, DEFS, M, SIZE, BYTES ) => contract NAME ! SIZE BYTES { DEFS ++TopLevelDefinitions #dasmFunctions(BS[I .. N], NBITS, FUNCS, NAME) } .Contract [owise]

    syntax Bool ::= #isValidContract(Bytes)                   [function]
                  | #isValidContract(Int, Int, Bytes)         [function, klabel(isValidContractAux)]
                  | #isValidStringTable(Int, Int, Bytes, Int) [function]
 // --------------------------------------------------------------------
    rule #isValidContract(CODE) => #isValidContract(0, lengthBytes(CODE), CODE)
    rule #isValidContract(_, 0, BS) => true
    rule #isValidContract(I, N, BS) => N -Int 4 >=Int #asUnsigned(BS[I .. 4]) andBool #isValidStringTable(I +Int 6, #asUnsigned(BS[I .. 4]) -Int 2, BS, BS[I +Int 5])
      requires N >=Int 6 andBool BS[I +Int 4] ==Int 99
    rule #isValidContract(_, _, _) => false [owise]

    rule #isValidStringTable(I, N, BS, NBITS) => N -Int 3 >=Int #asUnsigned(BS[I +Int 1 .. 2]) andBool #isValidStringTable(I +Int (3 +Int #asUnsigned(BS[I +Int 1 .. 2])), N -Int (3 +Int #asUnsigned(BS[I +Int 1 .. 2])), BS, NBITS)
      requires N >=Int 3 andBool BS[I] ==Int 105

    rule #isValidStringTable(I, N, BS, NBITS) => N -Int 3 >=Int #asUnsigned(BS[I +Int 1 .. 2]) andBool #isValidContract(I +Int 3, #asUnsigned(BS[I +Int 1 .. 2]), BS) andBool #isValidStringTable(I +Int (3 +Int #asUnsigned(BS[I +Int 1 .. 2])), N -Int (3 +Int #asUnsigned(BS[I +Int 1 .. 2])), BS, NBITS)
      requires N >=Int 3 andBool BS[I] ==Int 106 andBool #asUnsigned(BS[I +Int 1 .. 2]) =/=Int 0

    rule #isValidStringTable(I, N, BS, _) => false
      requires N >=Int 3 andBool BS[I] ==Int 106 andBool #asUnsigned(BS[I +Int 1 .. 2]) ==Int 0
    rule #isValidStringTable(I, N, BS, NBITS) => #isValidFunctions(BS[I .. N], NBITS, N) [owise]

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

    syntax TopLevelDefinitions ::= #dasmFunctions ( Bytes , Int , Map , IeleName ) [function]
    syntax TopLevelDefinitions ::= #dasmFunction ( Bool , IeleName , IeleName , Int , Bytes , Int , Map , Instructions , K ) [function]
 // -----------------------------------------------------------------------------------------------------------------------------------
    rule #dasmFunctions(BS, NBITS, FUNCS, NAME) => #dasmFunction(false, getIeleName(FUNCS [ #asUnsigned(BS[1 .. 2]) ] orDefault #asUnsigned(BS[1 .. 2])), NAME, #asUnsigned(BS[3 .. 2]), #drop(5, BS), NBITS, FUNCS, .Instructions, .K)
      requires lengthBytes(BS) >=Int 5 andBool BS[0] ==Int 103
    rule #dasmFunctions(BS, NBITS, FUNCS, NAME) => #dasmFunction(true , getIeleName(FUNCS [ #asUnsigned(BS[1 .. 2]) ] orDefault #asUnsigned(BS[1 .. 2])), NAME, #asUnsigned(BS[3 .. 2]), #drop(5, BS), NBITS, FUNCS, .Instructions, .K)
      requires lengthBytes(BS) >=Int 5 andBool BS[0] ==Int 104
    rule #dasmFunctions(BS, _, _, _) => .TopLevelDefinitions requires lengthBytes(BS) ==Int 0

    syntax Bool ::= #isValidFunctions(Bytes, Int, Int)           [function]
                  | #isValidFunction(Bytes, Int, Int)            [function]
                  | #isValidInstruction(OpCode, Bytes, Int, Int) [function]
                  | #isValidLoad(Bytes, Int)                     [function]
 // -----------------------------------------------------------------------
    rule #isValidFunctions(BS, NBITS, SIZE) => #isValidFunction(#drop(5, BS), NBITS, SIZE -Int 5)
      requires lengthBytes(BS) >=Int 5 andBool (BS[0] ==Int 103 orBool BS[0] ==Int 104)
    rule #isValidFunctions(BS, _, 0) => true requires lengthBytes(BS) ==Int 0
    rule #isValidFunctions(_, _, _) => false [owise]

    rule #isValidFunction(BS, NBITS, SIZE) => #isValidFunctions(BS, NBITS, SIZE)
      requires lengthBytes(BS) >=Int 1 andBool (BS[0] ==Int 103 orBool BS[0] ==Int 104)
    rule #isValidFunction(BS, _, 0) => true requires lengthBytes(BS) ==Int 0
    rule #isValidFunction(BS, NBITS, SIZE) => #isValidLoad(#drop(1, BS), SIZE -Int 1) andBool #isValidInstruction(#dasmOpCode(BS), BS, NBITS, SIZE)
      requires lengthBytes(BS) >=Int 1 andBool (BS[0] ==Int 97 orBool BS[0] ==Int 98)
    rule #isValidFunction(BS, NBITS, SIZE) => #isValidInstruction(#dasmOpCode(BS), BS, NBITS, SIZE) [owise]

    rule #isValidInstruction(encodingError(), _, _, _) => false
    rule #isValidInstruction(OP:OpCode, WS, NBITS, SIZE) => SIZE >=Int #opWidth(OP, NBITS) andBool #isValidFunction(#drop(#opWidth(OP, NBITS), WS), NBITS, SIZE -Int #opWidth(OP, NBITS)) [owise]

    rule #isValidLoad(BS, SIZE) => SIZE >=Int #loadLen(BS) +Int #loadOffset(BS)

    rule #dasmFunction(false, NAME, CNAME, SIG, BS, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(BS, NBITS, FUNCS, CNAME)
      requires lengthBytes(BS) >=Int 1 andBool (BS[0] ==Int 103 orBool BS[0] ==Int 104)
    rule #dasmFunction(true, NAME, CNAME, SIG, BS, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(BS, NBITS, FUNCS, CNAME)
      requires lengthBytes(BS) >=Int 1 andBool (BS[0] ==Int 103 orBool BS[0] ==Int 104)

    rule #dasmFunction(false, NAME, CNAME, SIG, BS, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions
      requires lengthBytes(BS) ==Int 0
    rule #dasmFunction(true, NAME, CNAME, SIG, BS, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions
      requires lengthBytes(BS) ==Int 0

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, BS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, BS, NBITS, FUNCS, INSTRS, #dasmOpCode(BS)) [owise]

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, BS, NBITS, FUNCS, INSTRS, OP:OpCode) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, #drop(#opWidth(OP, NBITS), BS), NBITS, FUNCS, #dasmInstruction(OP, #take(#opWidth(OP, NBITS) -Int #opCodeWidth(OP), #drop(#opCodeWidth(OP), BS)), NBITS, FUNCS, CNAME) INSTRS, .K)

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
    syntax Instruction ::= #dasmInstruction ( OpCode , Bytes , Int , Map , IeleName ) [function]
                         | #dasmInstruction ( OpCode , Int , Int , Int , Map , IeleName ) [function, klabel(#dasmInstructionAux)]
 // -----------------------------------------------------------------------------------------------------------------------------
    rule #dasmInstruction ( LOADPOS(N, W), BS, NBITS, FUNCS, NAME ) => #dasmInstruction(LOADPOS(N, W), #asUnsigned(#take(NBITS up/Int 8, BS)),                            NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME)
    rule #dasmInstruction ( LOADNEG(N, W), BS, NBITS, FUNCS, NAME ) => #dasmInstruction(LOADNEG(N, W), #asUnsigned(#take(NBITS up/Int 8, BS)),                            NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME)
    rule #dasmInstruction ( OP,            BS, NBITS, FUNCS, NAME ) => #dasmInstruction(OP,            #asUnsigned(#take(#opWidth(OP, NBITS) -Int #opCodeWidth(OP), BS)), NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME) [owise]

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

    syntax OpCode ::= #dasmOpCode ( Bytes )      [function]
                    | #dasmOpCode ( Int, Bytes ) [function]
 // -------------------------------------------------------
    rule #dasmOpCode( BS ) => #dasmOpCode( BS[0], #drop(1, BS) ) requires lengthBytes(BS) >=Int 1
    rule #dasmOpCode( _  ) => encodingError() [owise]
    rule #dasmOpCode( _, _  ) => encodingError() [owise]

    rule #dasmOpCode(   1 ,  _ ) => ADD ()
    rule #dasmOpCode(   2 ,  _ ) => MUL ()
    rule #dasmOpCode(   3 ,  _ ) => SUB ()
    rule #dasmOpCode(   4 ,  _ ) => DIV ()
    rule #dasmOpCode(   6 ,  _ ) => MOD ()
    rule #dasmOpCode(   7 ,  _ ) => EXP ()
    rule #dasmOpCode(   8 ,  _ ) => ADDMOD ()
    rule #dasmOpCode(   9 ,  _ ) => MULMOD ()
    rule #dasmOpCode(  10 ,  _ ) => EXPMOD ()
    rule #dasmOpCode(  11 ,  _ ) => SIGNEXTEND ()
    rule #dasmOpCode(  12 ,  _ ) => TWOS ()
    rule #dasmOpCode(  13 ,  _ ) => BSWAP ()
    rule #dasmOpCode(  15 ,  _ ) => NE ()
    rule #dasmOpCode(  16 ,  _ ) => LT ()
    rule #dasmOpCode(  17 ,  _ ) => GT ()
    rule #dasmOpCode(  18 ,  _ ) => LE ()
    rule #dasmOpCode(  19 ,  _ ) => GE ()
    rule #dasmOpCode(  20 ,  _ ) => EQ ()
    rule #dasmOpCode(  21 ,  _ ) => ISZERO ()
    rule #dasmOpCode(  22 ,  _ ) => AND ()
    rule #dasmOpCode(  23 ,  _ ) => OR ()
    rule #dasmOpCode(  24 ,  _ ) => XOR ()
    rule #dasmOpCode(  25 ,  _ ) => NOT ()
    rule #dasmOpCode(  26 ,  _ ) => BYTE ()
    rule #dasmOpCode(  27 ,  _ ) => SHIFT ()
    rule #dasmOpCode(  28 ,  _ ) => LOGARITHM2 ()
    rule #dasmOpCode(  32 ,  _ ) => SHA3 ()
    rule #dasmOpCode(  48 ,  _ ) => ADDRESS ()
    rule #dasmOpCode(  49 ,  _ ) => BALANCE ()
    rule #dasmOpCode(  50 ,  _ ) => ORIGIN ()
    rule #dasmOpCode(  51 ,  _ ) => CALLER ()
    rule #dasmOpCode(  52 ,  _ ) => CALLVALUE ()
    rule #dasmOpCode(  56 ,  _ ) => CODESIZE ()
    rule #dasmOpCode(  58 ,  _ ) => GASPRICE ()
    rule #dasmOpCode(  59 ,  _ ) => EXTCODESIZE ()
    rule #dasmOpCode(  64 ,  _ ) => BLOCKHASH ()
    rule #dasmOpCode(  65 ,  _ ) => BENEFICIARY ()
    rule #dasmOpCode(  66 ,  _ ) => TIMESTAMP ()
    rule #dasmOpCode(  67 ,  _ ) => NUMBER ()
    rule #dasmOpCode(  68 ,  _ ) => DIFFICULTY ()
    rule #dasmOpCode(  69 ,  _ ) => GASLIMIT ()
    rule #dasmOpCode(  80 ,  _ ) => MLOADN ()
    rule #dasmOpCode(  81 ,  _ ) => MLOAD ()
    rule #dasmOpCode(  82 ,  _ ) => MSTOREN ()
    rule #dasmOpCode(  83 ,  _ ) => MSTORE ()
    rule #dasmOpCode(  84 ,  _ ) => SLOAD ()
    rule #dasmOpCode(  85 ,  _ ) => SSTORE ()
    rule #dasmOpCode(  86 ,  _ ) => MSIZE ()
    rule #dasmOpCode(  87 ,  _ ) => GAS ()
    rule #dasmOpCode(  96 ,  _ ) => MOVE ()
    rule #dasmOpCode(  97 , BS ) => #dasmLoad(97, #loadLen(BS), #loadOffset(BS), BS)
    rule #dasmOpCode(  98 , BS ) => #dasmLoad(98, #loadLen(BS), #loadOffset(BS), BS)
    rule #dasmOpCode( 100 , BS ) => BR     ( #asUnsigned(BS[0 .. 2]) ) requires lengthBytes(BS) >=Int 2
    rule #dasmOpCode( 101 , BS ) => BRC    ( #asUnsigned(BS[0 .. 2]) ) requires lengthBytes(BS) >=Int 2
    rule #dasmOpCode( 102 , BS ) => BRLABEL( #asUnsigned(BS[0 .. 2]) ) requires lengthBytes(BS) >=Int 2
    rule #dasmOpCode( 160 ,  _ ) => LOG0 ()
    rule #dasmOpCode( 161 ,  _ ) => LOG1 ()
    rule #dasmOpCode( 162 ,  _ ) => LOG2 ()
    rule #dasmOpCode( 163 ,  _ ) => LOG3 ()
    rule #dasmOpCode( 164 ,  _ ) => LOG4 ()
    rule #dasmOpCode( 240 , BS ) => CREATE        ( #asUnsigned(BS[0 .. 2]), #asUnsigned(BS[2 .. 2])                          ) requires lengthBytes(BS) >=Int 4
    rule #dasmOpCode( 241 , BS ) => COPYCREATE    ( #asUnsigned(BS[0 .. 2])                                                   ) requires lengthBytes(BS) >=Int 2
    rule #dasmOpCode( 242 , BS ) => CALL          ( #asUnsigned(BS[0 .. 2]), #asUnsigned(BS[2 .. 2]), #asUnsigned(BS[4 .. 2]) ) requires lengthBytes(BS) >=Int 6
    rule #dasmOpCode( 243 , BS ) => CALLDYN       ( #asUnsigned(BS[0 .. 2]), #asUnsigned(BS[2 .. 2])                          ) requires lengthBytes(BS) >=Int 4
    rule #dasmOpCode( 244 , BS ) => STATICCALL    ( #asUnsigned(BS[0 .. 2]), #asUnsigned(BS[2 .. 2]), #asUnsigned(BS[4 .. 2]) ) requires lengthBytes(BS) >=Int 6
    rule #dasmOpCode( 245 , BS ) => STATICCALLDYN ( #asUnsigned(BS[0 .. 2]), #asUnsigned(BS[2 .. 2])                          ) requires lengthBytes(BS) >=Int 4
    rule #dasmOpCode( 246 , BS ) => RETURN        ( #asUnsigned(BS[0 .. 2])                                                   ) requires lengthBytes(BS) >=Int 2
    rule #dasmOpCode( 247 ,  _ ) => REVERT()
    rule #dasmOpCode( 248 , BS ) => LOCALCALL    ( #asUnsigned(BS[0 .. 2]), #asUnsigned(BS[2 .. 2]), #asUnsigned(BS[4 .. 2]) ) requires lengthBytes(BS) >=Int 6
    rule #dasmOpCode( 249 , BS ) => LOCALCALLDYN ( #asUnsigned(BS[0 .. 2]), #asUnsigned(BS[2 .. 2])                          ) requires lengthBytes(BS) >=Int 4
    rule #dasmOpCode( 250 , BS ) => CALLADDRESS  ( #asUnsigned(BS[0 .. 2])                                                   ) requires lengthBytes(BS) >=Int 2
    rule #dasmOpCode( 254 ,  _ ) => INVALID ()
    rule #dasmOpCode( 255 ,  _ ) => SELFDESTRUCT ()

    syntax OpCode ::= #dasmLoad ( Int , Int , Int , Bytes ) [function]
 // ----------------------------------------------------------------------
    rule #dasmLoad(97, LEN, POS, BS) => LOADPOS(LEN +Int POS, #asUnsigned(BS [ POS .. LEN ]))
    rule #dasmLoad(98, LEN, POS, BS) => LOADNEG(LEN +Int POS, #asUnsigned(BS [ POS .. LEN ]))
```

```{.k .wordstack}

    syntax Contract ::= #dasmContract ( WordStack , IeleName )       [function]
                      | #dasmContract ( WordStack , IeleName , WordStack ) [function, klabel(#dasmContractAux1)]
                      | #dasmContract ( WordStack , Int , Map, IeleName , TopLevelDefinitions, Int , Int , String ) [function, klabel(#dasmContractAux2)]
 // ----------------------------------------------------------------------------------------------------------------------------------------------------
    rule #dasmContract( .WordStack, _) => #emptyCode
    rule #dasmContract( W1 : W2 : W3 : W4 : 99 : WS, NAME) => #dasmContract(99 : #take( W1 *Int 16777216  +Int W2 *Int 65536 +Int W3 *Int 256 +Int W4 -Int 1, WS), NAME, W1 : W2 : W3 : W4 : .WordStack)

    rule #dasmContract( 99 : NBITS : WS, NAME, W1 : W2 : W3 : W4 : .WordStack ) => #dasmContract(WS, NBITS, 0 |-> init, NAME, .TopLevelDefinitions, 1, #sizeWordStack(WS) +Int 6 , #unparseByteStack(W1 : W2 : W3 : W4 : 99 : NBITS : WS))
    rule #dasmContract( 105 : W1 : W2 : WS, NBITS, FUNCS, NAME, DEFS, N, SIZE, BYTES ) => #dasmContract(#drop(W1 *Int 256 +Int W2, WS), NBITS, N |-> String2IeleName(#unparseByteStack(#take(W1 *Int 256 +Int W2, WS))) FUNCS, NAME, DEFS, N +Int 1, SIZE, BYTES )
    rule #dasmContract( 106 : W1 : W2 : WS, NBITS, FUNCS, NAME, DEFS, N, SIZE, BYTES ) => #dasmContract(#take(W1 *Int 256 +Int W2, WS), NAME +.+IeleName N) ++Contract #dasmContract(#drop(W1 *Int 256 +Int W2, WS), NBITS, FUNCS, NAME, external contract NAME +.+IeleName N DEFS, N +Int 1, SIZE, BYTES)
    rule #dasmContract( WS, NBITS, FUNCS, NAME, DEFS, N, SIZE, BYTES ) => contract NAME ! SIZE BYTES { DEFS ++TopLevelDefinitions #dasmFunctions(WS, NBITS, FUNCS, NAME) } .Contract [owise]

    syntax Bool ::= #isValidContract(WordStack)              [function]
                  | #isValidContract(WordStack, Int)         [function, klabel(isValidContractAux)]
                  | #isValidStringTable(WordStack, Int, Int) [function]
 // -------------------------------------------------------------------
    rule #isValidContract(CODE) => #isValidContract(CODE, #sizeWordStack(CODE))
    rule #isValidContract(.WordStack, 0) => true
    rule #isValidContract(W1 : W2 : W3 : W4 : 99 : NBITS : WS, SIZE) => #fun(DECLSIZE => SIZE -Int 4 >=Int DECLSIZE andBool #isValidStringTable(#take(DECLSIZE -Int 2, WS), NBITS, DECLSIZE -Int 2))(W1 *Int 16777216 +Int W2 *Int 65536 +Int W3 *Int 256 +Int W4)
    rule #isValidContract(_, _) => false [owise]

    rule #isValidStringTable(105 : W1 : W2 : WS, NBITS, SIZE) => SIZE -Int 3 >=Int W1 *Int 256 +Int W2 andBool #isValidStringTable(#drop(W1 *Int 256 +Int W2, WS), NBITS, SIZE -Int 3 -Int W1 *Int 256 -Int W2)
    rule #isValidStringTable(106 : W1 : W2 : WS, NBITS, SIZE) => SIZE -Int 3 >=Int W1 *Int 256 +Int W2 andBool #isValidContract(#take(W1 *Int 256 +Int W2, WS), W1 *Int 256 +Int W2) andBool #isValidStringTable(#drop(W1 *Int 256 +Int W2, WS), NBITS, SIZE -Int 3 -Int W1 *Int 256 -Int W2)
      requires W1 =/=Int 0 orBool W2 =/=Int 0
    rule #isValidStringTable(106 : 0 : 0 : WS, NBITS, SIZE) => false
    rule #isValidStringTable(WS, NBITS, SIZE) => #isValidFunctions(WS, NBITS, SIZE) [owise]

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

    syntax TopLevelDefinitions ::= #dasmFunctions ( WordStack , Int , Map , IeleName ) [function]
    syntax TopLevelDefinitions ::= #dasmFunction ( Bool , IeleName , IeleName , Int , WordStack , Int , Map , Instructions , K ) [function]
 // ----------------------------------------------------------------------------------------------------------------------------
    rule #dasmFunctions(103 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS, NAME) => #dasmFunction(false, getIeleName(FUNCS [ W1 *Int 256 +Int W2 ] orDefault W1 *Int 256 +Int W2), NAME, W3 *Int 256 +Int W4, WS, NBITS, FUNCS, .Instructions, .K)
    rule #dasmFunctions(104 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS, NAME) => #dasmFunction(true, getIeleName(FUNCS [ W1 *Int 256 +Int W2 ] orDefault W1 *Int 256 +Int W2), NAME, W3 *Int 256 +Int W4, WS, NBITS, FUNCS, .Instructions, .K)
    rule #dasmFunctions(.WordStack, NBITS, FUNCS, NAME) => .TopLevelDefinitions

    syntax Bool ::= #isValidFunctions(WordStack, Int, Int)           [function]
                  | #isValidFunction(WordStack, Int, Int)            [function]
                  | #isValidInstruction(OpCode, WordStack, Int, Int) [function]
                  | #isValidLoad(WordStack, Int)                     [function]
 // ---------------------------------------------------------------------------
    rule #isValidFunctions(103 : W1 : W2 : W3 : W4 : WS, NBITS, SIZE) => #isValidFunction(WS, NBITS, SIZE -Int 5)
    rule #isValidFunctions(104 : W1 : W2 : W3 : W4 : WS, NBITS, SIZE) => #isValidFunction(WS, NBITS, SIZE -Int 5)
    rule #isValidFunctions(.WordStack, _, 0) => true
    rule #isValidFunctions(_, _, _) => false [owise]

    rule #isValidFunction(W : WS, NBITS, SIZE) => #isValidFunctions(W : WS, NBITS, SIZE)
      requires W ==Int 103 orBool W ==Int 104
    rule #isValidFunction(.WordStack, _, 0) => true
    rule #isValidFunction(W : WS, NBITS, SIZE) => #isValidLoad(WS, SIZE -Int 1) andBool #isValidInstruction(#dasmOpCode(W : WS), W : WS, NBITS, SIZE)
      requires W ==Int 97 orBool W ==Int 98
    rule #isValidFunction(WS, NBITS, SIZE) => #isValidInstruction(#dasmOpCode(WS), WS, NBITS, SIZE) [owise]

    rule #isValidInstruction(encodingError(), _, _, _) => false
    rule #isValidInstruction(OP:OpCode, WS, NBITS, SIZE) => SIZE >=Int #opWidth(OP, NBITS) andBool #isValidFunction(#drop(#opWidth(OP, NBITS), WS), NBITS, SIZE -Int #opWidth(OP, NBITS)) [owise]

    rule #isValidLoad(WS, SIZE) => SIZE >=Int #loadLen(WS) +Int #loadOffset(WS)

    rule #dasmFunction(false, NAME, CNAME, SIG, W : WS, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(W : WS, NBITS, FUNCS, CNAME)
      requires W ==Int 103 orBool W ==Int 104
    rule #dasmFunction(true, NAME, CNAME, SIG, W : WS, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(W : WS, NBITS, FUNCS, CNAME)
      requires W ==Int 103 orBool W ==Int 104
    rule #dasmFunction(false, NAME, CNAME, SIG, .WordStack, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions
    rule #dasmFunction(true, NAME, CNAME, SIG, .WordStack, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, #dasmOpCode(WS)) [owise]

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, OP:OpCode) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, #drop(#opWidth(OP, NBITS), WS), NBITS, FUNCS, #dasmInstruction(OP, #take(#opWidth(OP, NBITS) -Int #opCodeWidth(OP), #drop(#opCodeWidth(OP), WS)), NBITS, FUNCS, CNAME) INSTRS, .K)

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
    syntax Instruction ::= #dasmInstruction ( OpCode , WordStack , Int , Map , IeleName ) [function]
                         | #dasmInstruction ( OpCode , Int , Int , Int , Map , IeleName ) [function, klabel(#dasmInstructionAux)]
 // ------------------------------------------------------------------------------------------------------------------
    rule #dasmInstruction ( LOADPOS(N, W), WS, NBITS, FUNCS, NAME ) => #dasmInstruction(LOADPOS(N, W), #asUnsigned(#take(NBITS up/Int 8, WS)),                            NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME)
    rule #dasmInstruction ( LOADNEG(N, W), WS, NBITS, FUNCS, NAME ) => #dasmInstruction(LOADNEG(N, W), #asUnsigned(#take(NBITS up/Int 8, WS)),                            NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME)
    rule #dasmInstruction ( OP,            WS, NBITS, FUNCS, NAME ) => #dasmInstruction(OP,            #asUnsigned(#take(#opWidth(OP, NBITS) -Int #opCodeWidth(OP), WS)), NBITS, (1 <<Int NBITS) -Int 1, FUNCS, NAME) [owise]

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

    syntax OpCode ::= #dasmOpCode ( WordStack ) [function]
 // ------------------------------------------------------
    rule #dasmOpCode(   1 :  _ ) => ADD ()
    rule #dasmOpCode(   2 :  _ ) => MUL ()
    rule #dasmOpCode(   3 :  _ ) => SUB ()
    rule #dasmOpCode(   4 :  _ ) => DIV ()
    rule #dasmOpCode(   6 :  _ ) => MOD ()
    rule #dasmOpCode(   7 :  _ ) => EXP ()
    rule #dasmOpCode(   8 :  _ ) => ADDMOD ()
    rule #dasmOpCode(   9 :  _ ) => MULMOD ()
    rule #dasmOpCode(  10 :  _ ) => EXPMOD ()
    rule #dasmOpCode(  11 :  _ ) => SIGNEXTEND ()
    rule #dasmOpCode(  12 :  _ ) => TWOS ()
    rule #dasmOpCode(  13 :  _ ) => BSWAP ()
    rule #dasmOpCode(  15 :  _ ) => NE ()
    rule #dasmOpCode(  16 :  _ ) => LT ()
    rule #dasmOpCode(  17 :  _ ) => GT ()
    rule #dasmOpCode(  18 :  _ ) => LE ()
    rule #dasmOpCode(  19 :  _ ) => GE ()
    rule #dasmOpCode(  20 :  _ ) => EQ ()
    rule #dasmOpCode(  21 :  _ ) => ISZERO ()
    rule #dasmOpCode(  22 :  _ ) => AND ()
    rule #dasmOpCode(  23 :  _ ) => OR ()
    rule #dasmOpCode(  24 :  _ ) => XOR ()
    rule #dasmOpCode(  25 :  _ ) => NOT ()
    rule #dasmOpCode(  26 :  _ ) => BYTE ()
    rule #dasmOpCode(  27 :  _ ) => SHIFT ()
    rule #dasmOpCode(  28 :  _ ) => LOGARITHM2 ()
    rule #dasmOpCode(  32 :  _ ) => SHA3 ()
    rule #dasmOpCode(  48 :  _ ) => ADDRESS ()
    rule #dasmOpCode(  49 :  _ ) => BALANCE ()
    rule #dasmOpCode(  50 :  _ ) => ORIGIN ()
    rule #dasmOpCode(  51 :  _ ) => CALLER ()
    rule #dasmOpCode(  52 :  _ ) => CALLVALUE ()
    rule #dasmOpCode(  56 :  _ ) => CODESIZE ()
    rule #dasmOpCode(  58 :  _ ) => GASPRICE ()
    rule #dasmOpCode(  59 :  _ ) => EXTCODESIZE ()
    rule #dasmOpCode(  64 :  _ ) => BLOCKHASH ()
    rule #dasmOpCode(  65 :  _ ) => BENEFICIARY ()
    rule #dasmOpCode(  66 :  _ ) => TIMESTAMP ()
    rule #dasmOpCode(  67 :  _ ) => NUMBER ()
    rule #dasmOpCode(  68 :  _ ) => DIFFICULTY ()
    rule #dasmOpCode(  69 :  _ ) => GASLIMIT ()
    rule #dasmOpCode(  80 :  _ ) => MLOADN ()
    rule #dasmOpCode(  81 :  _ ) => MLOAD ()
    rule #dasmOpCode(  82 :  _ ) => MSTOREN ()
    rule #dasmOpCode(  83 :  _ ) => MSTORE ()
    rule #dasmOpCode(  84 :  _ ) => SLOAD ()
    rule #dasmOpCode(  85 :  _ ) => SSTORE ()
    rule #dasmOpCode(  86 :  _ ) => MSIZE ()
    rule #dasmOpCode(  87 :  _ ) => GAS ()
    rule #dasmOpCode(  96 :  _ ) => MOVE ()
    rule #dasmOpCode(  97 : WS ) => #dasmLoad(97, #loadLen(WS), #loadOffset(WS), WS)
    rule #dasmOpCode(  98 : WS ) => #dasmLoad(98, #loadLen(WS), #loadOffset(WS), WS)
    rule #dasmOpCode( 100 : W1 : W2 : WS ) => BR(W1 *Int 256 +Int W2)
    rule #dasmOpCode( 101 : W1 : W2 : WS ) => BRC(W1 *Int 256 +Int W2)
    rule #dasmOpCode( 102 : W1 : W2 : WS ) => BRLABEL(W1 *Int 256 +Int W2)
    rule #dasmOpCode( 160 :  _ ) => LOG0 ()
    rule #dasmOpCode( 161 :  _ ) => LOG1 ()
    rule #dasmOpCode( 162 :  _ ) => LOG2 ()
    rule #dasmOpCode( 163 :  _ ) => LOG3 ()
    rule #dasmOpCode( 164 :  _ ) => LOG4 ()
    rule #dasmOpCode( 240 : W1 : W2 : W3 : W4 : WS ) => CREATE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4)
    rule #dasmOpCode( 241 : W1 : W2 : WS ) => COPYCREATE(W1 *Int 256 +Int W2)
    rule #dasmOpCode( 242 : W1 : W2 : W3 : W4 : W5 : W6 : WS ) => CALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6)
    rule #dasmOpCode( 243 : W1 : W2 : W3 : W4 : WS ) => CALLDYN(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4)
    rule #dasmOpCode( 244 : W1 : W2 : W3 : W4 : W5 : W6 : WS ) => STATICCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6)
    rule #dasmOpCode( 245 : W1 : W2 : W3 : W4 : WS ) => STATICCALLDYN(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4)
    rule #dasmOpCode( 246 : W1 : W2 : WS ) => RETURN(W1 *Int 256 +Int W2)
    rule #dasmOpCode( 247 :  _ ) => REVERT()
    rule #dasmOpCode( 248 : W1 : W2 : W3 : W4 : W5 : W6 : WS ) => LOCALCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6)
    rule #dasmOpCode( 249 : W1 : W2 : W3 : W4 : WS ) => LOCALCALLDYN(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4)
    rule #dasmOpCode( 250 : W1 : W2 : WS ) => CALLADDRESS(W1 *Int 256 +Int W2)
    rule #dasmOpCode( 254 :  _ ) => INVALID ()
    rule #dasmOpCode( 255 :  _ ) => SELFDESTRUCT ()
    rule #dasmOpCode( _ ) => encodingError() [owise]

    syntax OpCode ::= #dasmLoad ( Int , Int , Int , WordStack ) [function]
 // ----------------------------------------------------------------------
    rule #dasmLoad(97, LEN, POS, WS) => LOADPOS(LEN +Int POS, #asUnsigned(WS [ POS .. LEN ]))
    rule #dasmLoad(98, LEN, POS, WS) => LOADNEG(LEN +Int POS, #asUnsigned(WS [ POS .. LEN ]))
```

```k
endmodule
```

