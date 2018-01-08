IELE Binary Encoding (Work In Progress)
=======================================

Here we define an ad-hoc binary encoding for IELE. This encoding is subject to change and should not be viewed as final. The actual semantics of IELE is defined in terms of a fragment of its textual representation.
You can use the IELE assembler provided with the semantics to convert from the textual encoding into this binary encoding.

```{.k .uiuck .rvk .standalone .node}
requires "iele.k"
requires "iele-syntax.k"

module IELE-BINARY
    imports IELE
    imports IELE-COMMON
```

Here we define the OpCode sort. An OpCode is the atomic unit of a IELE program in binary representation and a IELE contract can be expressed as a sequence of operations.
Each operation consists of its OpCode plus zero or more bytes containing the register operands to the operation. The OpCode consists of between one and seven bytes.
The first byte represents the operation in question, and contains enough information to determine and decode the rest of the OpCode. The remainder of the OpCode then
contains enough information to determine and decode the rest of the operation.

```{.k .uiuck .rvk .standalone .node}

    syntax NullOp ::= LOADPOS ( Int , Int )
                    | LOADNEG ( Int , Int )
                    | BRLABEL ( Int )
                    | BR ( Int )
                    | INVALID ()

    syntax UnOp   ::= BRC ( Int )
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

    syntax BinOp ::= ISZERO ()
                   | NOT ()
                   | SHA3 ()
                   | MLOAD ()
                   | MSTORE ()
                   | SLOAD ()
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

    syntax CallSixOp ::= STATICCALL ( Int , Int , Int )

    syntax ReturnOp ::= RETURN ( Int )
                      | REVERT ( Int )

    syntax LocalCallOp ::= LOCALCALL ( Int , Int , Int )

    syntax OpCode ::= NullOp
                    | UnOp
                    | CreateOp
                    | CopyCreateOp
                    | CallOp
                    | CallSixOp
                    | ReturnOp
                    | LocalCallOp
                    | TernOp
                    | QuadOp
                    | BinOp
                    | FiveOp

```

After interpreting the strings representing programs as a `WordStack`, it should be changed into a `Contract` for use by the IELE semantics.

-   `#dasmContract` interperets `WordStack` as a `Contract`.
-   `#dasmFunction` interprets a single function of a contract represented as a `WordStack` into a `TopLevelDefinition`
-   `#dasmInstruction` disassembles the registers for a single instruction.
-   `#dasmOpCode` interperets a `Int` as an `OpCode`.

```{.k .uiuck .rvk .standalone .node}

    syntax Contract ::= #dasmContract ( WordStack , IeleName )       [function]
                      | #dasmContract ( WordStack , Int , Map, IeleName , TopLevelDefinitions, Int , Int ) [function, klabel(#dasmContractAux)]
 // -------------------------------------------------------------------------------------------------------------------------------------------
    rule #dasmContract( .WordStack, _) => #emptyCode
    rule #dasmContract( 99 : NBITS : WS, NAME ) => #dasmContract(WS, NBITS, 0 |-> init, NAME, .TopLevelDefinitions, 1, #sizeWordStack(WS) +Int 2)
    rule #dasmContract( 105 : W1 : W2 : WS, NBITS, FUNCS, NAME, DEFS, N, SIZE ) => #dasmContract(#drop(W1 *Int 256 +Int W2, WS), NBITS, N |-> #parseToken("IeleName", #unparseByteStack(#take(W1 *Int 256 +Int W2, WS))) FUNCS, NAME, DEFS, N +Int 1, SIZE )
    rule #dasmContract( 106 : W1 : W2 : WS, NBITS, FUNCS, NAME, DEFS, N, SIZE ) => #dasmContract(#take(W1 *Int 256 +Int W2, WS), NAME +.+IeleName N) ++Contract #dasmContract(#drop(W1 *Int 256 +Int W2, WS), NBITS, FUNCS, NAME, external contract NAME +.+IeleName N DEFS, N +Int 1, SIZE)
    rule #dasmContract( WS, NBITS, FUNCS, NAME, DEFS, N, SIZE ) => contract NAME ! SIZE { DEFS ++TopLevelDefinitions #dasmFunctions(WS, NBITS, FUNCS, NAME) } .Contract [owise]

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
    rule N +.+IeleName M => #parseToken("IeleName", IeleName2String(N) +String "." +String IeleName2String(M)) 

    syntax TopLevelDefinitions ::= #dasmFunctions ( WordStack , Int , Map , IeleName ) [function]
    syntax TopLevelDefinitions ::= #dasmFunction ( Bool , IeleName , IeleName , Int , WordStack , Int , Map , Instructions , K ) [function]
 // ----------------------------------------------------------------------------------------------------------------------------
    rule #dasmFunctions(103 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS, NAME) => #dasmFunction(false, {FUNCS [ W1 *Int 256 +Int W2 ] orDefault W1 *Int 256 +Int W2}:>IeleName, NAME, W3 *Int 256 +Int W4, WS, NBITS, FUNCS, .Instructions, .K)
    rule #dasmFunctions(104 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS, NAME) => #dasmFunction(true, {FUNCS [ W1 *Int 256 +Int W2 ] orDefault W1 *Int 256 +Int W2}:>IeleName, NAME, W3 *Int 256 +Int W4, WS, NBITS, FUNCS, .Instructions, .K)

    rule #dasmFunction(false, NAME, CNAME, SIG, W : WS, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(W : WS, NBITS, FUNCS, CNAME)
      requires W ==Int 103 orBool W ==Int 104
    rule #dasmFunction(true, NAME, CNAME, SIG, W : WS, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } #dasmFunctions(W : WS, NBITS, FUNCS, CNAME)
      requires W ==Int 103 orBool W ==Int 104
    rule #dasmFunction(false, NAME, CNAME, SIG, .WordStack, NBITS, FUNCS, INSTRS, .K) => define @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions
    rule #dasmFunction(true, NAME, CNAME, SIG, .WordStack, NBITS, FUNCS, INSTRS, .K) => define public @ NAME ( SIG ) { #toBlocks(INSTRS) } .TopLevelDefinitions

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, W : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, #dasmOpCode(W))
      requires (W >=Int 0   andBool W <=Int 96)
        orBool (W >=Int 107 andBool W <=Int 239)
        orBool (W >=Int 249 andBool W <=Int 255)

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, OP:OpCode) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, #drop(#opWidth(OP, NBITS) -Int #opCodeWidth(OP), WS), NBITS, FUNCS, #dasmInstruction(OP, #take(#opWidth(OP, NBITS) -Int #opCodeWidth(OP), WS), NBITS, FUNCS, CNAME) INSTRS, .K)

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, W : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, W : WS, NBITS, FUNCS, INSTRS, #str(#loadLen(#drop(NBITS up/Int 8, WS)), #loadOffset(#drop(NBITS up/Int 8, WS))))
      requires W >=Int 97 andBool W <Int 99
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 97  : WS, NBITS, FUNCS, INSTRS, #str(LEN, POS)) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, LOADPOS(LEN +Int POS, #asUnsigned(WS [ POS +Int (NBITS up/Int 8) .. LEN ])))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 98  : WS, NBITS, FUNCS, INSTRS, #str(LEN, POS)) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, LOADNEG(LEN +Int POS, #asUnsigned(WS [ POS +Int (NBITS up/Int 8) .. LEN ])))

    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 100 : W1 : W2 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, BR(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 101 : W1 : W2 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, BRC(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 102 : W1 : W2 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, BRLABEL(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 241 : W1 : W2 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, COPYCREATE(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 246 : W1 : W2 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, RETURN(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 247 : W1 : W2 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, REVERT(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 240 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, CREATE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 242 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, CALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 245 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, STATICCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))
    rule #dasmFunction(PUBLIC, NAME, CNAME, SIG, 248 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, INSTRS, .K) => #dasmFunction(PUBLIC, NAME, CNAME, SIG, WS, NBITS, FUNCS, INSTRS, LOCALCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))

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
    rule #dasmInstruction ( INVALID (),        R, W, M, _, _ ) => %(R, W, M, 0) = call @iele.invalid ( .Operands )
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

    rule #dasmInstruction ( STATICCALL (LABEL, ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS +Int 1) = staticcall @ {F [ LABEL ]}:>IeleName at %(R, W, M, 2 +Int RETS) ( %o(R, W, M, 3 +Int RETS, ARGS) ) gaslimit %(R, W, M, 1 +Int RETS)
    rule #dasmInstruction ( CALL (LABEL, ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS +Int 1) = call @ {F [ LABEL ]}:>IeleName at %(R, W, M, 2 +Int RETS) ( %o(R, W, M, 4 +Int RETS, ARGS) ) send %(R, W, M, 3 +Int RETS) , gaslimit %(R, W, M, 1 +Int RETS)
    rule #dasmInstruction ( LOCALCALL (LABEL, ARGS, RETS), R, W, M, F, _ ) => %l(R, W, M, 0, RETS) = call @ {F [ LABEL ] orDefault LABEL}:>IeleName ( %o(R, W, M, RETS, ARGS) )

    rule #dasmInstruction ( CREATE (LABEL, ARGS), R, W, M, _, NAME ) => %(R, W, M, 0) , %(R, W, M, 1) = create NAME +.+IeleName {#parseToken("IeleName", Int2String(LABEL))}:>IeleName ( %o(R, W, M, 3, ARGS) ) send %(R, W, M, 2)
    rule #dasmInstruction ( COPYCREATE (ARGS), R, W, M, _, _ ) => %(R, W, M, 0) , %(R, W, M, 1) = copycreate %(R, W, M, 3) ( %o(R, W, M, 4, ARGS) ) send %(R, W, M, 2)

    rule #dasmInstruction ( REVERT(1), R, W, M, _, _ ) => revert %(R, W, M, 0)
    rule #dasmInstruction ( RETURN(RETS), R, W, M, _, _ ) => ret %o(R, W, M, 0, RETS)
      requires RETS =/=Int 0
    rule #dasmInstruction ( RETURN(0), R, W, M, _, _ ) => ret void

    syntax LValue ::= "%" "(" Int "," Int "," Int "," Int ")" [function]
 // --------------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX) => % ((REGS >>Int (IDX *Int WIDTH)) &Int MASK)

    syntax Operands ::= "%o" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // -------------------------------------------------------------------------------
    rule %o(REGS, WIDTH, MASK, IDX, 0) => .Operands
    rule %o(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX) , %o(REGS, WIDTH, MASK, IDX +Int 1, COUNT -Int 1) [owise]

    syntax LValues ::= "%l" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // ------------------------------------------------------------------------------
    rule %l(REGS, WIDTH, MASK, IDX, 0) => .LValues
    rule %l(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX) , %l(REGS, WIDTH, MASK, IDX +Int 1, COUNT -Int 1) [owise]

    syntax Int ::= #opWidth ( OpCode , Int ) [function]
 // ---------------------------------------------------
    rule #opWidth ( LOADPOS(N, _), NBITS ) => 1 +Int N +Int (NBITS up/Int 8)
    rule #opWidth ( LOADNEG(N, _), NBITS ) => 1 +Int N +Int (NBITS up/Int 8)
    rule #opWidth ( OP, NBITS ) => #opCodeWidth(OP) +Int ((NBITS *Int #numArgs(OP)) up/Int 8) [owise]

    syntax Int ::= #opCodeWidth ( OpCode ) [function]
 // -------------------------------------------------
    rule #opCodeWidth( BRLABEL(_) )       => 3
    rule #opCodeWidth( BR(_) )            => 3
    rule #opCodeWidth( BRC(_) )           => 3
    rule #opCodeWidth( LOCALCALL(_,_,_) ) => 7
    rule #opCodeWidth( RETURN(_) )        => 3
    rule #opCodeWidth( REVERT(_) )        => 3
    rule #opCodeWidth( _:CallOp )         => 7
    rule #opCodeWidth( _:CallSixOp )      => 7
    rule #opCodeWidth( _:CreateOp )       => 5
    rule #opCodeWidth( _:CopyCreateOp )   => 3
    rule #opCodeWidth( OP )               => 1 [owise]

    syntax Int ::= #numArgs ( OpCode ) [function]
 // ---------------------------------------------
    rule #numArgs ( _:NullOp ) => 0
    rule #numArgs ( _:UnOp )   => 1
    rule #numArgs ( _:BinOp )  => 2
    rule #numArgs ( _:TernOp ) => 3
    rule #numArgs ( _:QuadOp ) => 4
    rule #numArgs ( _:FiveOp ) => 5
    rule #numArgs ( STATICCALL  (_, ARGS, RETS) ) => 3 +Int ARGS +Int RETS
    rule #numArgs ( CALL        (_, ARGS, RETS) ) => 4 +Int ARGS +Int RETS
    rule #numArgs ( LOCALCALL   (_, ARGS, RETS) ) => ARGS +Int RETS
    rule #numArgs ( RETURN(RETS) )                => RETS
    rule #numArgs ( REVERT(RETS) )                => RETS
    rule #numArgs ( CREATE(_, ARGS) )             => 3 +Int ARGS
    rule #numArgs ( COPYCREATE(ARGS) )            => 4 +Int ARGS

    syntax OpCode ::= #dasmOpCode ( Int ) [function]
 // ------------------------------------------------
    rule #dasmOpCode(   1 ) => ADD ()
    rule #dasmOpCode(   2 ) => MUL ()
    rule #dasmOpCode(   3 ) => SUB ()
    rule #dasmOpCode(   4 ) => DIV ()
    rule #dasmOpCode(   6 ) => MOD ()
    rule #dasmOpCode(   7 ) => EXP ()
    rule #dasmOpCode(   8 ) => ADDMOD ()
    rule #dasmOpCode(   9 ) => MULMOD ()
    rule #dasmOpCode(  10 ) => EXPMOD ()
    rule #dasmOpCode(  11 ) => SIGNEXTEND ()
    rule #dasmOpCode(  12 ) => TWOS ()
    rule #dasmOpCode(  15 ) => NE ()
    rule #dasmOpCode(  16 ) => LT ()
    rule #dasmOpCode(  17 ) => GT ()
    rule #dasmOpCode(  18 ) => LE ()
    rule #dasmOpCode(  19 ) => GE ()
    rule #dasmOpCode(  20 ) => EQ ()
    rule #dasmOpCode(  21 ) => ISZERO ()
    rule #dasmOpCode(  22 ) => AND ()
    rule #dasmOpCode(  23 ) => OR ()
    rule #dasmOpCode(  24 ) => XOR ()
    rule #dasmOpCode(  25 ) => NOT ()
    rule #dasmOpCode(  26 ) => BYTE ()
    rule #dasmOpCode(  27 ) => SHIFT ()
    rule #dasmOpCode(  32 ) => SHA3 ()
    rule #dasmOpCode(  48 ) => ADDRESS ()
    rule #dasmOpCode(  49 ) => BALANCE ()
    rule #dasmOpCode(  50 ) => ORIGIN ()
    rule #dasmOpCode(  51 ) => CALLER ()
    rule #dasmOpCode(  52 ) => CALLVALUE ()
    rule #dasmOpCode(  56 ) => CODESIZE ()
    rule #dasmOpCode(  58 ) => GASPRICE ()
    rule #dasmOpCode(  59 ) => EXTCODESIZE ()
    rule #dasmOpCode(  64 ) => BLOCKHASH ()
    rule #dasmOpCode(  65 ) => BENEFICIARY ()
    rule #dasmOpCode(  66 ) => TIMESTAMP ()
    rule #dasmOpCode(  67 ) => NUMBER ()
    rule #dasmOpCode(  68 ) => DIFFICULTY ()
    rule #dasmOpCode(  69 ) => GASLIMIT ()
    rule #dasmOpCode(  80 ) => MLOADN ()
    rule #dasmOpCode(  81 ) => MLOAD ()
    rule #dasmOpCode(  82 ) => MSTOREN ()
    rule #dasmOpCode(  83 ) => MSTORE ()
    rule #dasmOpCode(  84 ) => SLOAD ()
    rule #dasmOpCode(  85 ) => SSTORE ()
    rule #dasmOpCode(  86 ) => MSIZE ()
    rule #dasmOpCode(  87 ) => GAS ()
    rule #dasmOpCode(  96 ) => MOVE ()
    rule #dasmOpCode( 160 ) => LOG0 ()
    rule #dasmOpCode( 161 ) => LOG1 ()
    rule #dasmOpCode( 162 ) => LOG2 ()
    rule #dasmOpCode( 163 ) => LOG3 ()
    rule #dasmOpCode( 164 ) => LOG4 ()
    rule #dasmOpCode( 255 ) => SELFDESTRUCT ()
    rule #dasmOpCode(   W ) => INVALID () [owise]


endmodule
```

