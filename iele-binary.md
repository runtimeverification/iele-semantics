
========================================

Here we define an ad-hoc binary encoding for IELE. This encoding is subject to change and should not be viewed as final. The actual semantics of IELE is defined in terms of a fragment of its textual representation.

```{.k .uiuck .rvk}
requires "iele.k"
requires "iele-syntax.k"

module IELE-BINARY
    imports IELE
    imports IELE-COMMON
```

After interpreting the strings representing programs as a `WordStack`, it should be changed into a `Contract` for use by the IELE semantics.

-   `#dasmContract` interperets `WordStack` as a `Contract`.
-   `#dasmRegs` disassembles the registers for a single instruction.
-   `#dasmOpCode` interperets a `Int` as an `OpCode`.

```{.k .uiuck .rvk}

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
                    | COINBASE ()
                    | TIMESTAMP ()
                    | NUMBER ()
                    | DIFFICULTY ()
                    | MSIZE ()
                    | SELFDESTRUCT ()
                    | LOG0 ()

    syntax CreateOp ::= CREATE ( Int , Int )

    syntax CopyCreateOp ::= COPYCREATE ( Int )

    syntax CallOp ::= CALL ( Int , Int , Int )
                    | CALLCODE ( Int , Int , Int )

    syntax CallSixOp ::= DELEGATECALL ( Int , Int , Int )
                       | STATICCALL ( Int , Int , Int )

    syntax ReturnOp ::= RETURN ( Int )
                      | REVERT ( Int )

    syntax LocalCallOp ::= LOCALCALL ( Int , Int , Int )

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
                    | BYTE ()
                    | LOG2 ()

    syntax QuadOp ::= ADDMOD ()
                    | MULMOD ()
                    | EXPMOD ()
                    | MLOADN ()
                    | MSTOREN ()
                    | LOG3 ()

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

    syntax FiveOp ::= LOG4 ()

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

    syntax Contract ::= #dasmContract ( WordStack , IeleName )       [function]
                      | #dasmContract ( WordStack , Int , Map, IeleName , TopLevelDefinitions, Int , Int ) [function, klabel(#dasmContractAux)]
 // -----------------------------------------------------------------------------
    rule #dasmContract( .WordStack, _)      => #emptyCode
    rule #dasmContract( 99 : NBITS : WS, NAME ) => #dasmContract(WS, NBITS, 0 |-> "init", NAME, .TopLevelDefinitions, 1, #sizeWordStack(WS) +Int 2)
    rule #dasmContract( 105 : W1 : W2 : WS, NBITS, FUNCS, NAME, DEFS, N, SIZE ) => #dasmContract(#drop(W1 *Int 256 +Int W2, WS), NBITS, N |-> #unparseByteStack(#take(W1 *Int 256 +Int W2, WS)) FUNCS, NAME, DEFS, N +Int 1, SIZE )
    rule #dasmContract( 106 : W1 : W2 : WS, NBITS, FUNCS, NAME, DEFS, N, SIZE ) => #dasmContract(#take(W1 *Int 256 +Int W2, WS), N) ++Contract #dasmContract(#drop(W1 *Int 256 +Int W2, WS), NBITS, FUNCS, NAME, external contract N DEFS, N +Int 1, SIZE)
    rule #dasmContract( WS, NBITS, FUNCS, NAME, DEFS, N, SIZE ) => contract NAME ! SIZE { DEFS ++TopLevelDefintions #dasmFunctions(WS, NBITS, FUNCS) } .Contract

    syntax TopLevelDefinitions ::= #dasmFunctions ( WordStack , Int , Map ) [function]
    syntax TopLevelDefinitions ::= #dasmFunction ( Bool , IeleName , Int , WordStack , Int , Map , Blocks , K ) [function]
 // -----------------------------------------------------------------------------
    rule #dasmFunctions(103 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS) => #dasmFunction(false, W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, WS, NBITS, FUNCS, .LabeledBlocks, .K)
    rule #dasmFunctions(104 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS) => #dasmFunction(true, String2IeleName({FUNCS [ W1 *Int 256 +Int W2 ]}:>String), W3 *Int 256 +Int W4, WS, NBITS, FUNCS, .LabeledBlocks, .K)

    rule #dasmFunction(false, NAME, SIG, W : WS, NBITS, FUNCS, BLOCKS, .K) => define NAME ( SIG ) { BLOCKS } #dasmFunctions(W : WS, NBITS, FUNCS)
      requires W ==Int 103 orBool W ==Int 104
    rule #dasmFunction(true, NAME, SIG, W : WS, NBITS, FUNCS, BLOCKS, .K) => define public NAME ( SIG ) { BLOCKS } #dasmFunctions(W : WS, NBITS, FUNCS)
      requires W ==Int 103 orBool W ==Int 104
    rule #dasmFunction(false, NAME, SIG, .WordStack, NBITS, FUNCS, BLOCKS, .K) => define NAME ( SIG ) { BLOCKS } .TopLevelDefinitions
    rule #dasmFunction(true, NAME, SIG, .WordStack, NBITS, FUNCS, BLOCKS, .K) => define public NAME ( SIG ) { BLOCKS } .TopLevelDefinitions

    rule #dasmFunction(PUBLIC, NAME, SIG, W : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, #dasmOpCode(W))
      requires (W >=Int 0   andBool W <=Int 96)
        orBool (W >=Int 107 andBool W <=Int 239)
        orBool (W >=Int 249 andBool W <=Int 255)

    rule #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, OP:OpCode) => #dasmFunction(PUBLIC, NAME, SIG, #drop(#opWidth(OP, NBITS), WS), NBITS, FUNCS, #dasmInstruction(OP, #take(#opWidth(OP, NBITS), WS), NBITS) BLOCKS, .K)

    rule #dasmFunction(PUBLIC, NAME, SIG, W : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, W : WS, #str(#pushLen(#drop(NBITS up/Int 8, WS)), #pushOffset(#drop(NBITS up/Int 8, WS))))
      requires W >=Int 97 andBool W <Int 99
    rule #dasmFunction(PUBLIC, NAME, SIG, 97  : WS, NBITS, FUNCS, BLOCKS, #str(LEN, POS)) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, LOADPOS(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])))
    rule #dasmFunction(PUBLIC, NAME, SIG, 98  : WS, NBITS, FUNCS, BLOCKS, #str(LEN, POS)) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, LOADNEG(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])))

    rule #dasmFunction(PUBLIC, NAME, SIG, 100 : W1 : W2 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, JUMP(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, SIG, 101 : W1 : W2 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, JUMPI(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, SIG, 102 : W1 : W2 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, JUMPDEST(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, SIG, 241 : W1 : W2 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, COPYCREATE(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, SIG, 246 : W1 : W2 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, RETURN(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, SIG, 247 : W1 : W2 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, REVERT(W1 *Int 256 +Int W2))
    rule #dasmFunction(PUBLIC, NAME, SIG, 240 : W1 : W2 : W3 : W4 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, CREATE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4))
    rule #dasmFunction(PUBLIC, NAME, SIG, 242 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, CALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))
    rule #dasmFunction(PUBLIC, NAME, SIG, 243 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, CALLCODE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))
    rule #dasmFunction(PUBLIC, NAME, SIG, 244 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, DELEGATECALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))
    rule #dasmFunction(PUBLIC, NAME, SIG, 245 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, STATICCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))
    rule #dasmFunction(PUBLIC, NAME, SIG, 248 : W1 : W2 : W3 : W4 : W5 : W6 : WS, NBITS, FUNCS, BLOCKS, .K) => #dasmFunction(PUBLIC, NAME, SIG, WS, NBITS, FUNCS, BLOCKS, LOCALCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6))


    syntax Op ::= #dasmRegs ( OpCode , WordStack , Int ) [function]
                | #dasmRegs ( OpCode , Int , Int , Int ) [function, klabel(#dasmRegsAux)]
 // -------------------------------------------------------------------------------------
    rule #dasmRegs ( LOADPOS(N, W), WS, NREGS ) => #dasmRegs(LOADPOS(N, W), #asUnsigned(#take(NREGS up/Int 8, WS)),                            NREGS, (1 <<Int NREGS) -Int 1)
    rule #dasmRegs ( LOADNEG(N, W), WS, NREGS ) => #dasmRegs(LOADNEG(N, W), #asUnsigned(#take(NREGS up/Int 8, WS)),                            NREGS, (1 <<Int NREGS) -Int 1)
    rule #dasmRegs ( OP,            WS, NREGS ) => #dasmRegs(OP,            #asUnsigned(#take(#opWidth(OP, NREGS) -Int #opCodeWidth(OP), WS)), NREGS, (1 <<Int NREGS) -Int 1) [owise]

    rule #dasmRegs ( OP:NullOp,     R, W, M ) => OP %(R, W, M, 0)
    rule #dasmRegs ( OP:NullVoidOp, R, W, M ) => OP
    rule #dasmRegs ( OP:HeaderOp,   R, W, M ) => OP
    rule #dasmRegs ( OP:UnOp,       R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1)
    rule #dasmRegs ( OP:UnVoidOp,   R, W, M ) => OP %(R, W, M, 0)
    rule #dasmRegs ( OP:BinOp,      R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2)
    rule #dasmRegs ( OP:BinVoidOp,  R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1)
    rule #dasmRegs ( OP:TernOp,     R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3)
    rule #dasmRegs ( OP:TernVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2)
    rule #dasmRegs ( OP:QuadVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3)
    rule #dasmRegs ( OP:FiveVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3) %(R, W, M, 4)

    rule #dasmRegs ( _:CallSixOpCode (_, ARGS, RETS) #as OP::CallSixOp,   R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1 +Int RETS) %(R, W, M, 2 +Int RETS)                         %(R, W, M, 1, RETS) %(R, W, M, RETS +Int 3, ARGS)
    rule #dasmRegs ( _:CallOpCode    (_, ARGS, RETS) #as OP::CallOp,      R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1 +Int RETS) %(R, W, M, 2 +Int RETS) %(R, W, M, 3 +Int RETS) %(R, W, M, 1, RETS) %(R, W, M, RETS +Int 4, ARGS)
    rule #dasmRegs ( LOCALCALL       (_, ARGS, RETS) #as OP::LocalCallOp, R, W, M ) => OP                                                                                       %(R, W, M, 0, RETS) %(R, W, M, RETS,        ARGS)

    rule #dasmRegs ( CREATE (_,  ARGS) #as OP::CreateOp,     R, W, M) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2, ARGS)
    rule #dasmRegs ( COPYCREATE (ARGS) #as OP::CopyCreateOp, R, W, M) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3, ARGS)

    rule #dasmRegs ( REVERT(RETS), R, W, M ) => REVERT(RETS) %(R, W, M, 0, RETS)
    rule #dasmRegs ( RETURN(RETS), R, W, M ) => RETURN(RETS) %(R, W, M, 0, RETS)

    syntax Operand ::= "%" "(" Int "," Int "," Int "," Int ")" [function]
 // -----------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX) => % ((REGS >>Int (IDX *Int WIDTH)) &Int MASK)

    syntax Operands ::= "%" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // --------------------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX, 0) => .Operands
    rule %(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX) , %(REGS, WIDTH, MASK, IDX +Int 1, COUNT -Int 1) [owise]

    syntax LValues ::= "%" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // --------------------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX, 0) => .LValues
    rule %(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX) , %(REGS, WIDTH, MASK, IDX +Int 1, COUNT -Int 1) [owise]

    syntax Int ::= #opWidth ( OpCode , Int ) [function]
 // ---------------------------------------------------
    rule #opWidth ( LOADPOS(N, _), NREGS ) => 1 +Int N +Int (NREGS up/Int 8)
    rule #opWidth ( LOADNEG(N, _), NREGS ) => 1 +Int N +Int (NREGS up/Int 8)
    rule #opWidth ( OP, NREGS ) => #opCodeWidth(OP) +Int ((NREGS *Int #numArgs(OP)) up/Int 8) [owise]

    syntax Int ::= #opCodeWidth ( OpCode ) [function]
 // -------------------------------------------------
    rule #opCodeWidth( JUMPDEST(_) )      => 3
    rule #opCodeWidth( JUMP(_) )          => 3
    rule #opCodeWidth( JUMPI(_) )         => 3
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
    rule #numArgs ( DELEGATECALL(_, ARGS, RETS) ) => 3 +Int ARGS +Int RETS
    rule #numArgs ( STATICCALL  (_, ARGS, RETS) ) => 3 +Int ARGS +Int RETS
    rule #numArgs ( CALL        (_, ARGS, RETS) ) => 4 +Int ARGS +Int RETS
    rule #numArgs ( CALLCODE    (_, ARGS, RETS) ) => 4 +Int ARGS +Int RETS
    rule #numArgs ( LOCALCALL   (_, ARGS, RETS) ) => ARGS +Int RETS
    rule #numArgs ( RETURN(RETS) )                => RETS
    rule #numArgs ( REVERT(RETS) )                => RETS
    rule #numArgs ( CREATE(_, ARGS) )             => ARGS
    rule #numArgs ( COPYCREATE(ARGS) )            => ARGS

    syntax OpCode ::= #dasmOpCode ( Int) [function]
 // -----------------------------------------------------------
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
    rule #dasmOpCode(  65 ) => COINBASE ()
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
