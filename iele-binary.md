
========================================

Here we define an ad-hoc binary encoding for IELE. This encoding is subject to change and should not be viewed as final. The actual semantics of IELE is defined in terms of a fragment of its textual representation.

```{.k .uiuck .rvk}
requires "iele.k"

module IELE-BINARY
    imports IELE
```

After interpreting the strings representing programs as a `WordStack`, it should be changed into an `Ops` for use by the IELE semantics.

-   `#dasmOps` interperets `WordStack` as an `Ops`.
-   `#dasmRegs` disassembles the registers for a single instruction.
-   `#dasmOpCode` interperets a `Int` as an `OpCode`.

```{.k .uiuck .rvk}

    syntax Ops ::= #dasmOps ( WordStack )       [function]
                 | #dasmOps ( Ops , WordStack , K , Int ) [function, klabel(#dasmOpsAux)]
 // -----------------------------------------------------------------------------
    rule #dasmOps( .WordStack )      => #emptyCode
    rule #dasmOps( 99 : NBITS : WS ) => #revOps(#dasmOps(REGISTERS(NBITS) ; .Ops, WS, .K, NBITS), .Ops)

    rule #dasmOps( OPS, .WordStack, .K, _ ) => OPS

    rule #dasmOps( OPS, W : WS, .K, NREGS ) => #dasmOps(OPS, WS, #dasmOpCode(W), NREGS)
      requires (W >=Int 0   andBool W <=Int 96)
        orBool (W >=Int 106 andBool W <=Int 239)
        orBool (W >=Int 249 andBool W <=Int 255)

    rule #dasmOps( OPS, W : WS, .K,             NREGS ) => #dasmOps(OPS, W : WS, #str(#pushLen(#drop(NREGS up/Int 8, WS)), #pushOffset(#drop(NREGS up/Int 8, WS))), NREGS)
      requires W >=Int 97 andBool W <Int 99
    rule #dasmOps( OPS, 97  : WS, #str(LEN, POS), NREGS ) => #dasmOps(OPS, WS, LOADPOS(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])), NREGS)
    rule #dasmOps( OPS, 98  : WS, #str(LEN, POS), NREGS ) => #dasmOps(OPS, WS, LOADNEG(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])), NREGS)
    rule #dasmOps( OPS, 105 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, #drop(W1 *Int 256 +Int W2, WS), FUNCTION(#unparseByteStack(#take(W1 *Int 256 +Int W2, WS))), NREGS)

    rule #dasmOps( OPS, 100 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, JUMP(W1 *Int 256 +Int W2),      NREGS)
    rule #dasmOps( OPS, 101 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, JUMPI(W1 *Int 256 +Int W2),     NREGS)
    rule #dasmOps( OPS, 102 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, JUMPDEST(W1 *Int 256 +Int W2),  NREGS)
    rule #dasmOps( OPS, 103 : W1 : W2 : W3 : W4 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CALLDEST(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4), NREGS)
    rule #dasmOps( OPS, 104 : W1 : W2 : W3 : W4 : WS, .K, NREGS ) => #dasmOps(OPS, WS, EXTCALLDEST(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4), NREGS)
    rule #dasmOps( OPS, 240 : W1 : W2 : W3 : W4 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CREATE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4), NREGS)
    rule #dasmOps( OPS, 241 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, COPYCREATE(W1 *Int 256 +Int W2), NREGS)
    rule #dasmOps( OPS, 242 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 243 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CALLCODE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 244 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, DELEGATECALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 245 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, STATICCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 246 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, RETURN(W1 *Int 256 +Int W2), NREGS)
    rule #dasmOps( OPS, 247 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, REVERT(W1 *Int 256 +Int W2), NREGS)
    rule #dasmOps( OPS, 248 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, LOCALCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)

    rule #dasmOps( OPS, WS, OP:OpCode, NREGS) => #dasmOps(#dasmRegs(OP, WS, NREGS) ; OPS, #drop(#opWidth(OP, NREGS) -Int #opCodeWidth(OP), WS), .K, NREGS)

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

    syntax Reg ::= "%" "(" Int "," Int "," Int "," Int ")" [function]
 // -----------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX) => % ((REGS >>Int (IDX *Int WIDTH)) &Int MASK)

    syntax Regs ::= "%" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // --------------------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX, 0) => .Regs
    rule %(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX) %(REGS, WIDTH, MASK, IDX +Int 1, COUNT -Int 1) [owise]

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
    rule #numArgs ( _:NullOp )     => 1
    rule #numArgs ( _:NullVoidOp ) => 0
    rule #numArgs ( _:HeaderOp )   => 0
    rule #numArgs ( _:UnOp )       => 2
    rule #numArgs ( _:UnVoidOp )   => 1
    rule #numArgs ( _:BinOp )      => 3
    rule #numArgs ( _:BinVoidOp )  => 2
    rule #numArgs ( _:TernOp )     => 4
    rule #numArgs ( _:TernVoidOp ) => 3
    rule #numArgs ( _:QuadVoidOp ) => 4
    rule #numArgs ( _:FiveVoidOp ) => 5
    rule #numArgs ( _:CallSixOpCode(_, ARGS, RETS) ) => 3 +Int ARGS +Int RETS
    rule #numArgs ( _:CallOpCode   (_, ARGS, RETS) ) => 4 +Int ARGS +Int RETS
    rule #numArgs ( LOCALCALL      (_, ARGS, RETS) ) => ARGS +Int RETS
    rule #numArgs ( RETURN(RETS) )                => RETS
    rule #numArgs ( REVERT(RETS) )                => RETS
    rule #numArgs ( CREATE(_, ARGS) )             => ARGS
    rule #numArgs ( COPYCREATE(ARGS) )            => ARGS

    syntax OpCode ::= #dasmOpCode ( Int) [function]
 // -----------------------------------------------------------
    rule #dasmOpCode(   0 ) => STOP
    rule #dasmOpCode(   1 ) => ADD
    rule #dasmOpCode(   2 ) => MUL
    rule #dasmOpCode(   3 ) => SUB
    rule #dasmOpCode(   4 ) => DIV
    rule #dasmOpCode(   6 ) => MOD
    rule #dasmOpCode(   7 ) => EXP
    rule #dasmOpCode(   8 ) => ADDMOD
    rule #dasmOpCode(   9 ) => MULMOD
    rule #dasmOpCode(  10 ) => EXPMOD
    rule #dasmOpCode(  11 ) => SIGNEXTEND
    rule #dasmOpCode(  12 ) => TWOS
    rule #dasmOpCode(  16 ) => LT
    rule #dasmOpCode(  17 ) => GT
    rule #dasmOpCode(  20 ) => EQ
    rule #dasmOpCode(  21 ) => ISZERO
    rule #dasmOpCode(  22 ) => AND
    rule #dasmOpCode(  23 ) => OR
    rule #dasmOpCode(  24 ) => XOR
    rule #dasmOpCode(  25 ) => NOT
    rule #dasmOpCode(  26 ) => BYTE
    rule #dasmOpCode(  32 ) => SHA3
    rule #dasmOpCode(  48 ) => ADDRESS
    rule #dasmOpCode(  49 ) => BALANCE
    rule #dasmOpCode(  50 ) => ORIGIN
    rule #dasmOpCode(  51 ) => CALLER
    rule #dasmOpCode(  52 ) => CALLVALUE
    rule #dasmOpCode(  56 ) => CODESIZE
    rule #dasmOpCode(  58 ) => GASPRICE
    rule #dasmOpCode(  59 ) => EXTCODESIZE
    rule #dasmOpCode(  64 ) => BLOCKHASH
    rule #dasmOpCode(  65 ) => COINBASE
    rule #dasmOpCode(  66 ) => TIMESTAMP
    rule #dasmOpCode(  67 ) => NUMBER
    rule #dasmOpCode(  68 ) => DIFFICULTY
    rule #dasmOpCode(  69 ) => GASLIMIT
    rule #dasmOpCode(  80 ) => MLOADN
    rule #dasmOpCode(  81 ) => MLOAD
    rule #dasmOpCode(  82 ) => MSTOREN
    rule #dasmOpCode(  83 ) => MSTORE
    rule #dasmOpCode(  84 ) => SLOAD
    rule #dasmOpCode(  85 ) => SSTORE
    rule #dasmOpCode(  86 ) => MSIZE
    rule #dasmOpCode(  87 ) => GAS
    rule #dasmOpCode(  96 ) => MOVE
    rule #dasmOpCode( 160 ) => LOG0
    rule #dasmOpCode( 161 ) => LOG1
    rule #dasmOpCode( 162 ) => LOG2
    rule #dasmOpCode( 163 ) => LOG3
    rule #dasmOpCode( 164 ) => LOG4
    rule #dasmOpCode( 255 ) => SELFDESTRUCT
    rule #dasmOpCode(   W ) => INVALID [owise]


endmodule
```
