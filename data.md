IELE Words
=========

IELE uses arbitrary-precision integers, and sometimes also bytes (8 bit words).
Here we provide the arithmetic of these words, as well as some data-structures over them.
Both are implemented using K's `Int`.

```k
requires "plugin/plugin/krypto.md"
requires "domains.md"
requires "json.md"

module IELE-DATA
    imports KRYPTO
    imports STRING-BUFFER
    imports ARRAY
    imports BYTES
    imports IELE-CONSTANTS
    imports IELE-COMMON
    imports COLLECTIONS
    imports JSON

    syntax KResult ::= Int
```

Some important numbers that are referred to often during execution:

```k
    syntax Int ::= "pow30"  [function]
                 | "pow160" [function]
                 | "pow256" [function]
 // ----------------------------------
    rule pow30  => 2 ^Int 30
    rule pow160 => 2 ^Int 160
    rule pow256 => 2 ^Int 256
```

Primitives
----------

Primitives provide the basic conversion from K's sorts `Int` and `Bool` to IELE's words.

-   `chop` interperets an integers modulo $2^256$. This is used when interpreting
    arbitrary precision integers as memory indices.

```k
    syntax Int ::= chop ( Int ) [function]
 // --------------------------------------
    rule chop ( I:Int ) => bitRangeInt(I, 0, 256) requires I <Int 0  orBool I >=Int pow256
    rule chop ( I:Int ) => I               requires I >=Int 0 andBool I <Int pow256
```

-   `bool2Word` interperets a `Bool` as a `Int`.
-   `word2Bool` interperets a `Int` as a `Bool`.

```k
    syntax Int ::= bool2Word ( Bool ) [function]
 // --------------------------------------------
    rule bool2Word(true)  => 1
    rule bool2Word(false) => 0

    syntax Bool ::= word2Bool ( Int ) [function]
 // --------------------------------------------
    rule word2Bool( 0 ) => false
    rule word2Bool( W ) => true  requires W =/=K 0
```

### Empty Account

-   `.Account` represents the case when an account ID is needed, but
    the actual value of the account ID is the empty set. This is used, for example, when
    referring to the destination of a message which creates a new contract.

```k
    syntax Account ::= ".Account" | Int
```

### Register Operations

-   `#sizeRegs(R)` returns the number of registers in a list of Operands.
-   `#sizeLVals(R)` returns the number of registers in a list of LValues.

```k
    syntax Int ::= #sizeRegs ( Operands ) [function]
                 | #sizeRegs ( Operands , Int ) [function, klabel(#sizeRegsAux)]
 // ----------------------------------------------------------------------------
    rule #sizeRegs(REGS) => #sizeRegs(REGS, 0)
    rule #sizeRegs(REG , REGS, N) => #sizeRegs(REGS, N +Int 1)
    rule #sizeRegs(.Operands, N) => N

    syntax Int ::= #sizeLVals ( LValues ) [function]
                 | #sizeLVals ( LValues , Int ) [function, klabel(#sizeLValuesAux)]
 // -------------------------------------------------------------------------------
    rule #sizeLVals(REGS) => #sizeLVals(REGS, 0)
    rule #sizeLVals(REG , REGS, N) => #sizeLVals(REGS, N +Int 1)
    rule #sizeLVals(.LValues, N) => N

    syntax String ::= IeleName2String ( IeleName ) [function]
                    | IeleNameToken2String ( IeleNameToken ) [function, hook(STRING.token2string)]
    syntax IeleNameToken ::= String2IeleName ( String ) [function, hook(STRING.string2token)]
 // -----------------------------------------------------------------------------------------
    rule IeleName2String(I:Int) => Int2String(I)
    rule IeleName2String(N) => IeleNameToken2String(N) [owise]
    syntax String ::= StringIeleName2String ( StringIeleName ) [function, hook(STRING.token2string)]
 // ------------------------------------------------------------------------------------------------

    syntax Int ::= getInt(K) [function]
    syntax IeleName ::= getIeleName(K) [function]
 // ---------------------------------------------
    rule getInt(I:Int) => I
    rule getIeleName(X:IeleName) => X

```

Arithmetic
----------

-   `up/Int` performs integer division but rounds up instead of down.

NOTE: Here, we choose to add `I2 -Int 1` to the numerator beforing doing the division to mimic the C++ implementation.
You could alternatively calculate `I1 %Int I2`, then add one to the normal integer division afterward depending on the result.

```k
    syntax Int ::= Int "up/Int" Int [function, klabel(ceilDiv)]
 // -----------------------------------------------------------
    rule I1 up/Int I2 => (I1 +Int (I2 -Int 1)) /Int I2 requires I2 >Int 0

```

-   `intSize` returns the size in words of an integer.
-   `bitsInWords` converts a number of bits to a number of words.
-   `bytesInWords` ocnverts a number of bytes to a number of words.

```k
    syntax Int ::= intSize ( Int ) [function]
 // -----------------------------------------
    rule intSize(N) => (log2Int(N) +Int 2) up/Int 64 requires N >Int 0
    rule intSize(0) => 1
    rule intSize(N) => intSize(~Int N) requires N <Int 0

    syntax Int ::= intSizes ( Ints ) [function]
 // -------------------------------------------
    rule intSizes(.Ints) => 0
    rule intSizes(I , INTS) => intSize(I) +Int intSizes(INTS)

    syntax Int ::= intSizes ( Array , Int , Schedule ) [function, klabel(intSizesArr)]
                 | intSizes ( Array , Int , Int , Schedule ) [function, klabel(intSizesAux)]
 // -----------------------------------------------------------------------------
    rule intSizes(ARR::Array, I, SCHED) => intSizes(ARR, I, 0, SCHED)
    rule intSizes(ARR::Array, I, I, _) => 0
    rule intSizes(ARR, I, J, SCHED) => intSize(getInt(ARR [ J ])) +Int intSizes(ARR, I, J +Int 1, SCHED)
      requires SCHED =/=K ALBE [owise]
    rule intSizes(ARR, I, J, ALBE) => getInt(ARR [ J ]) +Int intSizes(ARR, I, J +Int 1, ALBE) [owise]

    syntax Int ::= bitsInWords ( Int , Schedule ) [function]
 // ---------------------------------------------
    rule bitsInWords(I, ALBE) => I up/Int 256
    rule bitsInWords(I, _) => I up/Int 64 [owise]

    syntax Int ::= bytesInWords ( Int ) [function]
 // ----------------------------------------------
    rule bytesInWords(I) => I up/Int 8
```

Here we provide simple syntactic sugar over our power-modulus operator.

```k
    syntax Int ::= powmod(Int, Int, Int) [function]
 // -----------------------------------------------
    rule powmod(W0, W1, W2) => W0 ^%Int W1 W2 requires W2 =/=Int 0
```

-   `gcdInt` computes the gcd of two integers.

```k
    syntax Int ::= gcdInt(Int, Int)  [function]
                 | #gcdInt(Int, Int) [function]
 // -------------------------------------------
    rule gcdInt(A, B) => #gcdInt(absInt(A), absInt(B)) requires absInt(A) >=Int absInt(B)
    rule gcdInt(A, B) => #gcdInt(absInt(B), absInt(A)) [owise]
    rule #gcdInt(A, 0) => A
    rule #gcdInt(A, B) => #gcdInt(B, A modInt B) [owise]
```

Bitwise Operators
-----------------

-   `byte` gets byte $N$ (0 being the LSB).

```k
    syntax Int ::= byte ( Int , Int ) [function]
 // --------------------------------------------
    rule byte(N, W) => bitRangeInt(W, N <<Int 3, 8)
```

-   `_<<Byte_` shifts an integer 8 bits to the left.

```k
    syntax Int ::= Int "<<Byte" Int [function]
 // ------------------------------------------
    rule N <<Byte M => N <<Int (8 *Int M)
```

-   `signextend(N, W)` sign-extends from byte $N$ of $W$ (0 being LSB).
-   `twos(N, W)` converts a signed integer from byte $N$ of $W$ to twos-complement representation (0 being LSB).
-   `bswap(N, W)` converts a signed integer from byte $N$ of $W$ from little endian to big endian representation (or back).

```k
    syntax Int ::= signextend ( Int , Int ) [function]
                 | twos ( Int , Int )       [function]
                 | bswap ( Int , Int )      [function]
 // --------------------------------------------------
    rule signextend(N, W) => signExtendBitRangeInt(W, 0, N <<Int 3)

    rule twos(N, W) => bitRangeInt(W, 0, N <<Int 3)

    rule bswap(N, W) => Bytes2Int(Int2Bytes(N, twos(N, W), BE), LE, Unsigned)
```

-   `keccak` serves as a wrapper around the `Keccak256` in `KRYPTO`.

```k
    syntax Int ::= keccak ( Bytes ) [function]
 // ----------------------------------------------
    rule keccak(WS) => #parseHexWord(Keccak256(Bytes2String(WS)))
```

Data Structures
===============

Several data-structures and operations over `Int` are useful to have around.

Memory
------

-   `.Array` is an arbitrary length array of zeroes.
-   `.Memory` is an arbitrary length array of byte buffers.

```k

    syntax Array ::= ".Array" [function]
 // ---------------------------------------------
    rule .Array => makeArray(pow30, 0)
```

Byte Arrays
-----------

The local memory of execution is a byte-array (instead of a word-array).

-   `#asUnsigned` will interpret a substring of a Bytes as a single unsigned integer (with MSB first).
-   `#asAccount` will interpret a Bytes as a single account id (with MSB first).
    Differs from `Bytes2Int` only in that an empty stack represents the empty account, not account zero.
-   `B [ N .. W ]` access the range of `B` beginning with `N` of width `W` (padding with zeros as needed).
-   `B [ N := B' ]` sets elements starting at $N$ of $B$ to $B'$ (padding with zeros as needed).

```k
    syntax Int ::= #asUnsigned ( Int , Int , Bytes )      [function]
                 | #asUnsigned ( Int , Int , Bytes, Int ) [function]
 // ----------------------------------------------------------------
    rule #asUnsigned( I, N, BS ) => #asUnsigned(I, N, BS, 0)
    rule #asUnsigned( _, 0, _, X )  => X
    rule #asUnsigned( I, N, BS, X ) => #asUnsigned( I +Int 1, N -Int 1, BS, (X *Int 256) +Int BS[I] ) requires N >Int 0

    syntax Account ::= #asAccount ( String ) [function]
 // ------------------------------------------------------
    rule #asAccount("") => .Account
    rule #asAccount("0x") => .Account
    rule #asAccount(S::String) => #parseHexWord(S) [owise]

    syntax Bytes ::= Bytes "[" Int ".." Int "]" [function, klabel(bytesRange)]
 // --------------------------------------------------------------------------
    rule B::Bytes [ I .. J ] => padRightBytes(substrBytes(B, I, minInt(lengthBytes(B), I +Int J)), J, 0)
      requires I <Int lengthBytes(B)
    rule B::Bytes [ I .. J ] => padRightBytes(.Bytes, J, 0) [owise]

    syntax Bytes ::= Bytes "[" Int ":=" Bytes "]" [function, klabel(assignBytesRange)]
 // ----------------------------------------------------------------------------------
    rule B::Bytes [ I := B'::Bytes ] => replaceAtBytes(padRightBytes(B, I +Int lengthBytes(B'), 0), I, B')
      requires B' =/=K .Bytes
    rule B::Bytes [ I := B'::Bytes ] => B
      requires B' ==K .Bytes
```

Addresses
---------

-   `#addr` turns a IELE arbitrary-precision word into the corresponding IELE address (modulo 2^160).

```k
    syntax Int ::= #addr ( Int ) [function]
 // ---------------------------------------
    rule #addr(W) => W modInt pow160
```

-   `#newAddr` computes the address of a new account given the address and nonce of the creating account.
-   `#sender` computes the sender of the transaction from its data and signature.

```k
    syntax Int ::= #newAddr ( Int , Int ) [function]
 // ------------------------------------------------
    rule #newAddr(ACCT, NONCE) => #addr(#parseHexWord(Keccak256(#rlpEncodeLength(#rlpEncodeBytes(ACCT, 20) +String #rlpEncodeWord(NONCE), 192))))

    syntax Account ::= #sender ( String , Int , String , String )                                         [function, klabel(#senderAux)]
                     | #sender ( String )                                                                 [function, klabel(#senderAux2)]
 // ---------------------------------------------------------------------------------------------------------------------------------
    rule #sender(HT, TW, TR, TS) => #sender(ECDSARecover(HT, TW, TR, TS))

    rule #sender("")  => .Account
    rule #sender(STR) => #addr(#parseHexWord(Keccak256(STR))) requires STR =/=String ""
```

Word Map
--------

-   `#removeZeros` removes any entries in a map with zero values.

```k
    syntax Map ::= #removeZeros ( Map ) [function]
                 | #removeZeros ( List , Map ) [function, klabel(#removeZerosAux)]
 // ------------------------------------------------------------------------------
    rule #removeZeros( M )                                   => #removeZeros(Set2List(keys(M)), M)
    rule #removeZeros( .List, .Map )                         => .Map
    rule #removeZeros( ListItem(KEY) L, KEY |-> 0 REST )     => #removeZeros(L, REST)
    rule #removeZeros( ListItem(KEY) L, KEY |-> VALUE REST ) => KEY |-> VALUE #removeZeros(L, REST) requires VALUE =/=K 0
```

Parsing/Unparsing
=================

The IELE test-sets are represented in JSON format with hex-encoding of the data and programs.
Here we provide some standard parser/unparser functions for that format.

Parsing
-------

These parsers can interperet hex-encoded strings as `Int`s, `Bytes`s, and `Map`s.

-   `#parseHexWord` interperets a string as a single hex-encoded `Word`.
-   `#parseByteStack` interperets a string as a hex-encoded stack of bytes, but makes sure to remove the leading "0x".
-   `#parseByteStackRaw` inteprets a string as a stack of bytes.
-   `#parseMap` interperets a JSON key/value object as a map from `Word` to `Word`.
-   `#parseAddr` interperets a string as a 160 bit hex-endcoded address.

```k
    syntax Int ::= #parseHexWord ( String ) [function]
                 | #parseWord    ( String ) [function]
 // --------------------------------------------------
    rule #parseHexWord("")   => 0
    rule #parseHexWord("0x") => 0
    rule #parseHexWord(S)    => String2Base(replaceAll(S, "0x", ""), 16) requires (S =/=String "") andBool (S =/=String "0x")

    rule #parseWord("") => 0
    rule #parseWord(S)  => #parseHexWord(S) requires lengthString(S) >=Int 2 andBool substrString(S, 0, 2) ==String "0x"
    rule #parseWord(S)  => String2Int(S) [owise]

    syntax String ::= #alignHexString ( String ) [function, functional]
 // -------------------------------------------------------------------
    rule #alignHexString(S) => S             requires         lengthString(S) modInt 2 ==Int 0
    rule #alignHexString(S) => "0" +String S requires notBool lengthString(S) modInt 2 ==Int 0
```

```k
    syntax Bytes ::= #parseByteStack    ( String ) [function, memo]
                   | #parseByteStackRaw ( String ) [function]
 // ---------------------------------------------------------------
    rule #parseHexBytes(S)     => #parseHexBytesAux(#alignHexString(S))
    rule #parseHexBytesAux("") => .Bytes
    rule #parseHexBytesAux(S)  => Int2Bytes(lengthString(S) /Int 2, String2Base(S, 16), BE)
      requires lengthString(S) >=Int 2

    syntax Bytes ::= #parseHexBytes     ( String ) [function]
                   | #parseHexBytesAux  ( String ) [function]
 // ---------------------------------------------------------
    rule #parseByteStack(S) => #parseHexBytes(replaceAll(S, "0x", ""))

    rule #parseByteStackRaw(S) => String2Bytes(S)
```

```k
    syntax Map ::= #parseMap ( JSON ) [function]
 // --------------------------------------------
    rule #parseMap( { .JSONs                   } ) => .Map
    rule #parseMap( { _   : (VALUE:String) , REST } ) => #parseMap({ REST })                                                requires #parseHexWord(VALUE) ==K 0
    rule #parseMap( { KEY : (VALUE:String) , REST } ) => #parseMap({ REST }) [ #parseHexWord(KEY) <- #parseHexWord(VALUE) ] requires #parseHexWord(VALUE) =/=K 0

    syntax Int ::= #parseAddr ( String ) [function]
 // -----------------------------------------------
    rule #parseAddr(S) => #addr(#parseHexWord(S))
```

Unparsing
---------

We need to interperet a `Bytes` as a `String` again so that we can call `Keccak256` on it from `KRYPTO`.

-   `#unparseByteStack` turns a stack of bytes (as a `Bytes`) into a `String`.

```k
    syntax String ::= #unparseByteStack ( Bytes ) [function, klabel(unparseByteStack), symbol]
 // ------------------------------------------------------------------------------------------
    rule #unparseByteStack(WS) => Bytes2String(WS)
```

Recursive Length Prefix (RLP)
=============================

RLP encoding is used extensively for executing the blocks of a transaction.
For details about RLP encoding, see the [YellowPaper Appendix B](http://gavwood.com/paper.pdf).
This is included only for compatibility with the EVM test suite.

Encoding
--------

-   `#rlpEncodeWord` RLP encodes a single EVM word.
-   `#rlpEncodeBytes` RLP encodes a single integer as a fixed-width unsigned byte buffer.
-   `#rlpEncodeString` RLP encodes a single `String`.

```k
    syntax String ::= #rlpEncodeWord ( Int )            [function]
                    | #rlpEncodeBytes ( Int , Int )     [function]
                    | #rlpEncodeString ( String )       [function]
                    | #rlpEncodeInts ( Ints ) [function, klabel(rlpEncodeInts), symbol]
                    | #rlpEncodeInts ( StringBuffer, Ints ) [function, klabel(#rlpEncodeIntsAux)]
 // ---------------------------------------------------------------------------------------------
    rule #rlpEncodeWord(0) => "\x80"
    rule #rlpEncodeWord(WORD) => chrChar(WORD) requires WORD >Int 0 andBool WORD <Int 128
    rule #rlpEncodeWord(WORD) => #rlpEncodeLength(Bytes2String(Int2Bytes(WORD, BE, Unsigned)), 128) requires WORD >=Int 128

    rule #rlpEncodeBytes(WORD, LEN) => #rlpEncodeString(Bytes2String(Int2Bytes(LEN, WORD, BE)))

    rule #rlpEncodeString(STR) => STR                        requires lengthString(STR) ==Int 1 andBool ordChar(STR) <Int 128
    rule #rlpEncodeString(STR) => #rlpEncodeLength(STR, 128) [owise]

    rule #rlpEncodeInts(INTS) => #rlpEncodeInts(.StringBuffer, INTS)
    rule #rlpEncodeInts(BUF => BUF +String #rlpEncodeString(Bytes2String(Int2Bytes(I, BE, Signed))), (I , INTS) => INTS) requires I =/=Int 0
    rule #rlpEncodeInts(BUF => BUF +String #rlpEncodeString("\x00"), (0, INTS) => INTS)
    rule #rlpEncodeInts(BUF, .Ints) => #rlpEncodeLength(StringBuffer2String(BUF), 192)

    syntax String ::= #rlpEncodeLength ( String , Int )          [function]
                    | #rlpEncodeLength ( String , Int , String ) [function, klabel(#rlpEncodeLengthAux)]
 // ----------------------------------------------------------------------------------------------------
    rule #rlpEncodeLength(STR, OFFSET) => chrChar(lengthString(STR) +Int OFFSET) +String STR requires lengthString(STR) <Int 56
    rule #rlpEncodeLength(STR, OFFSET) => #rlpEncodeLength(STR, OFFSET, Bytes2String(Int2Bytes(lengthString(STR), BE, Unsigned))) requires lengthString(STR) >=Int 56
    rule #rlpEncodeLength(STR, OFFSET, BL) => chrChar(lengthString(BL) +Int OFFSET +Int 55) +String BL +String STR
```

Decoding
--------

-   `#loadLen` and `#loadOffset` decode a `Bytes` into a single string in an RLP-like encoding which does not allow lists in its structure.
-   `#rlpDecode` RLP decodes a single `String` into a `JSON`.
-   `#rlpDecodeList` RLP decodes a single `String` into a `JSONs`, interpereting the string as the RLP encoding of a list.

```k
    syntax LengthPrefixType ::= "#str" | "#list"
    syntax LengthPrefix ::= LengthPrefixType "(" Int "," Int ")"
 // ------------------------------------------------------------

    syntax Int ::= #loadLen ( Bytes ) [function]
 // --------------------------------------------
    rule #loadLen ( WS ) => 1                                                           requires WS[0]  <Int 128 orBool  WS[0] >=Int 192
    rule #loadLen ( WS ) => WS[0] -Int 128                                              requires WS[0] >=Int 128 andBool WS[0]  <Int 184
    rule #loadLen ( WS ) => Bytes2Int(substrBytes(WS, 0, WS[0] -Int 183), BE, Unsigned) requires WS[0] >=Int 184 andBool WS[0]  <Int 192

    syntax Int ::= #loadOffset ( Bytes ) [function]
 // -----------------------------------------------
    rule #loadOffset ( WS ) => 0              requires WS[0]  <Int 128 orBool  WS[0] >=Int 192
    rule #loadOffset ( WS ) => 1              requires WS[0] >=Int 128 andBool WS[0]  <Int 184
    rule #loadOffset ( WS ) => WS[0] -Int 182 requires WS[0] >=Int 184 andBool WS[0]  <Int 192

    syntax JSON ::= #rlpDecode(String)               [function, klabel(rlpDecode), symbol]
                  | #rlpDecode(String, LengthPrefix) [function, klabel(#rlpDecodeAux)]
 // ----------------------------------------------------------------------------------
    rule #rlpDecode(STR) => #rlpDecode(STR, #decodeLengthPrefix(STR, 0))
    rule #rlpDecode(STR, #str(LEN, POS))  => substrString(STR, POS, POS +Int LEN)
    rule #rlpDecode(STR, #list(LEN, POS)) => [#rlpDecodeList(STR, POS)]

    syntax JSONs ::= #rlpDecodeList(String, Int)               [function]
                      | #rlpDecodeList(String, Int, LengthPrefix) [function, klabel(#rlpDecodeListAux)]
 // ---------------------------------------------------------------------------------------------------
    rule #rlpDecodeList(STR, POS) => #rlpDecodeList(STR, POS, #decodeLengthPrefix(STR, POS)) requires POS <Int lengthString(STR)
    rule #rlpDecodeList(STR, POS) => .JSONs [owise]
    rule #rlpDecodeList(STR, POS, _:LengthPrefixType(L, P)) => #rlpDecode(substrString(STR, POS, L +Int P)) , #rlpDecodeList(STR, L +Int P)

    syntax LengthPrefixType ::= "#str" | "#list"
    syntax LengthPrefix ::= LengthPrefixType "(" Int "," Int ")"
                          | #decodeLengthPrefix ( String , Int )                                [function]
                          | #decodeLengthPrefix ( String , Int , Int )                          [function, klabel(#decodeLengthPrefixAux)]
                          | #decodeLengthPrefixLength ( LengthPrefixType , String , Int , Int ) [function]
                          | #decodeLengthPrefixLength ( LengthPrefixType , Int    , Int , Int ) [function, klabel(#decodeLengthPrefixLengthAux)]
 // --------------------------------------------------------------------------------------------------------------------------------------------
    rule #decodeLengthPrefix(STR, START) => #decodeLengthPrefix(STR, START, ordChar(substrString(STR, START, START +Int 1)))

    rule #decodeLengthPrefix(STR, START, B0) => #str(1, START)                                   requires B0 <Int 128
    rule #decodeLengthPrefix(STR, START, B0) => #str(B0 -Int 128, START +Int 1)                  requires B0 >=Int 128 andBool B0 <Int (128 +Int 56)
    rule #decodeLengthPrefix(STR, START, B0) => #decodeLengthPrefixLength(#str, STR, START, B0)  requires B0 >=Int (128 +Int 56) andBool B0 <Int 192
    rule #decodeLengthPrefix(STR, START, B0) => #list(B0 -Int 192, START +Int 1)                 requires B0 >=Int 192 andBool B0 <Int 192 +Int 56
    rule #decodeLengthPrefix(STR, START, B0) => #decodeLengthPrefixLength(#list, STR, START, B0) [owise]

    rule #decodeLengthPrefixLength(#str,  STR, START, B0) => #decodeLengthPrefixLength(#str,  START, B0 -Int 128 -Int 56 +Int 1, Bytes2Int(String2Bytes(substrString(STR, START +Int 1, START +Int 1 +Int (B0 -Int 128 -Int 56 +Int 1))), BE, Unsigned))
    rule #decodeLengthPrefixLength(#list, STR, START, B0) => #decodeLengthPrefixLength(#list, START, B0 -Int 192 -Int 56 +Int 1, Bytes2Int(String2Bytes(substrString(STR, START +Int 1, START +Int 1 +Int (B0 -Int 192 -Int 56 +Int 1))), BE, Unsigned))
    rule #decodeLengthPrefixLength(TYPE, START, LL, L) => TYPE(L, START +Int 1 +Int LL)

endmodule
```

