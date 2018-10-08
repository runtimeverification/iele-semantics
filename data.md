IELE Words
=========

IELE uses arbitrary-precision integers, and sometimes also bytes (8 bit words).
Here we provide the arithmetic of these words, as well as some data-structures over them.
Both are implemented using K's `Int`.

```k
requires "krypto.k"
requires "domains.k"

module IELE-DATA
    imports KRYPTO
    imports STRING-BUFFER
    imports ARRAY
    imports BYTES
    imports IELE-COMMON
    imports DOMAINS

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

The JSON format is used to encode IELE test cases.
Writing a JSON-ish parser in K takes 6 lines.

```k
    syntax JSONList ::= List{JSON,","}
    syntax JSONKey  ::= String | Int
    syntax JSON     ::= String | Bool
                      | JSONKey ":" JSON
                      | "{" JSONList "}"
                      | "[" JSONList "]"
 // ------------------------------------
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

    syntax String ::= IeleName2String ( IeleName ) [function, hook(STRING.token2string)]
    syntax IeleName ::= String2IeleName ( String ) [function, hook(STRING.string2token)]
 // ------------------------------------------------------------------------------------
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

    syntax Int ::= intSizes ( Array , Int ) [function, klabel(intSizesArr)]
                 | intSizes ( Array , Int , Int ) [function, klabel(intSizesAux)]
 // -----------------------------------------------------------------------------
    rule intSizes(ARR::Array, I) => intSizes(ARR, I, 0)
    rule intSizes(ARR::Array, I, I) => 0
    rule intSizes(ARR, I, J) => getInt(ARR [ J ]) +Int intSizes(ARR, I, J +Int 1) [owise]

    syntax Int ::= bitsInWords ( Int ) [function]
 // ---------------------------------------------
    rule bitsInWords(I) => I up/Int 256

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

Word Stack
----------

IELE makes use of a stack in some places in order to represent lists of integers.
The stack and some standard operations over it are provided here.
This stack also serves as a cons-list, so we provide some standard cons-list manipulation tools.

```k
    syntax WordStack [flatPredicate]
    syntax WordStack ::= ".WordStack" | Int ":" WordStack
 // -----------------------------------------------------
```

-   `_++_` acts as `WordStack` append.
-   `#rev` reverses a `WordStack`.
-   `#take(N , WS)` keeps the first $N$ elements of a `WordStack` (passing with zeros as needed).
-   `#drop(N , WS)` removes the first $N$ elements of a `WordStack`.
-   `WS [ N .. W ]` access the range of `WS` beginning with `N` of width `W`.

```k
    syntax WordStack ::= WordStack "++" WordStack [function, right]
 // ---------------------------------------------------------------
    rule .WordStack ++ WS' => WS'
    rule (W : WS)   ++ WS' => W : (WS ++ WS')

    syntax WordStack ::= #rev ( WordStack , WordStack ) [function]
 // --------------------------------------------------------------
    rule #rev ( .WordStack , WS ) => WS
    rule #rev ( W : WS1 , WS2 ) => #rev(WS1, W : WS2)

    syntax WordStack ::= #take ( Int , WordStack ) [function]
                       | #take ( Int , WordStack , WordStack ) [function, klabel(#takeAux)]
 // ---------------------------------------------------------------------------------------
    rule #take(N, WS)             => #take(N, WS, .WordStack)
    rule #take(0, _, WS)          => #rev(WS, .WordStack)
    rule #take(N, .WordStack, WS) => #take(N -Int 1, .WordStack, 0 : WS)  requires N >Int 0
    rule #take(N, (W : WS1), WS2) => #take(N -Int 1, WS1,        W : WS2) requires N >Int 0

    syntax WordStack ::= #drop ( Int , WordStack ) [function]
 // ---------------------------------------------------------
    rule #drop(0, WS)         => WS
    rule #drop(N, .WordStack) => .WordStack
    rule #drop(N, (W : WS))   => #drop(N -Int 1, WS) [owise]

    syntax WordStack ::= WordStack "[" Int ".." Int "]" [function]
 // --------------------------------------------------------------
    rule WS::WordStack [ START .. WIDTH ] => #take(chop(WIDTH), #drop(chop(START), WS))
```

-   `WS [ N := WS' ]` sets elements starting at $N$ of $WS$ to $WS'$ (padding with zeros as needed).

```k
    syntax WordStack ::= WordStack "[" Int ":=" WordStack "]" [function, klabel(assignWordStackRange)]
 // --------------------------------------------------------------------------------------------------
    rule WS1::WordStack [ N := WS2::WordStack ] => #take(N, WS1) ++ WS2 ++ #drop(N +Int #sizeWordStack(WS2), WS1)
```

-   `#sizeWordStack` calculates the size of a `WordStack`.
-   `_in_` determines if a `Int` occurs in a `WordStack`.

```k
    syntax Int ::= #sizeWordStack ( WordStack )       [function, smtlib(sizeWordStack)]
                 | #sizeWordStack ( WordStack , Int ) [function, klabel(sizeWordStackAux), smtlib(sizeWordStackAux)]
 // ----------------------------------------------------------------------------------------------------------------
    rule #sizeWordStack ( WS ) => #sizeWordStack(WS, 0)
    rule #sizeWordStack ( .WordStack, SIZE ) => SIZE
    rule #sizeWordStack ( W : WS, SIZE )     => #sizeWordStack(WS, SIZE +Int 1)
```

-   `#padToWidth(N, WS)` makes sure that a `WordStack` is the correct size.

```k
    syntax WordStack ::= #padToWidth ( Int , WordStack ) [function]
 // ---------------------------------------------------------------
    rule #padToWidth(N, WS) => WS                     requires notBool #sizeWordStack(WS) <Int N
    rule #padToWidth(N, WS) => #padToWidth(N, 0 : WS) requires #sizeWordStack(WS) <Int N
```

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

-   `#asUnsigned` will interpret a WordStack as a single unsigned integer (with MSB first).
-   `#asAccount` will interpret a Bytes as a single account id (with MSB first).
    Differs from `Bytes2Int` only in that an empty stack represents the empty account, not account zero.
-   `B [ N .. W ]` access the range of `B` beginning with `N` of width `W` (padding with zeros as needed).
-   `B [ N := B' ]` sets elements starting at $N$ of $B$ to $B'$ (padding with zeros as needed).

```k
    syntax Int ::= #asUnsigned ( WordStack ) [function]
 // ---------------------------------------------------
    rule #asUnsigned( .WordStack )    => 0
    rule #asUnsigned( W : .WordStack) => W
    rule #asUnsigned( W0 : W1 : WS )  => #asUnsigned(((W0 <<Int 8) |Int W1) : WS)

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

These parsers can interperet hex-encoded strings as `Int`s, `WordStack`s, and `Map`s.

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

    syntax WordStack ::= #parseByteStack ( String )    [function]
                       | #parseByteStack ( String , WordStack , Int , Int ) [function, klabel(#parseByteStackAux)]
                       | #parseByteStackRaw ( String ) [function]
                       | #parseByteStackRaw ( String , WordStack , Int , Int ) [function, klabel(#parseByteStackRawAux)]
 // --------------------------------------------------------------------------------------------------------------------
    rule #parseByteStack(S) => #fun(STR => #parseByteStack(STR, .WordStack, 0, lengthString(STR)))(replaceAll(S, "0x", ""))
    rule #parseByteStack(_, WS, LEN, LEN) => #rev(WS, .WordStack)
    rule #parseByteStack(S, WS, I, LEN)  => #parseByteStack(S, #parseHexWord(substrString(S, I, I +Int 2)) : WS, I +Int 2, LEN) [owise]
    rule #parseByteStackRaw(S) => #parseByteStackRaw(S, .WordStack, 0, lengthString(S))
    rule #parseByteStackRaw(S, WS, LEN, LEN) => #rev(WS, .WordStack)
    rule #parseByteStackRaw(S, WS, I, LEN) => #parseByteStackRaw(S, ordChar(substrString(S, I, I +Int 1)) : WS, I +Int 1, LEN) [owise]

    syntax Map ::= #parseMap ( JSON ) [function]
 // --------------------------------------------
    rule #parseMap( { .JSONList                   } ) => .Map
    rule #parseMap( { _   : (VALUE:String) , REST } ) => #parseMap({ REST })                                                requires #parseHexWord(VALUE) ==K 0
    rule #parseMap( { KEY : (VALUE:String) , REST } ) => #parseMap({ REST }) [ #parseHexWord(KEY) <- #parseHexWord(VALUE) ] requires #parseHexWord(VALUE) =/=K 0

    syntax Int ::= #parseAddr ( String ) [function]
 // -----------------------------------------------
    rule #parseAddr(S) => #addr(#parseHexWord(S))
```

Unparsing
---------

We need to interperet a `WordStack` as a `String` again so that we can call `Keccak256` on it from `KRYPTO`.

-   `#unparseByteStack` turns a stack of bytes (as a `WordStack`) into a `String`.

```k
    syntax String ::= #unparseByteStack ( WordStack )                [function, klabel(unparseByteStack)]
                    | #unparseByteStack ( WordStack , StringBuffer ) [function, klabel(#unparseByteStackAux)]
 // ---------------------------------------------------------------------------------------------------------
    rule #unparseByteStack ( WS ) => #unparseByteStack(WS, .StringBuffer)

    rule #unparseByteStack( .WordStack, BUFFER ) => StringBuffer2String(BUFFER)
    rule #unparseByteStack( W : WS, BUFFER )     => #unparseByteStack(WS, BUFFER +String chrChar(W))
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
                    | #rlpEncodeInts ( Ints ) [function]
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

-   `#loadLen` and `#loadOffset` decode a `WordStack` into a single string in an RLP-like encoding which does not allow lists in its structure.

```k
    syntax LengthPrefixType ::= "#str" | "#list"
    syntax LengthPrefix ::= LengthPrefixType "(" Int "," Int ")"
 // ------------------------------------------------------------

    syntax Int ::= #loadLen ( WordStack ) [function]
 // ------------------------------------------------
    rule #loadLen ( B0 : WS ) => 1                               requires B0  <Int 128 orBool  B0 >=Int 192
    rule #loadLen ( B0 : WS ) => B0 -Int 128                     requires B0 >=Int 128 andBool B0  <Int 184
    rule #loadLen ( B0 : WS ) => #asUnsigned(#take(B0 -Int 183, WS)) requires B0 >=Int 184 andBool B0  <Int 192

    syntax Int ::= #loadOffset ( WordStack ) [function]
 // ---------------------------------------------------
    rule #loadOffset ( B0 : WS ) => 0           requires B0  <Int 128 orBool  B0 >=Int 192
    rule #loadOffset ( B0 : WS ) => 1           requires B0 >=Int 128 andBool B0  <Int 184
    rule #loadOffset ( B0 : WS ) => B0 -Int 182 requires B0 >=Int 184 andBool B0  <Int 192
endmodule
```

