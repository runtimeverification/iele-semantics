IELE Words
=========

IELE uses arbitrary-precision integers, and sometimes also bytes (8 bit words).
Here we provide the arithmetic of these words, as well as some data-structures over them.
Both are implemented using K's `Int`.

```{.k .uiuck .rvk}
requires "krypto.k"
requires "domains.k"

module IELE-DATA
    imports KRYPTO
    imports STRING-BUFFER
    imports ARRAY

    syntax KResult ::= Int
```

Some important numbers that are referred to often during execution:

```{.k .uiuck .rvk}
    syntax Int ::= "pow16"  [function]
                 | "pow30"  [function]
                 | "pow160" [function]
                 | "pow255" [function]
                 | "pow256" [function]
 // ----------------------------------
    rule pow16  => 2 ^Int 16
    rule pow30  => 2 ^Int 30
    rule pow160 => 2 ^Int 160
    rule pow255 => 2 ^Int 255
    rule pow256 => 2 ^Int 256
```

The JSON format is used to encode IELE test cases.
Writing a JSON-ish parser in K takes 6 lines.

```{.k .uiuck .rvk}
    syntax JSONList ::= List{JSON,","}
    syntax JSONKey  ::= String | Int
    syntax JSON     ::= String
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

```{.k .uiuck .rvk}
    syntax Int ::= chop ( Int ) [function]
 // --------------------------------------
    rule chop ( I:Int ) => I modInt pow256 requires I <Int 0  orBool I >=Int pow256
    rule chop ( I:Int ) => I               requires I >=Int 0 andBool I <Int pow256
```

-   `bool2Word` interperets a `Bool` as a `Int`.
-   `word2Bool` interperets a `Int` as a `Bool`.

```{.k .uiuck .rvk}
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

```{.k .uiuck .rvk}
    syntax Account ::= ".Account" | Int
```

Arithmetic
----------

-   `up/Int` performs integer division but rounds up instead of down.

NOTE: Here, we choose to add `I2 -Int 1` to the numerator beforing doing the division to mimic the C++ implementation.
You could alternatively calculate `I1 %Int I2`, then add one to the normal integer division afterward depending on the result.

```{.k .uiuck .rvk}
    syntax Int ::= Int "up/Int" Int [function]
 // ------------------------------------------
    rule I1 up/Int 0  => 0
    rule I1 up/Int 1  => I1
    rule I1 up/Int I2 => (I1 +Int (I2 -Int 1)) /Int I2 requires I2 >Int 1
```

-   `logNInt` returns the log base N (floored) of an integer.

```{.k .uiuck .rvk}
    syntax Int ::= log2Int ( Int ) [function]
 // -----------------------------------------
    rule log2Int(1) => 0
    rule log2Int(W) => 1 +Int log2Int(W >>Int 1) requires W >Int 1

    syntax Int ::= log256Int ( Int ) [function]
 // -------------------------------------------
    rule log256Int(N) => log2Int(N) /Int 8
```

Here we provide simple syntactic sugar over our power-modulus operator.

```{.k .uiuck .rvk}
    syntax Int ::= powmod(Int, Int, Int) [function]
 // -----------------------------------------------
    rule powmod(W0, W1, W2) => W0 ^%Int W1 W2 requires W2 =/=Int 0
    rule powmod(W0, W1, 0) => 0
```

Bitwise Operators
-----------------

-   `bit` gets bit $N$ (0 being LSB).
-   `byte` gets byte $N$ (0 being the LSB).

```{.k .uiuck .rvk}
    syntax Int ::= bit  ( Int , Int ) [function]
                 | byte ( Int , Int ) [function]
 // --------------------------------------------
    rule bit(N, W)  => (W >>Int (N)) %Int 2
    rule byte(N, W) => (W >>Int (N *Int 8)) %Int 256
```

-   `#nBits` shifts in $N$ ones from the right.
-   `#nBytes` shifts in $N$ bytes of ones from the right.
-   `_<<Byte_` shifts an integer 8 bits to the left.

```{.k .uiuck .rvk}
    syntax Int ::= #nBits  ( Int )  [function]
                 | #nBytes ( Int )  [function]
                 | Int "<<Byte" Int [function]
 // ------------------------------------------
    rule #nBits(N)  => (1 <<Int N) -Int 1  requires N >=Int 0
    rule #nBytes(N) => #nBits(N *Int 8)   requires N >=Int 0
    rule N <<Byte M => N <<Int (8 *Int M)
```

-   `signextend(N, W)` sign-extends from byte $N$ of $W$ (0 being LSB).
-   `twos(N, W)` converts a signed integer from byte $N$ of $W$ to twos-complement representation (0 being LSB).

```{.k .uiuck .rvk}
    syntax Int ::= signextend ( Int , Int ) [function]
                 | twos ( Int , Int )       [function]
 // --------------------------------------------------
    rule signextend(N, W) => twos(N +Int 1, W) -Int (1 <<Byte (N +Int 1))  requires         word2Bool(bit((8 *Int (N +Int 1) -Int 1), twos(N +Int 1, W)))
    rule signextend(N, W) => twos(N +Int 1, W)                             requires notBool word2Bool(bit((8 *Int (N +Int 1) -Int 1), twos(N +Int 1, W)))

    rule twos(N, W) => W modInt (1 <<Byte N)
```

-   `keccak` serves as a wrapper around the `Keccak256` in `KRYPTO`.

```{.k .uiuck .rvk}
    syntax Int ::= keccak ( WordStack ) [function]
 // ----------------------------------------------
    rule keccak(WS) => #parseHexWord(Keccak256(#unparseByteStack(WS)))
```

Data Structures
===============

Several data-structures and operations over `Int` are useful to have around.

Word Stack
----------

IELE makes use of a stack in some places in order to represent lists of integers.
The stack and some standard operations over it are provided here.
This stack also serves as a cons-list, so we provide some standard cons-list manipulation tools.

```{.k .uiuck .rvk}
    syntax WordStack [flatPredicate]
    syntax WordStack ::= ".WordStack" | Int ":" WordStack
 // -----------------------------------------------------
```

-   `_++_` acts as `WordStack` append.
-   `#rev` reverses a `WordStack`.
-   `#take(N , WS)` keeps the first $N$ elements of a `WordStack` (passing with zeros as needed).
-   `#drop(N , WS)` removes the first $N$ elements of a `WordStack`.
-   `WS [ N .. W ]` access the range of `WS` beginning with `N` of width `W`.

```{.k .uiuck .rvk}
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
    rule #drop(N, (W : WS))   => #drop(N -Int 1, WS) requires N >Int 0

    syntax WordStack ::= WordStack "[" Int ".." Int "]" [function]
 // --------------------------------------------------------------
    rule WS [ START .. WIDTH ] => #take(chop(WIDTH), #drop(chop(START), WS))
```

-   `WS [ N ]` accesses element $N$ of $WS$.
-   `WS [ N := W ]` sets element $N$ of $WS$ to $W$ (padding with zeros as needed).
-   `WS [ N := WS' ]` sets elements starting at $N$ of $WS$ to $WS'$ (padding with zeros as needed).

```{.k .uiuck .rvk}
    syntax Int ::= WordStack "[" Int "]" [function]
 // -----------------------------------------------
    rule (W0 : WS)   [0] => W0
    rule (.WordStack)[N] => 0            requires N >Int 0
    rule (W0 : WS)   [N] => WS[N -Int 1] requires N >Int 0

    syntax WordStack ::= WordStack "[" Int ":=" Int "]" [function]
 // --------------------------------------------------------------
    rule (W0 : WS)  [ 0 := W::Int ] => W  : WS
    rule .WordStack [ N := W::Int ] => 0  : (.WordStack [ N -Int 1 := W ]) requires N >Int 0
    rule (W0 : WS)  [ N := W::Int ] => W0 : (WS [ N -Int 1 := W ])         requires N >Int 0

    syntax WordStack ::= WordStack "[" Int ":=" WordStack "]" [function, klabel(assignWordStackRange)]
 // --------------------------------------------------------------------------------------------------
    rule WS1 [ N := WS2 ] => #take(N, WS1) ++ WS2 ++ #drop(N +Int #sizeWordStack(WS2), WS1)
```

-   `#sizeWordStack` calculates the size of a `WordStack`.
-   `_in_` determines if a `Int` occurs in a `WordStack`.

```{.k .uiuck .rvk}
    syntax Int ::= #sizeWordStack ( WordStack )       [function, smtlib(sizeWordStack)]
                 | #sizeWordStack ( WordStack , Int ) [function, klabel(sizeWordStackAux), smtlib(sizeWordStackAux)]
 // ----------------------------------------------------------------------------------------------------------------
    rule #sizeWordStack ( WS ) => #sizeWordStack(WS, 0)
    rule #sizeWordStack ( .WordStack, SIZE ) => SIZE
    rule #sizeWordStack ( W : WS, SIZE )     => #sizeWordStack(WS, SIZE +Int 1)

    syntax Bool ::= Int "in" WordStack [function]
 // ---------------------------------------------
    rule W in .WordStack => false
    rule W in (W' : WS)  => (W ==K W') orElseBool (W in WS)
```

-   `#padToWidth(N, WS)` makes sure that a `WordStack` is the correct size.

```{.k .uiuck .rvk}
    syntax WordStack ::= #padToWidth ( Int , WordStack ) [function]
 // ---------------------------------------------------------------
    rule #padToWidth(N, WS) => WS                     requires notBool #sizeWordStack(WS) <Int N
    rule #padToWidth(N, WS) => #padToWidth(N, 0 : WS) requires #sizeWordStack(WS) <Int N
```

Memory
------

-   `.Array` is an arbitrary length array of zeroes.
-   `.Memory` is an arbitrary length array of byte buffers.

We use the impure attribute on the function definitions because the Array sort in a
fast backend is mutable, so we need to ensure we do not cache identical arrays for each time
we call this function.

```{.k .uiuck .rvk}

    syntax Array ::= ".Array" [function, impure]
                   | ".Memory" [function, impure]
 // ---------------------------------------------
    rule .Array => makeArray(pow30, 0)
    rule .Memory => makeArray(pow30, .WordStack)
```

Byte Arrays
-----------

The local memory of execution is a byte-array (instead of a word-array).

-   `#asSigned` will interperet a stack of bytes as a single signed arbitrary-precision integaer (with MSB first).
-   `#asUnsigned` will interperet a stack of bytes as a single unsigned arbitrary-precision integer (with MSB first).
-   `#asAccount` will interpret a stack of bytes as a single account id (with MSB first).
    Differs from `#asUnsigned` only in that an empty stack represents the empty account, not account zero.
-   `#asSignedBytes` will split a single signed integer up into a `WordStack` where each word is a byte wide.
-   `#asUnsignedBytes` will split a single unsigned integer up into a `WordStack` where each word is a byte wide.

```{.k .uiuck .rvk}
    syntax Int ::= #asSigned ( WordStack )       [function]
                 | #asSigned ( WordStack , Int ) [function, klabel(#asSignedAux), smtlib(asSigned)]
 // -----------------------------------------------------------------------------------------------
    rule #asSigned( WS )                => #asSigned(WS, 0)
    rule #asSigned( .WordStack,     _ ) => 0
    rule #asSigned( W : .WordStack, N ) => signextend(N, W)
    rule #asSigned( W0 : W1 : WS,   N ) => #asSigned(((W0 *Int 256) +Int W1) : WS, N +Int 1)

    syntax Int ::= #asUnsigned ( WordStack ) [function]
 // ---------------------------------------------------
    rule #asUnsigned( .WordStack )    => 0
    rule #asUnsigned( W : .WordStack) => W
    rule #asUnsigned( W0 : W1 : WS )  => #asUnsigned(((W0 *Int 256) +Int W1) : WS)

    syntax Account ::= #asAccount ( WordStack ) [function]
 // ------------------------------------------------------
    rule #asAccount( .WordStack ) => .Account
    rule #asAccount( W : WS )     => #asUnsigned(W : WS)

    syntax Int ::= #numBytes ( Int )       [function]
                 | #numBytes ( Int , Int ) [function, klabel(#numBytesAux)]
 // -----------------------------------------------------------------------
    rule #numBytes(W) => #numBytes(W, 0)
    rule #numBytes(0, N) => N
    rule #numBytes(W, N) => #numBytes(W >>Int 8, N +Int 1)

    syntax WordStack ::= #asSignedBytes ( Int )                    [function]
                       | #asSignedBytes ( Int , WordStack , Bool ) [function, klabel(#asSignedBytesAux), smtlib(asSignedBytes)]
 // ---------------------------------------------------------------------------------------------------------------------------
    rule #asSignedBytes( W ) => #asSignedBytes( W ,                                          .WordStack , true  ) requires W >=Int 0
    rule #asSignedBytes( W ) => #asSignedBytes( twos(#numBytes(0 -Int W *Int 2 -Int 1), W) , .WordStack , false ) requires W  <Int 0
    rule #asSignedBytes( 0 , WS         , false ) => WS
    rule #asSignedBytes( 0 , .WordStack , true  ) => .WordStack
    rule #asSignedBytes( 0 , W : WS     , true  ) => W : WS requires W <Int 128
    rule #asSignedBytes( 0 , W : WS     , true  ) => 0 : W : WS requires W >=Int 128
    rule #asSignedBytes( W , WS , POS ) => #asSignedBytes( W /Int 256 , W %Int 256 : WS , POS ) requires W =/=K 0

    syntax WordStack ::= #asUnsignedBytes ( Int )             [function]
                       | #asUnsignedBytes ( Int , WordStack ) [function, klabel(#asUnsignedBytesAux), smtlib(asUnsignedBytes)]
 // --------------------------------------------------------------------------------------------------------------------------
    rule #asUnsignedBytes( W ) => #asUnsignedBytes( W , .WordStack )
    rule #asUnsignedBytes( 0 , WS ) => WS
    rule #asUnsignedBytes( W , WS ) => #asUnsignedBytes( W /Int 256 , W %Int 256 : WS ) requires W =/=K 0

```

Addresses
---------

-   `#addr` turns a IELE arbitrary-precision word into the corresponding IELE address (modulo 2^160).

```{.k .uiuck .rvk}
    syntax Int ::= #addr ( Int ) [function]
 // ---------------------------------------
    rule #addr(W) => W modInt pow160
```

-   `#newAddr` computes the address of a new account given the address and nonce of the creating account.
-   `#sender` computes the sender of the transaction from its data and signature. The format which takes an entire transaction is used only for backwards compatibility with the EVM test suite; the argument which takes an ECDSA signature is used by IELE.

```{.k .uiuck .rvk}
    syntax Int ::= #newAddr ( Int , Int ) [function]
 // ------------------------------------------------
    rule #newAddr(ACCT, NONCE) => #addr(#parseHexWord(Keccak256(#rlpEncodeLength(#rlpEncodeBytes(ACCT, 20) +String #rlpEncodeWord(NONCE), 192))))

    syntax Int ::= #sender ( Int , Int , Int , Account , Int , String , Int , WordStack , WordStack ) [function]
                 | #sender ( String , Int , String , String )                                         [function, klabel(#senderAux)]
                 | #sender ( String )                                                                 [function, klabel(#senderAux2)]
 // ---------------------------------------------------------------------------------------------------------------------------------
    rule #sender(TN, TP, TG, TT, TV, DATA, TW, TR, TS)
      => #sender(#unparseByteStack(#parseHexBytes(Keccak256(#rlpEncodeLength(#rlpEncodeWordStack(TN : TP : TG : .WordStack) +String #rlpEncodeAccount(TT) +String #rlpEncodeWord(TV) +String #rlpEncodeString(DATA), 192)))), TW, #unparseByteStack(TR), #unparseByteStack(TS))

    rule #sender(HT, TW, TR, TS) => #sender(ECDSARecover(HT, TW, TR, TS))

    rule #sender("")  => .Account
    rule #sender(STR) => #addr(#parseHexWord(Keccak256(STR))) requires STR =/=String ""
```

-   `#blockHeaderHash` computes the hash of a block header given all the block data. This is used solely for backwards compatibility with the EVM test suite.

```{.k .uiuck .rvk}
    syntax Int ::= #blockHeaderHash( Int , Int , Int , Int , Int , Int , WordStack , Int , Int , Int , Int , Int , WordStack , Int , Int ) [function]
                 | #blockHeaderHash(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String) [function, klabel(#blockHashHeaderStr)]
 // -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   rule #blockHeaderHash(HP, HO, HC, HR, HT, HE, HB, HD, HI, HL, HG, HS, HX, HM, HN)
         => #blockHeaderHash(#asUnsigned(#parseByteStackRaw(HP)),
                             #asUnsigned(#parseByteStackRaw(HO)),
                             #asUnsigned(#parseByteStackRaw(HC)),
                             #asUnsigned(#parseByteStackRaw(HR)),
                             #asUnsigned(#parseByteStackRaw(HT)),
                             #asUnsigned(#parseByteStackRaw(HE)),
                                     #parseByteStackRaw(HB) ,
                             #asUnsigned(#parseByteStackRaw(HD)),
                             #asUnsigned(#parseByteStackRaw(HI)),
                             #asUnsigned(#parseByteStackRaw(HL)),
                             #asUnsigned(#parseByteStackRaw(HG)),
                             #asUnsigned(#parseByteStackRaw(HS)),
                                     #parseByteStackRaw(HX) ,
                             #asUnsigned(#parseByteStackRaw(HM)),
                             #asUnsigned(#parseByteStackRaw(HN)))

    rule #blockHeaderHash(HP, HO, HC, HR, HT, HE, HB, HD, HI, HL, HG, HS, HX, HM, HN)
         => #parseHexWord(Keccak256(#rlpEncodeLength(         #rlpEncodeBytes(HP, 32)
                                                      +String #rlpEncodeBytes(HO, 32)
                                                      +String #rlpEncodeBytes(HC, 20)
                                                      +String #rlpEncodeBytes(HR, 32)
                                                      +String #rlpEncodeBytes(HT, 32)
                                                      +String #rlpEncodeBytes(HE, 32)
                                                      +String #rlpEncodeString(#unparseByteStack(HB))
                                                      +String #rlpEncodeWordStack(HD : HI : HL : HG : HS : .WordStack)
                                                      +String #rlpEncodeString(#unparseByteStack(HX))
                                                      +String #rlpEncodeBytes(HM, 32)
                                                      +String #rlpEncodeBytes(HN, 8),
                                                    192)))

```

Word Map
--------

Most of IELE data is held in finite arrays.
We are using the polymorphic `Array` sort for these word maps.

-   `WM [ N := WS ]` assigns a contiguous chunk of $WM$ to $WS$ starting at position $W$.
-   `#range(M, START, WIDTH)` reads off $WIDTH$ elements from $WM$ beginning at position $START$ (padding with zeros as needed).

```{.k .uiuck .rvk}
    syntax Array ::= Array "[" Int ":=" WordStack "]" [function]
 // ------------------------------------------------------------
    rule WM::Array[ N := .WordStack ] => WM
    rule WM::Array[ N := W : WS     ] => (WM[chop(N) <- W])[N +Int 1 := WS]

    syntax WordStack ::= #range ( Array , Int , Int )            [function]
    syntax WordStack ::= #range ( Array , Int , Int , WordStack) [function, klabel(#rangeAux)]
 // ------------------------------------------------------------------------------------------
    rule #range(WM, START, WIDTH) => #range(WM, chop(START) +Int chop(WIDTH) -Int 1, chop(WIDTH), .WordStack)

    rule #range(WM, END, 0,     WS) => WS
```
```{.k .uiuck}
    rule #range(WM, END, WIDTH, WS) => #range(WM, END -Int 1, WIDTH -Int 1, WM [ END ]:>Int : WS) requires (WIDTH >Int 0)
```
```{.k .rvk}
    rule #range(WM, END, WIDTH, WS) => #range(WM, END -Int 1, WIDTH -Int 1, {WM [ END ]}:>Int : WS) requires (WIDTH >Int 0)
```

-   `#removeZeros` removes any entries in a map with zero values.

```{.k .uiuck .rvk}
    syntax Map ::= #removeZeros ( Map ) [function]
 // ----------------------------------------------
    rule #removeZeros( .Map )               => .Map
    rule #removeZeros( KEY |-> 0     REST ) => #removeZeros(REST)
    rule #removeZeros( KEY |-> VALUE REST ) => KEY |-> VALUE #removeZeros(REST) requires VALUE =/=K 0
```

-   `#lookup` looks up a key in a map and returns 0 if the key doesn't exist, otherwise returning its value.

```{.k .uiuck .rvk}
    syntax Int ::= #lookup ( Map , Int ) [function]
 // -----------------------------------------------
    rule #lookup( (KEY |-> VAL) M, KEY ) => VAL
    rule #lookup(               M, KEY ) => 0 requires notBool KEY in_keys(M)
```

Parsing/Unparsing
=================

The IELE test-sets are represented in JSON format with hex-encoding of the data and programs.
Here we provide some standard parser/unparser functions for that format.

Parsing
-------

These parsers can interperet hex-encoded strings as `Int`s, `WordStack`s, and `Map`s.

-   `#parseHexWord` interperets a string as a single hex-encoded `Word`.
-   `#parseHexBytes` interperets a string as a hex-encoded stack of bytes.
-   `#parseByteStack` interperets a string as a hex-encoded stack of bytes, but makes sure to remove the leading "0x".
-   `#parseByteStackRaw` inteprets a string as a stack of bytes.
-   `#parseWordStack` interperets a JSON list as a stack of `Word`.
-   `#parseMap` interperets a JSON key/value object as a map from `Word` to `Word`.
-   `#parseAddr` interperets a string as a 160 bit hex-endcoded address.

```{.k .uiuck .rvk}
    syntax Int ::= #parseHexWord ( String ) [function]
                 | #parseWord    ( String ) [function]
 // --------------------------------------------------
    rule #parseHexWord("")   => 0
    rule #parseHexWord("0x") => 0
    rule #parseHexWord(S)    => String2Base(replaceAll(S, "0x", ""), 16) requires (S =/=String "") andBool (S =/=String "0x")

    rule #parseWord("") => 0
    rule #parseWord(S)  => #parseHexWord(S) requires lengthString(S) >=Int 2 andBool substrString(S, 0, 2) ==String "0x"
    rule #parseWord(S)  => String2Int(S) [owise]

    syntax WordStack ::= #parseHexBytes  ( String )    [function]
                       | #parseByteStack ( String )    [function]
                       | #parseByteStackRaw ( String ) [function]
 // -------------------------------------------------------------
    rule #parseByteStack(S) => #parseHexBytes(replaceAll(S, "0x", ""))
    rule #parseHexBytes("") => .WordStack
    rule #parseHexBytes(S)  => #parseHexWord(substrString(S, 0, 2)) : #parseHexBytes(substrString(S, 2, lengthString(S))) requires lengthString(S) >=Int 2

    rule #parseByteStackRaw(S) => ordChar(substrString(S, 0, 1)) : #parseByteStackRaw(substrString(S, 1, lengthString(S))) requires lengthString(S) >=Int 1
    rule #parseByteStackRaw("") => .WordStack

    syntax WordStack ::= #parseWordStack ( JSON ) [function]
 // --------------------------------------------------------
    rule #parseWordStack( [ .JSONList ] )            => .WordStack
    rule #parseWordStack( [ (WORD:String) , REST ] ) => #parseHexWord(WORD) : #parseWordStack( [ REST ] )

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
-   `#padByte` ensures that the `String` interperetation of a `Int` is wide enough.

```{.k .uiuck .rvk}
    syntax String ::= #unparseByteStack ( WordStack )                [function]
                    | #unparseByteStack ( WordStack , StringBuffer ) [function, klabel(#unparseByteStackAux)]
 // ---------------------------------------------------------------------------------------------------------
    rule #unparseByteStack ( WS ) => #unparseByteStack(WS, .StringBuffer)

    rule #unparseByteStack( .WordStack, BUFFER ) => StringBuffer2String(BUFFER)
    rule #unparseByteStack( W : WS, BUFFER )     => #unparseByteStack(WS, BUFFER +String chrChar(W %Int (2 ^Int 8)))

    syntax String ::= #padByte( String ) [function]
 // -----------------------------------------------
    rule #padByte( S ) => S             requires lengthString(S) ==K 2
    rule #padByte( S ) => "0" +String S requires lengthString(S) ==K 1
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
-   `#rlpEncodeWordStack` RLP encodes a list of EVM words.
-   `#rlpEncodeString` RLP encodes a single `String`.
-   `#rlpEncodeAccount` RLP encodes a single account ID.

```{.k .uiuck .rvk}
    syntax String ::= #rlpEncodeWord ( Int )            [function]
                    | #rlpEncodeBytes ( Int , Int )     [function]
                    | #rlpEncodeWordStack ( WordStack ) [function]
                    | #rlpEncodeString ( String )       [function]
                    | #rlpEncodeAccount ( Account )     [function]
 // --------------------------------------------------------------
    rule #rlpEncodeWord(0) => "\x80"
    rule #rlpEncodeWord(WORD) => chrChar(WORD) requires WORD >Int 0 andBool WORD <Int 128
    rule #rlpEncodeWord(WORD) => #rlpEncodeLength(#unparseByteStack(#asUnsignedBytes(WORD)), 128) requires WORD >=Int 128

    rule #rlpEncodeBytes(WORD, LEN) => #rlpEncodeString(#unparseByteStack(#padToWidth(LEN, #asUnsignedBytes(WORD))))

    rule #rlpEncodeWordStack(.WordStack) => ""
    rule #rlpEncodeWordStack(W : WS)     => #rlpEncodeWord(W) +String #rlpEncodeWordStack(WS)

    rule #rlpEncodeString(STR) => STR                        requires lengthString(STR) ==Int 1 andBool ordChar(STR) <Int 128
    rule #rlpEncodeString(STR) => #rlpEncodeLength(STR, 128) [owise]

    rule #rlpEncodeAccount(.Account) => "\x80"
    rule #rlpEncodeAccount(ACCT)     => #rlpEncodeBytes(ACCT, 20) requires ACCT =/=K .Account

    syntax String ::= #rlpEncodeLength ( String , Int )          [function]
                    | #rlpEncodeLength ( String , Int , String ) [function, klabel(#rlpEncodeLengthAux)]
 // ----------------------------------------------------------------------------------------------------
    rule #rlpEncodeLength(STR, OFFSET) => chrChar(lengthString(STR) +Int OFFSET) +String STR requires lengthString(STR) <Int 56
    rule #rlpEncodeLength(STR, OFFSET) => #rlpEncodeLength(STR, OFFSET, #unparseByteStack(#asUnsignedBytes(lengthString(STR)))) requires lengthString(STR) >=Int 56
    rule #rlpEncodeLength(STR, OFFSET, BL) => chrChar(lengthString(BL) +Int OFFSET +Int 55) +String BL +String STR
```

Decoding
--------

-   `#rlpDecode` RLP decodes a single `String` into a `JSON`.
-   `#rlpDecodeList` RLP decodes a single `String` into a `JSONList`, interpereting the string as the RLP encoding of a list.
-   `#loadLen` and `#loadOffset` decode a `WordStack` into a single string in an RLP-like encoding which does not allow lists in its structure.

```{.k .uiuck .rvk}
    syntax JSON ::= #rlpDecode(String)               [function]
                  | #rlpDecode(String, LengthPrefix) [function, klabel(#rlpDecodeAux)]
 // ----------------------------------------------------------------------------------
    rule #rlpDecode(STR) => #rlpDecode(STR, #decodeLengthPrefix(STR, 0))
    rule #rlpDecode(STR, #str(LEN, POS))  => substrString(STR, POS, POS +Int LEN)
    rule #rlpDecode(STR, #list(LEN, POS)) => [#rlpDecodeList(STR, POS)]

    syntax JSONList ::= #rlpDecodeList(String, Int)               [function]
                      | #rlpDecodeList(String, Int, LengthPrefix) [function, klabel(#rlpDecodeListAux)]
 // ---------------------------------------------------------------------------------------------------
    rule #rlpDecodeList(STR, POS) => #rlpDecodeList(STR, POS, #decodeLengthPrefix(STR, POS)) requires POS <Int lengthString(STR)
    rule #rlpDecodeList(STR, POS) => .JSONList [owise]
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

    rule #decodeLengthPrefixLength(#str,  STR, START, B0) => #decodeLengthPrefixLength(#str,  START, B0 -Int 128 -Int 56 +Int 1, #asUnsigned(#parseByteStackRaw(substrString(STR, START +Int 1, START +Int 1 +Int (B0 -Int 128 -Int 56 +Int 1)))))
    rule #decodeLengthPrefixLength(#list, STR, START, B0) => #decodeLengthPrefixLength(#list, START, B0 -Int 192 -Int 56 +Int 1, #asUnsigned(#parseByteStackRaw(substrString(STR, START +Int 1, START +Int 1 +Int (B0 -Int 192 -Int 56 +Int 1)))))
    rule #decodeLengthPrefixLength(TYPE, START, LL, L) => TYPE(L, START +Int 1 +Int LL)

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
