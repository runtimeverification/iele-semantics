IELE Gas Calculation
====================

The gas calculation is designed to mirror the style of the yellowpaper.
Gas is consumed either by increasing the amount of memory being used, or by executing opcodes.

Memory Consumption
------------------

The current choices in IELE are similar to those for EVM:

* each contract has a default amount of memory within which to execute;
* memory over the limit incurs a quadratic allocation cost
* memory is charged only at allocation time

However, IELE allows registers and memory cells to refer to these arbitrarily
large numbers.  Moreover, the amount of memory/storage required for
representing the value of a register or memory/storage cell, instead of being
fixed at 256 bits, now varies during its usage.  This was previously only
considered for permanent storage (resetting a stored value to 0 would generate
a refund).

Memory consumed is tracked to determine the appropriate amount of gas to charge
for each operation.  As noted above, unlike EVM, the amount of used memory in
IELE can decrease when memory cells are deallocated or resized.

-   `#memory` computes the memory variation given the next operator (with its arguments).
-   `#registerDelta` computes the memory variation introduced by resizing the return
    register to fit the value computed by the next operator.

Note that the values returned by the above functions could be negative.

```{.k .uiuck .rvk}
module IELE-GAS
    imports IELE-DATA
    imports IELE-CONFIGURATION
    imports IELE-COMMON
    imports IELE-INFRASTRUCTURE
    imports IELE-PRECOMPILED
```

-   `#gas` calculates how much gas this operation costs, and takes into account the memory consumed.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#deductGas"
 // ----------------------------------
    rule <k> #gas [ OP ] => #memory [ OP ] ~> #compute [ OP , SCHED ] ~> #deductGas ... </k> <schedule> SCHED </schedule>

    rule <k> G:Int ~> #deductGas => #exception OUT_OF_GAS ... </k> <gas> GAVAIL                  </gas> requires GAVAIL <Int G
    rule <k> G:Int ~> #deductGas => .                     ... </k> <gas> GAVAIL => GAVAIL -Int G </gas> <previousGas> _ => GAVAIL </previousGas> requires GAVAIL >=Int G

    syntax InternalOp ::= "#memory" "[" Instruction "]"
 // ---------------------------------------------------
```

### Expressions

The size of the result register `REG` for an arithmetic operation is estimated
as follows:

#### Bitwise arithmetic

-   `REG = not W` size of bitwise negation of W is the same as that of W.
-   `REG = and W0, W1` Size of result is the minimum of the sizes of W0 and W1,
    because bitwise and-ing with 0 yields 0
-   `REG = or W0, W1` and `REG = xor W0, W1`
    size of the result is the maximum of the sizes of W0 and W1

```{.k .uiuck .rvk}
    rule #memory [ REG = not W       ] => #registerDelta(REG, intSize(W))
    rule #memory [ REG = and W0 , W1 ] => #registerDelta(REG, minInt(intSize(W0), intSize(W1)))
    rule #memory [ REG = or  W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)))
    rule #memory [ REG = xor W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)))
```

#### Comparison operators

Since the result is boolean, the result size for all comparison operations is 1.

```{.k .uiuck .rvk}
    rule #memory [ REG = iszero _     ] => #registerDelta(REG, 1)
    rule #memory [ REG = cmp _  _ , _ ] => #registerDelta(REG, 1)
```

#### Regular arithmetic

-   `REG = add W0, W1` and `REG = sub W0, W1`
    the result can require at most one more memory word than the maximum of the
    sizes of W0 and W1.
-   `REG = mul W0, W1` the size of the result of multiplication can be at most
    the sum of the sizes of W0 and W1.
-   `REG = div W0, W1` the result of division can require 1 word more than the
    difference between the sizes of W0 and W1.
-   `REG = mod W0, W1` size of the result is at most the minimum of the sizes
    of W0 and W1.

```{.k .uiuck .rvk}
    rule #memory [ REG = add W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)) +Int 1)
    rule #memory [ REG = sub W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)) +Int 1)
    rule #memory [ REG = mul W0 , W1 ] => #registerDelta(REG, intSize(W0) +Int intSize(W1))
    rule #memory [ REG = div W0 , W1 ] => #registerDelta(REG, maxInt(1, intSize(W0) -Int intSize(W1) +Int 1))
    rule #memory [ REG = mod W0 , W1 ] => #registerDelta(REG, minInt(intSize(W0), intSize(W1)))
    rule #memory [ REG = exp W0 , W1 ] => #registerDelta(REG, intSize(W0) *Int maxInt(1, W1))
```

#### Modular arithmetic

For all modular arithmetic operations, the size of the result is at most that
of the modulo operand (W2).

```{.k .uiuck .rvk}
    rule #memory [ REG = addmod _  , _  , W2 ] => #registerDelta(REG, intSize(W2))
    rule #memory [ REG = mulmod W0 , W1 , W2 ] => #registerDelta(REG, intSize(W2))
    rule #memory [ REG = expmod _  , _  , W2 ] => #registerDelta(REG, intSize(W2))
```

#### SHA3

Result size of SHA3 is 256 bits, i.e., 4 words.

```{.k .uiuck .rvk}
    rule #memory [ REG = sha3 _ ] => #registerDelta(REG, 4)
```

#### Byte access

-   `REG = byte INDEX, W`  the result size is one byte, fitting in one word
-   `REG = sext WIDTH , W` and `REG = twos WIDTH , W`
    the result size is WIDTH bytes, i.e., WIDTH / 8 words.

```{.k .uiuck .rvk}
    rule #memory [ REG = byte INDEX , _ ] => #registerDelta(REG, 1)
    rule #memory [ REG = sext WIDTH , _ ] => #registerDelta(REG, WIDTH up/Int 8)
    rule #memory [ REG = twos WIDTH , _ ] => #registerDelta(REG, WIDTH up/Int 8)
```

#### Local state operations

Operations whose result should fit into a word.

```{.k .uiuck .rvk}
    rule #memory [ REG = call @iele.gas         ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.gasprice    ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.gaslimit    ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.number      ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.msize       ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.codesize    ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.extcodesize ( _     ) ] => #registerDelta(REG, 1)
```

Operations whose result is an address:

```{.k .uiuck .rvk}
    rule #memory [ REG = call @iele.beneficiary ( .Ints ) ] => #registerDelta(REG, 3)
    rule #memory [ REG = call @iele.address     ( .Ints ) ] => #registerDelta(REG, 3)
    rule #memory [ REG = call @iele.origin      ( .Ints ) ] => #registerDelta(REG, 3)
    rule #memory [ REG = call @iele.caller      ( .Ints ) ] => #registerDelta(REG, 3)
```

Operations whose result should fit into 256 bits.

```{.k .uiuck .rvk}
    rule #memory [ REG = call @iele.timestamp   ( .Ints ) ] => #registerDelta(REG, 4)
    rule #memory [ REG = call @iele.difficulty  ( .Ints ) ] => #registerDelta(REG, 4)
    rule #memory [ REG = call @iele.callvalue   ( .Ints ) ] => #registerDelta(REG, 4)
    rule #memory [ REG = call @iele.blockhash   ( _     ) ] => #registerDelta(REG, 4)
    rule #memory [ REG = call @iele.balance     ( _     ) ] => #registerDelta(REG, 4)
```

```{.k .uiuck .rvk}
    rule <k> #memory [ REGS = call @ NAME ( ARGS ) ] => #memoryDelta(REGISTERS -Int #sizeRegs(ARGS) +Int intSizes(ARGS) +Int Gcallmemory < SCHED >) ... </k>
         <schedule> SCHED </schedule>
         <funcId> NAME </funcId>
         <nregs> REGISTERS </nregs>
```

#### Memory operations

- `REG = load INDEX1 , INDEX2 , WIDTH`
  In addition to resizing the return register to fit the loaded data,
  the memory needs to potentially be extended to include the entire segment
  being loaded.
- `REG = store INDEX1 , INDEX2 , WIDTH`
  the memory needs to potentially be extended to include the entire segment
  being loaded.
- `REG = load INDEX`
  the size of the register needs to be resized to fit the size of the value
  at the INDEX in memory
- `REG = store VALUE, INDEX`
  the memory cell at INDEX needs to be resized to store VALUE

```{.k .uiuck .rvk}
    rule #memory [ REG = load INDEX1 , INDEX2 , WIDTH ] => #registerDelta(REG, chop(WIDTH) up/Int 8) ~> #memoryExpand(INDEX1, (chop(INDEX2) +Int chop(WIDTH)) up/Int 8) requires chop(WIDTH) >Int 0
    rule #memory [ REG = load INDEX1 , INDEX2 , 0 ] => .K
    rule #memory [ store _ ,  INDEX1 , INDEX2 , WIDTH ] => #memoryExpand(INDEX1, (chop(INDEX2) +Int chop(WIDTH)) up/Int 8) requires chop(WIDTH) >Int 0
    rule #memory [ store _ ,  INDEX1 , INDEX2 , 0 ] => .K

    rule <k> #memory [ REG = load INDEX ] => #registerDelta(REG, #sizeWordStack({LM [ INDEX]}:>WordStack) up/Int 8) ... </k>
         <localMem> LM </localMem>
    rule #memory [ store VALUE ,  INDEX ] => #memoryDelta(INDEX, intSize(VALUE))
```

```{.k .uiuck .rvk}
    rule <k> #memory [ DEST = % SRC:Int ] => #registerDelta(DEST, intSize({REGS [ SRC ]}:>Int)) ... </k>
         <regs> REGS </regs>
    rule <k> #memory [ DEST = SRC:Int ] => #registerDelta(DEST, intSize(SRC)) ... </k>
```

```{.k .uiuck .rvk}
    rule <k> #memory [ REG = sload INDEX ] => #registerDelta(REG, intSize(#lookup(STORAGE, INDEX))) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account>
```

```{.k .uiuck .rvk}
    rule #memory [ br _           ] => .
    rule #memory [ br _ , _       ] => .
    rule #memory [ revert _       ] => .
    rule #memory [ log _          ] => .
    rule #memory [ log _ , _      ] => .
    rule #memory [ sstore _ , _   ] => .
    rule #memory [ selfdestruct _ ] => .

    rule <k> #memory [ ret _          ] => . ... </k> <localCalls> .List </localCalls>
```

```{.k .uiuck .rvk}
    rule #memory [ _ = call _ at _ ( _ ) send _ , gaslimit _ ] => .
    rule #memory [ _ = staticcall _ at _ ( _ ) gaslimit _ ] => .
    rule #memory [ _ , _ = create _ ( _ ) send _ ] => .
    rule #memory [ _ , _ = copycreate _ ( _ ) send _ ] => .
```

```{.k .uiuck .rvk}
    rule #memory [ ECREC ] => .
    rule #memory [ SHA256 ] => .
    rule #memory [ RIP160 ] => .
    rule #memory [ ID ] => .
    rule #memory [ ECADD ] => .
    rule #memory [ ECMUL ] => .
    rule #memory [ ECPAIRING ] => .
```

As the current amount of allocated memory can also decrease in IELE,
memory costs are computed w.r.t. the peak level of allocated memory.
Therefore, the configuration also contains a cell for the peak memory level,
which is maintained by the next rules.

For `#registerDelta`, when trying to store in a register `REG` a value of
size `NEWSIZE`, the current memory level decreases by the current size of
`REG` and increases by `NEWSIZE`.

```{.k .uiuck .rvk}
    syntax InternalOp ::= #registerDelta ( LValue , Int )
                        | #registerDelta ( LValues , Ints )
                        | #memoryExpand  ( Int , Int )
                        | #memoryDelta   ( Int , Int )
                        | #memoryDelta   ( Int ) [klabel(memoryDirectDelta)]
 // ------------------------------------------------------------------------
    rule <k> #registerDelta(% REG, NEWSIZE) => #deductMemory(PEAK) ... </k>
         <currentMemory> CURR </currentMemory>
         <regs> REGS </regs>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int NEWSIZE -Int intSize({REGS [ REG ]}:>Int)) </peakMemory>

    rule #registerDelta(REG, REGS, INT, INTS) => #registerDelta(REG, intSize(INT)) ~> #registerDelta(REGS, INTS)
    rule #registerDelta(.LValues, _) => .K
    rule #registerDelta(_, .Ints) => .K

    rule <k> #memoryExpand(INDEX, NEWSIZE) => #deductMemory(PEAK) ... </k>
         <localMem> LM </localMem>
         <currentMemory> CURR => CURR +Int maxInt(0, NEWSIZE -Int ((#sizeWordStack({LM [ INDEX ]}:>WordStack)) up/Int 8)) </currentMemory>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int maxInt(0, NEWSIZE -Int ((#sizeWordStack({LM [ INDEX ]}:>WordStack)) up/Int 8))) </peakMemory>

    rule <k> #memoryDelta(INDEX, NEWSIZE) => #deductMemory(PEAK) ... </k>
         <localMem> LM </localMem>
         <currentMemory> CURR => CURR +Int NEWSIZE -Int ((#sizeWordStack({LM [ INDEX ]}:>WordStack)) up/Int 8) </currentMemory>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int NEWSIZE -Int ((#sizeWordStack({LM [ INDEX ]}:>WordStack)) up/Int 8)) </peakMemory>

    rule <k> #memoryDelta(DELTA) => #deductMemory(PEAK) ... </k>
         <currentMemory> CURR => CURR +Int DELTA </currentMemory>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int DELTA) </peakMemory>

    syntax InternalOp ::= #deductMemory ( Int )
 // -------------------------------------------
    rule <k> #deductMemory(OLDPEAK) => Cmem(SCHED, NEWPEAK) -Int Cmem(SCHED, OLDPEAK) ~> #deductGas ... </k>
         <schedule> SCHED </schedule>
         <peakMemory> NEWPEAK </peakMemory>

    syntax Int ::= Cmem ( Schedule , Int )       [function, memo]
                 | Cpricedmem ( Schedule, Int )  [function]
 // -------------------------------------------------------
    rule Cmem(SCHED, N)  => (Cpricedmem(SCHED, N) *Int Gmemory < SCHED >) +Int ((Cpricedmem(SCHED, N) *Int Cpricedmem(SCHED, N)) /Int Gquadcoeff < SCHED >)
    rule Cpricedmem(SCHED, N) => maxInt(0, N -Int Smemallowance < SCHED > )
```

Execution Gas
-------------

Each opcode has an intrinsic gas cost of execution as well.

-   `#compute` loads all the relevant surronding state and uses that to compute the intrinsic execution gas of each opcode.

Note that, unlike EVM, operations need to take into account the size of the operands.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#compute" "[" Instruction "," Schedule "]"
 // -----------------------------------------------------------------
    rule #compute [ _ = not W,       SCHED ] => Gnot < SCHED > +Int intSize(W) *Int Gnotword < SCHED >
    rule #compute [ _ = and W0 , W1, SCHED ] => Gbitwise < SCHED > +Int minInt(intSize(W0), intSize(W1)) *Int Gbitwiseword < SCHED >
    rule #compute [ _ = or  W0 , W1, SCHED ] => Gbitwise < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gbitwiseword < SCHED >
    rule #compute [ _ = xor W0 , W1, SCHED ] => Gbitwise < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gbitwiseword < SCHED >

    rule #compute [ _ = iszero W,      SCHED ] => Giszero < SCHED >
    rule #compute [ _ = cmp _ W0 , W1, SCHED ] => Gcmp < SCHED > +Int minInt(intSize(W0), intSize(W1)) *Int Gcmpword < SCHED >

    rule #compute [ _ = add W0 , W1, SCHED ] => Gadd < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gaddword < SCHED >
    rule #compute [ _ = sub W0 , W1, SCHED ] => Gadd < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gaddword < SCHED >
    rule #compute [ _ = mul W0 , W1, SCHED ] => Gmul < SCHED > +Int Cmul(SCHED, intSize(W0), intSize(W1))
    rule #compute [ _ = div W0 , W1, SCHED ] => Gdiv < SCHED > +Int Cdiv(SCHED, intSize(W0), intSize(W1))
    rule #compute [ _ = mod W0 , W1, SCHED ] => Gdiv < SCHED > +Int Cdiv(SCHED, intSize(W0), intSize(W1))
    rule #compute [ _ = exp W0 , W1, SCHED ] => Cexp(SCHED, intSize(W0), W1)

    rule #compute [ _ = addmod W0 , W1 , W2, SCHED ] => Gadd < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gaddword < SCHED > +Int Cdiv(SCHED, maxInt(intSize(W0), intSize(W1)) +Int 1, intSize(W2))
    rule #compute [ _ = mulmod W0 , W1 , W2, SCHED ] => Cdiv(SCHED, intSize(W0), intSize(W2)) +Int Cdiv(SCHED, intSize(W1), intSize(W2)) +Int Cmul(SCHED, minInt(intSize(W0), intSize(W2)), minInt(intSize(W1), intSize(W2))) +Int Cdiv(SCHED, intSize(W2) *Int 2, intSize(W2)) +Int Gmulmod < SCHED >
    rule #compute [ _ = expmod W0 , W1 , W2, SCHED ] => Cexpmod(SCHED, intSize(W0), intSize(W1), intSize(W2))

    rule <k> #compute [ _ = sha3 W0, SCHED ] => Gsha3 < SCHED > +Int (#sizeWordStack({LM [ W0 ]}:>WordStack) up/Int 8) *Int Gsha3word < SCHED > ... </k>
         <localMem> LM </localMem>

    rule #compute [ _ = byte _ , _, SCHED ] => Gbyte < SCHED >
    rule #compute [ _ = twos WIDTH, _, SCHED ] => Gsign < SCHED > +Int (maxInt(1, WIDTH) up/Int 8) *Int Gsignword < SCHED >
    rule #compute [ _ = sext WIDTH, _, SCHED ] => Gsign < SCHED > +Int (maxInt(1, WIDTH) up/Int 8) *Int Gsignword < SCHED >

    rule #compute [ _ = call @iele.gas         ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.gasprice    ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.gaslimit    ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.number      ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.msize       ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.codesize    ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.beneficiary ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.address     ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.origin      ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.caller      ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.timestamp   ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.difficulty  ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.callvalue   ( _ ), SCHED ] => Greadstate < SCHED >
    rule #compute [ _ = call @iele.blockhash   ( _ ), SCHED ] => Gblockhash < SCHED >

    rule #compute [ br _, SCHED ] => Gbr < SCHED >
    rule #compute [ br _ , _, SCHED ] => Gbrcond < SCHED >

    rule <k> #compute [ _ = call @ NAME ( ARGS ), SCHED ] => Gcallreg < SCHED > *Int REGISTERS +Int intSizes(ARGS) *Int Gcopy < SCHED > +Int Glocalcall < SCHED > ... </k>
         <funcId> NAME </funcId>
         <nregs> REGISTERS </nregs>

    rule <k> #compute [ ret ARGS, SCHED ] => Gmove < SCHED > *Int #sizeRegs(ARGS) +Int Gret < SCHED > ... </k>
         <localCalls> ListItem(_) ... </localCalls>
    rule <k> #compute [ ret _, SCHED ] => 0 ... </k>
         <localCalls> .List </localCalls>

    rule #compute [ revert _, SCHED ] => 0

    rule <k> #compute [ _, RETS::LValues = call _ at ACCTTO ( ARGS ) send VALUE , gaslimit GCAP, SCHED ] => Ccall(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, VALUE, #sizeLVals(RETS), intSizes(ARGS)) ... </k>
         <gas> GAVAIL </gas>
         <activeAccounts> ACCTS </activeAccounts>

    rule <k> #compute [ _, RETS::LValues = staticcall _ at ACCTTO ( ARGS ) gaslimit GCAP, SCHED ] => Ccall(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, 0, #sizeLVals(RETS), intSizes(ARGS)) ... </k>
         <gas> GAVAIL </gas>
         <activeAccounts> ACCTS </activeAccounts>

    rule <k> #compute [ log IDX, SCHED ]                                 => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (0 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int, SCHED ]                         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (1 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int , _:Int, SCHED ]                 => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (2 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int , _:Int , _:Int, SCHED ]         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (3 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int , _:Int , _:Int,  _:Int, SCHED ] => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (3 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>

    rule <k> #compute [ _ = load INDEX, SCHED ] => Gloadcell < SCHED > +Int #sizeWordStack({LM [ INDEX ]}:>WordStack) *Int Gloadword < SCHED > ... </k>
         <localMem> LM </localMem>

    rule #compute [ _ = load INDEX , OFFSET , WIDTH, SCHED ] => Gload < SCHED > +Int WIDTH *Int Gloadword < SCHED >

    rule #compute [ store VALUE , INDEX, SCHED ] => Gstorecell < SCHED > +Int intSize(VALUE) *Int Gstoreword < SCHED >
    rule #compute [ store VALUE , INDEX , OFFSET , WIDTH, SCHED ] => Gstore < SCHED > +Int WIDTH *Int Gstoreword < SCHED >

    rule <k> #compute [ DEST = % SRC:Int, SCHED ] => Gcopy < SCHED > *Int intSize({REGS [ SRC ]}:>Int) ... </k>
         <regs> REGS </regs>

    rule #compute [ DEST = SRC:Int, SCHED ] => Gcopy < SCHED > *Int intSize(SRC)

    rule #compute [ _ = call @iele.balance     ( _ ), SCHED ] => Gbalance     < SCHED >
    rule #compute [ _ = call @iele.extcodesize ( _ ), SCHED ] => Gextcodesize < SCHED >

    rule <k> #compute [ _ = sload INDEX, SCHED ] => Gsload < SCHED > +Int Gsloadkey < SCHED > *Int intSize(INDEX) +Int Gsloadword < SCHED > *Int intSize(#lookup(STORAGE, INDEX)) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account>

    rule <k> #compute [ sstore VALUE , INDEX, SCHED ] => Csstore(SCHED, INDEX, VALUE, #lookup(STORAGE, INDEX)) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account>

    rule #compute [ _ , _ = create _ ( ARGS ) send _, SCHED ] => Gcreate < SCHED > +Int Gcopy < SCHED > *Int intSizes(ARGS)
    rule #compute [ _ , _ = copycreate _ ( ARGS ) send _, SCHED ] => Gcopycreate < SCHED > +Int Gcopy < SCHED > *Int intSizes(ARGS)

    rule <k> #compute [ selfdestruct ACCTTO, SCHED ] => Cselfdestruct(SCHED, ACCTTO, ACCTS, BAL) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <id> ACCTFROM </id>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL </balance>
           ...
         </account>

    // Precompiled
    rule <k> #compute [ ECREC, SCHED ]  => 3000 ... </k>
    rule <k> #compute [ SHA256, SCHED ] =>  60 +Int  12 *Int (maxInt(LEN, intSize(DATA)) up/Int 32) ... </k> <callData> LEN , DATA , .Ints </callData>
    rule <k> #compute [ RIP160, SCHED ] => 600 +Int 120 *Int (maxInt(LEN, intSize(DATA)) up/Int 32) ... </k> <callData> LEN , DATA , .Ints </callData>
    rule <k> #compute [ ID, SCHED ]     =>  0 ... </k>

    rule #compute [ ECADD, SCHED ] => 500
    rule #compute [ ECMUL, SCHED ] => 40000
    rule <k> #compute [ ECPAIRING, SCHED ] => 100000 +Int (#sizeRegs(DATA) /Int 6) *Int 80000 ... </k> <callData> DATA </callData>
```

There are several helpers for calculating gas.

Note: These are all functions as the operator `#compute` has already loaded all the relevant state.

```{.k .uiuck .rvk}
    syntax Int ::= Csstore ( Schedule , Int , Int , Int ) [function]
 // ----------------------------------------------------------
    rule Csstore(SCHED, INDEX, VALUE, OLDVALUE) => Gsstore < SCHED > +Int Gsstorekey < SCHED > *Int intSize(INDEX) +Int Gsstoreword < SCHED > *Int intSize(VALUE) +Int #if VALUE =/=Int 0 andBool OLDVALUE ==Int 0 #then Gsstoresetkey < SCHED > *Int intSize(INDEX) +Int Gsstoreset < SCHED > *Int intSize(VALUE) #else maxInt(0, Gsstoreset < SCHED > *Int (intSize(VALUE) -Int intSize(OLDVALUE))) #fi

    syntax Int ::= Ccall    ( Schedule , Int , Map , Int , Int , Int , Int , Int ) [function]
                 | Ccallgas ( Schedule , Int , Map , Int , Int , Int , Int , Int ) [function]
                 | Cgascap  ( Schedule , Int , Int , Int )                         [function]
                 | Cextra   ( Schedule , Int , Map , Int , Int , Int )             [function]
                 | Cxfer    ( Schedule , Int )                                     [function]
                 | Cnew     ( Schedule , Int , Map , Int )                         [function]
                 | Ccallmem ( Schedule , Int , Int )                               [function]
                 | Cdiv     ( Schedule , Int , Int )                               [function]
                 | Cmul     ( Schedule , Int , Int )                               [function]
                 | Cexp     ( Schedule , Int , Int )                               [function]
                 | Cexpmod  ( Schedule , Int , Int , Int )                         [function]
 // -----------------------------------------------------------------------------------------
    rule Ccall(SCHED, ACCT, ACCTS, GCAP, GAVAIL, VALUE, RETS, ARGS) => Cextra(SCHED, ACCT, ACCTS, VALUE, RETS, ARGS) +Int Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ACCT, ACCTS, VALUE, RETS, ARGS))

    rule Ccallgas(SCHED, ACCT, ACCTS, GCAP, GAVAIL, 0, RETS, ARGS)     => Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ACCT, ACCTS,     0, RETS, ARGS))
    rule Ccallgas(SCHED, ACCT, ACCTS, GCAP, GAVAIL, VALUE, RETS, ARGS) => Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ACCT, ACCTS, VALUE, RETS, ARGS)) +Int Gcallstipend < SCHED > requires VALUE =/=K 0

    rule Cgascap(SCHED, GCAP, GAVAIL, GEXTRA) => minInt(#allBut64th(GAVAIL -Int GEXTRA), GCAP) requires GAVAIL >=Int GEXTRA andBool notBool Gstaticcalldepth << SCHED >>
    rule Cgascap(SCHED, GCAP, GAVAIL, GEXTRA) => GCAP                                          requires GAVAIL <Int  GEXTRA orBool Gstaticcalldepth << SCHED >>

    rule Cextra(SCHED, ACCT, ACCTS, VALUE, RETS, ARGS) => Gcall < SCHED > +Int Cnew(SCHED, ACCT, ACCTS, VALUE) +Int Cxfer(SCHED, VALUE) +Int Ccallmem(SCHED, RETS, ARGS)

    rule Cxfer(SCHED, 0) => 0
    rule Cxfer(SCHED, N) => Gcallvalue < SCHED > requires N =/=K 0

    rule Cnew(SCHED, ACCT, ACCTS, VALUE) => Gnewaccount < SCHED >
      requires         #accountNonexistent(SCHED, ACCT, ACCTS) andBool VALUE =/=Int 0
    rule Cnew(SCHED, ACCT, ACCTS, VALUE) => 0
      requires notBool #accountNonexistent(SCHED, ACCT, ACCTS) orBool  VALUE  ==Int 0

    rule Ccallmem(SCHED, RETS, ARGS) => Gmove < SCHED > *Int RETS +Int Gcopy < SCHED > *Int ARGS

    syntax Int ::= Cselfdestruct ( Schedule , Int , Map , Int ) [function]
 // ----------------------------------------------------------------------
    rule Cselfdestruct(SCHED, ACCT, ACCTS, BAL) => Gselfdestruct < SCHED > +Int Gnewaccount < SCHED >
      requires (#accountNonexistent(SCHED, ACCT, ACCTS)) andBool (        Gselfdestructnewaccount << SCHED >>) andBool BAL =/=Int 0
    rule Cselfdestruct(SCHED, ACCT, ACCTS, BAL) => Gselfdestruct < SCHED >
      requires (#accountNonexistent(SCHED, ACCT, ACCTS)) andBool (notBool Gselfdestructnewaccount << SCHED >>  orBool  BAL  ==Int 0)
    rule Cselfdestruct(SCHED, ACCT, ACCTS, BAL) => Gselfdestruct < SCHED >
      requires notBool #accountNonexistent(SCHED, ACCT, ACCTS)

    syntax Bool ::= #accountNonexistent ( Schedule , Int , Map ) [function]
                  | #accountEmpty ( Int , Map )                  [function]
 // -----------------------------------------------------------------------
    rule #accountNonexistent(SCHED, ACCT, ACCTS) => notBool ACCT in_keys(ACCTS) orBool #accountEmpty(ACCT, ACCTS)
    rule #accountEmpty(ACCT, (ACCT |-> EMPTY) _) => EMPTY

    syntax Int ::= #allBut64th ( Int ) [function]
 // ---------------------------------------------
    rule #allBut64th(N) => N -Int (N /Int 64)

    syntax Int ::= G0 ( Schedule , WordStack , Ints , Bool ) [function]
 // ------------------------------------------------------------
    rule G0(SCHED, .WordStack, .Ints, true)  => Gtxcreate    < SCHED >
    rule G0(SCHED, .WordStack, .Ints, false) => Gtransaction < SCHED >

    rule G0(SCHED, (WS => #asSignedBytes(I) ++ WS), (I , INTS => INTS), _)

    rule G0(SCHED, 0 : REST, .Ints, ISCREATE) => Gtxdatazero    < SCHED > +Int G0(SCHED, REST, .Ints, ISCREATE)
    rule G0(SCHED, N : REST, .Ints, ISCREATE) => Gtxdatanonzero < SCHED > +Int G0(SCHED, REST, .Ints, ISCREATE) requires N =/=Int 0

    syntax Int ::= "G*" "(" Int "," Int "," Int ")" [function]
 // ----------------------------------------------------------
    rule G*(GAVAIL, GLIMIT, REFUND) => GAVAIL +Int minInt((GLIMIT -Int GAVAIL)/Int 2, REFUND)
```

Gas Model Parameters
--------------------

The IELE semantics is designed to be extensible in future hard forks while still maintaining an accurate semantics of the language prior
to the fork. As such, we introduce a number of parameters to the gas model which are dependent on the gas schedule used.
Here we introduce only two gas schedules, the "DEFAULT" schedule, provided solely for backwards-compatibility with the EVM VMTests test suite,
and the "ALBE" schedule, representing the initial release of IELE. The name Albe is chosen due to its significance as the name for one of the Romanian Iele.
You can specify which profile is used by passing in the argument `-cSCHEDULE=<FEE_SCHEDULE>` when calling `krun` (the available `<FEE_SCHEDULE>` are supplied here).

A `ScheduleFlag` is a boolean determined by the fee schedule; applying a `ScheduleFlag` to a `Schedule` yields whether the flag is set or not.

```{.k .uiuck .rvk}
    syntax Bool ::= ScheduleFlag "<<" Schedule ">>" [function]
 // ----------------------------------------------------------

    syntax ScheduleFlag ::= "Gselfdestructnewaccount" | "Gstaticcalldepth"
 // ----------------------------------------------------------------------
```

A `ScheduleConst` is a constant determined by the fee schedule; applying a `ScheduleConst` to a `Schedule` yields the correct constant for that schedule.

```{.k .uiuck .rvk}
    syntax Int ::= ScheduleConst "<" Schedule ">" [function]
 // --------------------------------------------------------

    syntax ScheduleConst ::= "Gextcodesize"  | "Gbalance"       | "Gsload"        | "Gadd"         | "Gaddword"    | "Gbitwise"      | "Gbitwiseword"
                           | "Rselfdestruct" | "Gselfdestruct"  | "Gcreate"       | "Gcodedeposit" | "Gcall"       | "Gbr"           | "Gbrcond"
                           | "Gcallvalue"    | "Gcallstipend"   | "Gnewaccount"   | "Gexp"         | "Gmemory"     | "Gtxcreate"
                           | "Gtxdatazero"   | "Gtxdatanonzero" | "Gtransaction"  | "Glog"         | "Glogdata"    | "Glogtopic"     | "Gsha3"
                           | "Gsha3word"     | "Gcopy"          | "Gmove"         | "Gblockhash"   | "Gquadcoeff"  | "Rb"            | "Gdiv"
                           | "Gstoreword"    | "Gstorecell"     | "Gstore"        | "Gsloadword"   | "Gsloadkey"   | "Gsignword"     | "Gsign"
                           | "Gret"          | "Greadstate"     | "Gnot"          | "Gnotword"     | "Gmul"        | "Gmulmod"
                           | "Glocalcall"    | "Gloadword"      | "Gload"         | "Gloadcell"    | "Giszero"     | "Gcmp"          | "Gcmpword"
                           | "Gcallreg"      | "Gcallmemory"    | "Gbyte"         | "Gcopycreate"  | "Gsstore"     | "Gsstorekey"
                           | "Gsstoreset"    | "Gsstoresetkey"  | "Gsstoreword"
                           | "Smemallowance"
 // ---------------------------------------------------------------------------------------------------------------------------------
```

### Default Schedule

```{.k .uiuck .rvk}
    syntax Schedule ::= "DEFAULT"
 // -----------------------------
    rule Gcopy          < DEFAULT > => 3
    rule Gmove          < DEFAULT > => 3
    rule Greadstate     < DEFAULT > => 2
    rule Gadd           < DEFAULT > => 0
    rule Gaddword       < DEFAULT > => 3
    rule Gnot           < DEFAULT > => 0
    rule Gnotword       < DEFAULT > => 3
    rule Gbitwise       < DEFAULT > => 0
    rule Gbitwiseword   < DEFAULT > => 3
    rule Gbyte          < DEFAULT > => 3
    rule Gsign          < DEFAULT > => 0
    rule Gsignword      < DEFAULT > => 5
    rule Giszero        < DEFAULT > => 3
    rule Gcmp           < DEFAULT > => 0
    rule Gcmpword       < DEFAULT > => 3
    rule Gbr            < DEFAULT > => 8
    rule Gbrcond        < DEFAULT > => 10
    rule Gblockhash     < DEFAULT > => 20
    rule Gsha3          < DEFAULT > => 30
    rule Gsha3word      < DEFAULT > => 6
    rule Gbalance       < DEFAULT > => 400
    rule Gextcodesize   < DEFAULT > => 700
    rule Glog           < DEFAULT > => 375
    rule Glogdata       < DEFAULT > => 8
    rule Glogtopic      < DEFAULT > => 375
    rule Gloadcell      < DEFAULT > => 3
    rule Gload          < DEFAULT > => 0
    rule Gloadword      < DEFAULT > => 3
    rule Gstorecell     < DEFAULT > => 3
    rule Gstore         < DEFAULT > => 0
    rule Gstoreword     < DEFAULT > => 3
    rule Gsstore        < DEFAULT > => 1000
    rule Gsstoreword    < DEFAULT > => 500
    rule Gsstorekey     < DEFAULT > => 500
    rule Gsstoreset     < DEFAULT > => 1875
    rule Gsstoresetkey  < DEFAULT > => 1875
    rule Gsload         < DEFAULT > => 50
    rule Gsloadkey      < DEFAULT > => 100
    rule Gsloadword     < DEFAULT > => 50
    rule Gselfdestruct  < DEFAULT > => 0
    rule Gcallmemory    < DEFAULT > => 2
    rule Gcallreg       < DEFAULT > => 3
    rule Glocalcall     < DEFAULT > => 11
    rule Gret           < DEFAULT > => 8
    rule Gcall          < DEFAULT > => 40
    rule Gcallstipend   < DEFAULT > => 2300
    rule Gcallvalue     < DEFAULT > => 9000
    rule Gnewaccount    < DEFAULT > => 25000
    rule Gcreate        < DEFAULT > => 32000
    rule Gcopycreate    < DEFAULT > => 33000
    rule Gcodedeposit   < DEFAULT > => 200
    rule Gmemory        < DEFAULT > => 1
    rule Gquadcoeff     < DEFAULT > => 8192
    rule Gtransaction   < DEFAULT > => 21000
    rule Gtxcreate      < DEFAULT > => 53000
    rule Gtxdatazero    < DEFAULT > => 4
    rule Gtxdatanonzero < DEFAULT > => 68
    rule Rselfdestruct  < DEFAULT > => 24000
    rule Rb             < DEFAULT > => 3 *Int (10 ^Int 18)

    rule Gselfdestructnewaccount << DEFAULT >> => false
    rule Gstaticcalldepth        << DEFAULT >> => true

    rule Smemallowance  < DEFAULT > => 4096
```

### Albe Schedule

```{.k .uiuck .rvk}
    syntax Schedule ::= "ALBE"
 // --------------------------
    rule Gcall         < ALBE > => 700
    rule Gselfdestruct < ALBE > => 5000
    rule SCHEDCONST    < ALBE > => SCHEDCONST < DEFAULT > [owise]

    rule Gselfdestructnewaccount << ALBE >> => true
    rule Gstaticcalldepth        << ALBE >> => false
    rule SCHEDCONST              << ALBE >> => SCHEDCONST << DEFAULT >> [owise]
endmodule
```

