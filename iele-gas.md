IELE Gas Calculation
====================

The following document describes the gas model of IELE. Note that this gas model should
be considered a first draft and may be subject to changes before deploying IELE on a live blockchain.
Gas is consumed either by increasing the amount of memory being used, or by the computational effort to execute instructions.

```k
module IELE-GAS
    imports IELE-DATA
    imports IELE-CONFIGURATION
    imports IELE-COMMON
    imports IELE-INFRASTRUCTURE
    imports IELE-PRECOMPILED
```

Overall Gas Calculation
-----------------------

The gas cost of an instruction is the cost incurred from the memory used by the instruction plus the
computational cost of executing the instruction.

-   `#gas` calculates how much gas this operation costs, and takes into account the memory consumed.
-   `#deductGas` deducts a specific integer amount of gas, raising an out of gas exception if insufficient gas remains.

```k
    syntax InternalOp ::= "#deductGas"
 // ----------------------------------
    rule <k> #gas [ OP ] => #memory [ OP ] ~> #compute [ OP , SCHED ] ~> #deductGas ... </k> <schedule> SCHED </schedule>

    rule <k> G:Int ~> #deductGas => #exception OUT_OF_GAS ... </k> <gas> GAVAIL                  </gas> requires GAVAIL <Int G
    rule <k> G:Int ~> #deductGas => .                     ... </k> <gas> GAVAIL => GAVAIL -Int G </gas> <previousGas> _ => GAVAIL </previousGas> requires GAVAIL >=Int G
```

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

```k
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
-   `REG = shift W0, W1`
    size of the result is the size of the variable modified by the shift amount (positive for left shift, negative for right shift)
-   `REG = log2 W` size of logarithm base 2 is equal to at most 8 * the size in bytes of the number,
    which must be less than 2^64 to fit in memory on a 64-bit processor. We can compute this as a linear cost
    on the word size because it is a floored logarithm and can be computed using bit counting.

```k
    rule #memory [ REG = not   W       ] => #registerDelta(REG, intSize(W))
    rule #memory [ REG = and   W0 , W1 ] => #registerDelta(REG, minInt(intSize(W0), intSize(W1)))
    rule #memory [ REG = or    W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)))
    rule #memory [ REG = xor   W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)))
    rule #memory [ REG = shift W0 , W1 ] => #registerDelta(REG, maxInt(1, intSize(W0) +Int bitsInWords(W1)))
    rule #memory [ REG = log2  W       ] => #registerDelta(REG, 2)
```

#### Comparison operators

Since the result is boolean, the result size for all comparison operations is 1.

```k
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
-   `REG = mod W0, W1` the size of the result is at most the minimum of the
    sizes of W0 and W1.
-   `REG = exp W0, W1` the size of the result is equal to the size of the base
    multiplied by the exponent.

```k
    rule #memory [ REG = add W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)) +Int 1)
    rule #memory [ REG = sub W0 , W1 ] => #registerDelta(REG, maxInt(intSize(W0), intSize(W1)) +Int 1)
    rule #memory [ REG = mul W0 , W1 ] => #registerDelta(REG, intSize(W0) +Int intSize(W1))
    rule #memory [ REG = div W0 , W1 ] => #registerDelta(REG, maxInt(1, intSize(W0) -Int intSize(W1) +Int 1))
    rule #memory [ REG = mod W0 , W1 ] => #registerDelta(REG, minInt(intSize(W0), intSize(W1)))
    rule #memory [ REG = exp W0 , W1 ] => #registerDelta(REG, #adjustedBitLength(intSize(W0), W0) *Int W1 /Int 64)
```

#### Modular arithmetic

For all modular arithmetic operations, the size of the result is at most that
of the modulo operand (W2).

```k
    rule #memory [ REG = addmod _  , _  , W2 ] => #registerDelta(REG, intSize(W2))
    rule #memory [ REG = mulmod W0 , W1 , W2 ] => #registerDelta(REG, intSize(W2))
    rule #memory [ REG = expmod _  , _  , W2 ] => #registerDelta(REG, intSize(W2))
```

#### SHA3

Result size of SHA3 is 256 bits, i.e., 4 words.

```k
    rule #memory [ REG = sha3 _ ] => #registerDelta(REG, bitsInWords(256))
```

#### Byte access

-   `REG = byte INDEX, W`  the result size is one byte, fitting in one word
-   `REG = sext WIDTH , W`, `REG = twos WIDTH , W`, and `REG = bswap WIDTH , W`
    the result size is WIDTH bytes, i.e., WIDTH / 8 words.

```k
    rule #memory [ REG = byte  INDEX , _ ] => #registerDelta(REG, bytesInWords(1))
    rule #memory [ REG = sext  WIDTH , _ ] => #registerDelta(REG, bytesInWords(chop(WIDTH)))
    rule #memory [ REG = twos  WIDTH , _ ] => #registerDelta(REG, bytesInWords(chop(WIDTH)))
    rule #memory [ REG = bswap WIDTH , _ ] => #registerDelta(REG, bytesInWords(chop(WIDTH)))
```

#### Local state operations

Operations whose result should fit into a word.

```k
    rule #memory [ REG = call @iele.gas         ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.gasprice    ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.gaslimit    ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.number      ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.msize       ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.codesize    ( .Ints ) ] => #registerDelta(REG, 1)
    rule #memory [ REG = call @iele.extcodesize ( _     ) ] => #registerDelta(REG, 1)
```

Operations whose result is an address:

```k
    rule #memory [ REG = call @iele.beneficiary ( .Ints ) ] => #registerDelta(REG, bytesInWords(20))
    rule #memory [ REG = call @iele.address     ( .Ints ) ] => #registerDelta(REG, bytesInWords(20))
    rule #memory [ REG = call @iele.origin      ( .Ints ) ] => #registerDelta(REG, bytesInWords(20))
    rule #memory [ REG = call @iele.caller      ( .Ints ) ] => #registerDelta(REG, bytesInWords(20))
```

Operations whose result should fit into 256 bits.

```k
    rule #memory [ REG = call @iele.timestamp   ( .Ints ) ] => #registerDelta(REG, bitsInWords(256))
    rule #memory [ REG = call @iele.difficulty  ( .Ints ) ] => #registerDelta(REG, bitsInWords(256))
    rule #memory [ REG = call @iele.callvalue   ( .Ints ) ] => #registerDelta(REG, bitsInWords(256))
    rule #memory [ REG = call @iele.blockhash   ( _     ) ] => #registerDelta(REG, bitsInWords(256))
    rule #memory [ REG = call @iele.balance     ( _     ) ] => #registerDelta(REG, bitsInWords(256))
```

#### Assignment operations

The memory cost of assigning a register or immediate to a register is the cost associated with
resizing the register to equal the value being assigned.

```k
    rule <k> #memory [ DEST = % SRC:Int ] => #registerDelta(DEST, intSize({REGS [ SRC ]}:>Int)) ... </k>
         <regs> REGS </regs>
    rule <k> #memory [ DEST = SRC:Int ] => #registerDelta(DEST, intSize(SRC)) ... </k>
```

### Function Call/Return

The memory cost of a function call is the cost of initializing a new set of registers plus the cost of
saving the return address and other information on the function stack. The latter is a constant.
For the former, each register used by the function call consumes one word by default, except for the parameters
to the function, which consume as much as the size of their arguments.

```k
    rule <k> #memory [ REGS = call @ NAME ( ARGS ) ] => #memoryDelta(REGISTERS -Int #sizeRegs(ARGS) +Int intSizes(ARGS) +Int Gcallmemory < SCHED >) ... </k>
         <schedule> SCHED </schedule>
         <funcId> NAME </funcId>
         <nregs> REGISTERS </nregs>

    rule <k> #memory [ _ = call (IDX:Int => @ FUNC) ( _ ) ] ... </k>
         <funcLabels> ... IDX |-> FUNC ... </funcLabels>
    // this will throw an exception, so the gas cost doesn't really matter
    rule <k> #memory [ _ = call IDX:Int ( _ ) ] => . ... </k>
         <funcLabels> LABELS </funcLabels>
      requires notBool IDX in_keys(LABELS)
```

The memory cost of returning to a function is negative. Instead of charging a gas cost, memory is reclaimed according to the
memory freed by returning from the function. In other words, we free the call frame on the stack, as well as each of the registers
in the callee.

There is also some memory change associated with assigning `ARGS` to the
caller's registers, but that is handled in `iele.md`.

```k
    rule <k> #memory [ ret ARGS ] => #memoryDelta(0 -Int intSizes(REGS, NREGS) -Int Gcallmemory < SCHED >) ... </k>
         <schedule> SCHED </schedule>
         <fid> NAME </fid>
         <regs> REGS </regs>
         <nregs> NREGS </nregs>
```

### Memory operations

- `REG = load INDEX1 , INDEX2 , WIDTH`
  We resize the return register to fit the loaded data
- `REG = store INDEX1 , INDEX2 , WIDTH`
  the memory needs to potentially be extended to include the entire segment
  being stored.
- `REG = load INDEX`
  the size of the register needs to be resized to fit the size of the value
  at the INDEX in memory
- `REG = store VALUE, INDEX`
  the memory cell at INDEX needs to be resized to store VALUE

```k
    rule #memory [ REG = load INDEX1 , INDEX2 , WIDTH ] => #registerDelta(REG, bytesInWords(chop(WIDTH)))
    rule #memory [ store _ ,  INDEX1 , INDEX2 , WIDTH ] => #memoryExpand(INDEX1, bytesInWords(chop(INDEX2) +Int chop(WIDTH))) requires chop(WIDTH) >Int 0
    rule #memory [ store _ ,  INDEX1 , INDEX2 , WIDTH ] => .K requires chop(WIDTH) ==Int 0

    rule <k> #memory [ REG = load INDEX ] => #registerDelta(REG, bytesInWords(#sizeWordStack({LM [ INDEX]}:>WordStack))) ... </k>
         <localMem> LM </localMem>
    rule #memory [ store VALUE ,  INDEX ] => #memoryDelta(INDEX, intSize(VALUE))
```

### Storage

Storage contains arbitrary-precision values, therefore the memory cost of loading a value from storage
is the cost associated with resizing the register to equal the value contained in storage.

```k
    rule <k> #memory [ REG = sload INDEX ] => #registerDelta(REG, intSize(VALUE)) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> ... INDEX |-> VALUE ... </storage>
           ...
         </account>

    rule <k> (.K => #lookupStorage(ACCT, INDEX)) ~> #memory [ _ = sload INDEX ] ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account>
      requires notBool INDEX in_keys(STORAGE)
```

Storing to storage incurs no memory cost (its disk cost is included in its computational cost).

```k
    rule #memory [ sstore _ , _   ] => .
```

### Miscellaneous

The following instructions do not incur any memory costs as they either do not return a value and do not write to memory,
or else their memory cost is paid after the instruction executes.

For example, `revert`, `ret`, `call`, `staticcall`, `create`, and `copycreate` each invoke `#registerDelta` directly
as part of the process of returning from a contract. For information on how these are used, refer to the usages in `iele.md`.

For `revert` there is some memory change associated with changing the caller's
registers, but that is handled in `iele.md`.

```k
    rule #memory [ br _           ] => .
    rule #memory [ br _ , _       ] => .
    rule #memory [ revert _       ] => .
    rule #memory [ log _          ] => .
    rule #memory [ log _ , _      ] => .
    rule #memory [ selfdestruct _ ] => .

    rule <k> #memory [ ret _          ] => . ... </k> <localCalls> .List </localCalls>

    rule #memory [ _ = call _ at _ ( _ ) send _ , gaslimit _ ] => .
    rule #memory [ _ = staticcall _ at _ ( _ ) gaslimit _ ] => .
    rule #memory [ _ , _ = create _ ( _ ) send _ ] => .
    rule #memory [ _ , _ = copycreate _ ( _ ) send _ ] => .

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

-   `#registerDelta` computes the new peak memory usage based on an estimation of the size of the result of the instruction, and incurs gas cost
    if the new peak is greater than the old peak. It does not update `<currentMemory`, which is updated when registers are actually written by
    `#load` in `iele.md`. The delta is the estimated size after the instruction minus the current size before the instruction.

```k
    syntax InternalOp ::= #registerDelta ( LValue , Int )
 // -----------------------------------------------------
    rule <k> #registerDelta(% REG, NEWSIZE) => #deductMemory(PEAK) ... </k>
         <currentMemory> CURR </currentMemory>
         <regs> REGS </regs>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int NEWSIZE -Int intSize({REGS [ REG ]}:>Int)) </peakMemory>
```

-   `#registerDeltas` invokes `#registerDelta` on a sequence of registers and values, using their exact size. This form is invoked when
    a contract returns and the return registers of an inter-contract call instruction are written.

```k
    syntax InternalOp ::= #registerDeltas ( LValues , Ints )
 // --------------------------------------------------------
    rule #registerDeltas(REG, REGS, INT, INTS) => #registerDelta(REG, intSize(INT)) ~> #registerDeltas(REGS, INTS)
    rule #registerDeltas(.LValues, _) => .K
    rule #registerDeltas(_, .Ints) => .K
```

-   `#memoryExpand` updates `<currentMemory>` and `<peakMemory>` and incurs the gas cost if peak memory increases.
    It accepts a memory cell and its new size and computes the delta based on the difference in sizes before and after the instruction.
    However, it does not ever decrease the current memory usage, only expanding it up to the new size if it is less than that size currently.
-   `#memoryDelta` does the same as `#memoryExpand` except that it can also decrease the current memory if the new size is less than the old size.
-   `#memoryDelta` also takes a one argument form which takes an exact delta. This is used by function call/return and in other places where
    a memory delta occurs despite no write to local memory.
-   `#deductMemory` computes the actual gas cost from the old and new peak memory.

```k
    syntax InternalOp ::= #memoryExpand  ( Int , Int )
                        | #memoryDelta   ( Int , Int )
                        | #memoryDelta   ( Int ) [klabel(memoryDirectDelta)]
 // ------------------------------------------------------------------------
    rule <k> #memoryExpand(INDEX, NEWSIZE) => #deductMemory(PEAK) ... </k>
         <localMem> LM </localMem>
         <currentMemory> CURR => CURR +Int maxInt(0, NEWSIZE -Int bytesInWords((#sizeWordStack({LM [ INDEX ]}:>WordStack)))) </currentMemory>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int maxInt(0, NEWSIZE -Int bytesInWords((#sizeWordStack({LM [ INDEX ]}:>WordStack))))) </peakMemory>

    rule <k> #memoryDelta(INDEX, NEWSIZE) => #deductMemory(PEAK) ... </k>
         <localMem> LM </localMem>
         <currentMemory> CURR => CURR +Int NEWSIZE -Int bytesInWords((#sizeWordStack({LM [ INDEX ]}:>WordStack))) </currentMemory>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int NEWSIZE -Int bytesInWords((#sizeWordStack({LM [ INDEX ]}:>WordStack)))) </peakMemory>

    rule <k> #memoryDelta(DELTA) => #deductMemory(PEAK) ... </k>
         <currentMemory> CURR => CURR +Int DELTA </currentMemory>
         <peakMemory> PEAK => maxInt(PEAK, CURR +Int DELTA) </peakMemory>

    syntax InternalOp ::= #deductMemory ( Int )
 // -------------------------------------------
    rule <k> #deductMemory(OLDPEAK) => Cmem(SCHED, NEWPEAK) -Int Cmem(SCHED, OLDPEAK) ~> #deductGas ... </k>
         <schedule> SCHED </schedule>
         <peakMemory> NEWPEAK </peakMemory>
```

-   `Cmem` computes the absolute cost of using N bytes of memory. It scales linearly up to a certain amount and quadratically afterwards.
    A certain amount of memory is also free, as computed by `Cpricedmem`.
-   `Cpricedmem` is the memory that is actually charged, which is the actual memory usage minus the memory allowance, which is an amount of memory
    free in each contract call frame.

```k
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

```k
    syntax InternalOp ::= "#compute" "[" Instruction "," Schedule "]"
 // -----------------------------------------------------------------
```

### Expressions

#### Bitwise arithmetic

The bitwise expressions have a constant cost plus a linear factor in the number of words manipulated.

```k
    rule #compute [ _ = not   W,       SCHED ] => Gnot < SCHED > +Int intSize(W) *Int Gnotword < SCHED >
    rule #compute [ _ = and   W0 , W1, SCHED ] => Gbitwise < SCHED > +Int minInt(intSize(W0), intSize(W1)) *Int Gbitwiseword < SCHED >
    rule #compute [ _ = or    W0 , W1, SCHED ] => Gbitwise < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gbitwiseword < SCHED >
    rule #compute [ _ = xor   W0 , W1, SCHED ] => Gbitwise < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gbitwiseword < SCHED >
    rule #compute [ _ = shift W0 , W1, SCHED ] => Gbitwise < SCHED > +Int maxInt(1, intSize(W0) +Int bitsInWords(W1)) *Int Gbitwiseword < SCHED >
    rule #compute [ _ = log2 W,        SCHED ] => Gbitwise < SCHED > +Int intSize(W) *Int Gbitwiseword < SCHED >
```

#### Comparison operators

`iszero` has a constant cost, whereas `cmp` has a constant cost and a linear factor in the smaller of the two sizes.

```k
    rule #compute [ _ = iszero W,      SCHED ] => Giszero < SCHED >
    rule #compute [ _ = cmp _ W0 , W1, SCHED ] => Gcmp < SCHED > +Int minInt(intSize(W0), intSize(W1)) *Int Gcmpword < SCHED >
```

#### Regular arithmetic

-   `add` and `sub` have a linear cost in the larger of the two sizes.
-   `mul`, `div`, `mod` and `exp` have more complicated costs, which are
    detailed elswhere.

```k
    rule #compute [ _ = add W0 , W1, SCHED ] => Gadd < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gaddword < SCHED >
    rule #compute [ _ = sub W0 , W1, SCHED ] => Gadd < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gaddword < SCHED >
    rule #compute [ _ = mul W0 , W1, SCHED ] => Cmul(SCHED, intSize(W0), intSize(W1))
    rule #compute [ _ = div W0 , W1, SCHED ] => Cdiv(SCHED, intSize(W0), intSize(W1))
    rule #compute [ _ = mod W0 , W1, SCHED ] => Cdiv(SCHED, intSize(W0), intSize(W1))
    rule #compute [ _ = exp W0 , W1, SCHED ] => Cexp(SCHED, intSize(W0), W0, W1)
```

#### Modular arithmetic

-   `addmod` is the cost of an addition plus the cost of the modulus.
-   `mulmod` is the cost of two moduli, plus a multiplication, plus another
    modulus.
-   `expmod` has a more complicated cost and is defined elswhere.

```k
    rule #compute [ _ = addmod W0 , W1 , W2, SCHED ] => Gadd < SCHED > +Int maxInt(intSize(W0), intSize(W1)) *Int Gaddword < SCHED > +Int Cdiv(SCHED, maxInt(intSize(W0), intSize(W1)) +Int 1, intSize(W2))
    rule #compute [ _ = mulmod W0 , W1 , W2, SCHED ] => Cmul(SCHED, intSize(W0), intSize(W1)) +Int Cdiv(SCHED, intSize(W0) +Int intSize(W1), intSize(W2)) +Int Gmulmod < SCHED >
    rule #compute [ _ = expmod W0 , W1 , W2, SCHED ] => Cexpmod(SCHED, intSize(W0), intSize(W1), intSize(W2), W2)
```

#### SHA3

The cost of hashing a memory cell is equal to a constant plus the size of the cell in words.

```k
    rule <k> #compute [ _ = sha3 W0, SCHED ] => Gsha3 < SCHED > +Int bytesInWords(#sizeWordStack({LM [ W0 ]}:>WordStack)) *Int Gsha3word < SCHED > ... </k>
         <localMem> LM </localMem>
```

#### Byte access

-   `byte` has a constant cost.
-   `twos`, `sext`, and `bswap` have a constant cost plus a linear factor in the `WIDTH` parameter.

```k
    rule #compute [ _ = byte _ , _, SCHED ] => Gbyte < SCHED >
    rule #compute [ _ = twos  WIDTH, _, SCHED ] => Gsign < SCHED > +Int maxInt(1, bytesInWords(chop(WIDTH))) *Int Gsignword < SCHED >
    rule #compute [ _ = sext  WIDTH, _, SCHED ] => Gsign < SCHED > +Int maxInt(1, bytesInWords(chop(WIDTH))) *Int Gsignword < SCHED >
    rule #compute [ _ = bswap WIDTH, _, SCHED ] => Gsign < SCHED > +Int maxInt(1, bytesInWords(chop(WIDTH))) *Int Gsignword < SCHED >
```

#### Local state operations

Each of these operations merely reads a constant value from the execution context.

```k
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
```

The blockhash function looks up state in the blockchain, and is therefore more expensive than the other builtin functions.

```k
    rule #compute [ _ = call @iele.blockhash   ( _ ), SCHED ] => Gblockhash < SCHED >
```

#### Network state operations

Each of these operations pays a constant cost to look up information about an account on the network.

```k
    rule #compute [ _ = call @iele.balance     ( _ ), SCHED ] => Gbalance     < SCHED >
    rule #compute [ _ = call @iele.extcodesize ( _ ), SCHED ] => Gextcodesize < SCHED >
```

#### Assignment operations

The cost to load a value into a register is simply the cost to copy its value.

```k
    rule <k> #compute [ DEST = % SRC:Int, SCHED ] => Gcopy < SCHED > *Int intSize({REGS [ SRC ]}:>Int) ... </k>
         <regs> REGS </regs>

    rule #compute [ DEST = SRC:Int, SCHED ] => Gcopy < SCHED > *Int intSize(SRC)
```

### Jump statements

The cost of jumping to a label, both conditionally and unconditionally, is a constant,
but the cost of a conditional jump is slightly higher since it must test the register against zero.

```k
    rule #compute [ br _, SCHED ] => Gbr < SCHED >
    rule #compute [ br _ , _, SCHED ] => Gbrcond < SCHED >
```

### Function Call/Return

The cost of an intra-contract call is the cost to initialize the new set of registers, the cost to copy the arguments to the call frame, and the
constant cost to perform the jump and store the return address.

```k
    rule <k> #compute [ _ = call @ NAME ( ARGS ), SCHED ] => Gcallreg < SCHED > *Int REGISTERS +Int intSizes(ARGS) *Int Gcopy < SCHED > +Int Glocalcall < SCHED > ... </k>
         <funcId> NAME </funcId>
         <nregs> REGISTERS </nregs>
    rule <k> #compute [ _ = call (IDX:Int => @ FUNC) ( _ ), _ ] ... </k>
         <funcLabels> ... IDX |-> FUNC ... </funcLabels>
    // this will throw an exception, so the gas cost doesn't really matter
    rule <k> #compute [ _ = call IDX:Int ( _ ), _ ] => 0 ... </k>
         <funcLabels> LABELS </funcLabels>
      requires notBool IDX in_keys(LABELS)
```

The cost to return from an intra-contract call is the cost to move the return values into the result registers plus the cost to
jump to the return address.

```k
    rule <k> #compute [ ret ARGS::Ints, SCHED ] => Gmove < SCHED > *Int #sizeRegs(ARGS) +Int Gret < SCHED > ... </k>
         <localCalls> ListItem(_) ... </localCalls>
```

The cost to return from a contract call is zero; this cost is paid by the calling frame as part of the call instruction.

```k
    rule <k> #compute [ ret _::Ints, SCHED ] => 0 ... </k>
         <localCalls> .List </localCalls>
    rule #compute [ revert _, SCHED ] => 0
```

The cost to call another contract is very similar to the cost in EVM:

-   A constant cost for the call itself
-   A constant cost if a value is transferred along with the call
-   A constant cost if the call creates a new empty account
-   The cost of initializing the memory of the called frame with the arguments to the function.
-   The gas stipend paid to the callee to execute its code.

```k
    rule <k> #compute [ _, RETS::LValues = call _ at ACCTTO ( ARGS ) send VALUE , gaslimit GCAP, SCHED ] => Ccall(SCHED, #accountEmpty(ACCTTO), GCAP, GAVAIL, VALUE, #sizeLVals(RETS), intSizes(ARGS)) ... </k>
         <gas> GAVAIL </gas>

    rule <k> #compute [ _, RETS::LValues = staticcall _ at ACCTTO ( ARGS ) gaslimit GCAP, SCHED ] => Ccall(SCHED, #accountEmpty(ACCTTO), GCAP, GAVAIL, 0, #sizeLVals(RETS), intSizes(ARGS)) ... </k>
         <gas> GAVAIL </gas>
```

### Logging

The cost of logging is similar to the cost in EVM: a constant ccost plus a cost per byte of unindexed data plus a cost per indexed log topic.

```k
    rule <k> #compute [ log IDX, SCHED ]                                 => (Glog < SCHED > +Int (Glogdata < SCHED > *Int bytesInWords(#sizeWordStack({LM [ IDX ]}:>WordStack))) +Int (0 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int, SCHED ]                         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int bytesInWords(#sizeWordStack({LM [ IDX ]}:>WordStack))) +Int (1 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int , _:Int, SCHED ]                 => (Glog < SCHED > +Int (Glogdata < SCHED > *Int bytesInWords(#sizeWordStack({LM [ IDX ]}:>WordStack))) +Int (2 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int , _:Int , _:Int, SCHED ]         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int bytesInWords(#sizeWordStack({LM [ IDX ]}:>WordStack))) +Int (3 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #compute [ log IDX , _:Int , _:Int , _:Int,  _:Int, SCHED ] => (Glog < SCHED > +Int (Glogdata < SCHED > *Int bytesInWords(#sizeWordStack({LM [ IDX ]}:>WordStack))) +Int (4 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
```

### Local Memory

-   `load` pays a constant cost plus a cost per word loaded. The constant cost is higher if we must compute the width to be loaded dynamically.
-   `store` pays a constant cost plus a cost per word stored. The constant cost is higher if we must compute the width to be stored dynamically.

```k
    rule <k> #compute [ _ = load INDEX, SCHED ] => Gloadcell < SCHED > +Int bytesInWords(#sizeWordStack({LM [ INDEX ]}:>WordStack)) *Int Gloadword < SCHED > ... </k>
         <localMem> LM </localMem>

    rule #compute [ _ = load INDEX , OFFSET , WIDTH, SCHED ] => Gload < SCHED > +Int bytesInWords(WIDTH) *Int Gloadword < SCHED >

    rule #compute [ store VALUE , INDEX, SCHED ] => Gstorecell < SCHED > +Int intSize(VALUE) *Int Gstoreword < SCHED >
    rule #compute [ store VALUE , INDEX , OFFSET , WIDTH, SCHED ] => Gstore < SCHED > +Int bytesInWords(WIDTH) *Int Gstoreword < SCHED >
```

### Storage

-   `sload` pays a constant cost plus a cost per word in the key, plus a cost per word loaded.

```k
        rule <k> #compute [ _ = sload INDEX, SCHED ] => Gsload < SCHED > +Int Gsloadkey < SCHED > *Int intSize(INDEX) +Int Gsloadword < SCHED > *Int intSize(VALUE) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> ... INDEX |-> VALUE ... </storage>
           ...
         </account>
```

-   `sstore` pays a constant cost plus a cost per word in the key and in the value, plus a larger cost for increasing the size of the storage of the account that is partially refunded when the storage is released.

```k
    rule <k> #compute [ sstore VALUE , INDEX, SCHED ] => Csstore(SCHED, INDEX, VALUE, OLDVALUE) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> ... INDEX |-> OLDVALUE ... </storage>
           ...
         </account>

    rule <k> (.K => #lookupStorage(ACCT, INDEX)) ~> #compute [ sstore VALUE , INDEX, _ ] ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account>
      requires notBool INDEX in_keys(STORAGE)
```

### Contract creation and destruction

-   `create` pays a constant cost to initialize the account, a cost to copy the arguments of the constructor, plus a stipend to the constructor of 63/64ths of the current gas.
-   `copycreate` pays a very similar cost to `create` but with a slightly higher constant because the account code must be looked up on the blockchain.

```k
    rule #compute [ _ , _ = create _ ( ARGS ) send _, SCHED ] => Gcreate < SCHED > +Int Gcopy < SCHED > *Int intSizes(ARGS)
    rule #compute [ _ , _ = copycreate _ ( ARGS ) send _, SCHED ] => Gcopycreate < SCHED > +Int Gcopy < SCHED > *Int intSizes(ARGS)
```

-   `selfdestruct` costs a fixed amount plus a cost if the account the funds are transferred to must be created.

```k
    rule <k> #compute [ selfdestruct ACCTTO, SCHED ] => Cselfdestruct(SCHED, #accountEmpty(ACCTTO), BAL) ... </k>
         <id> ACCTFROM </id>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL </balance>
           ...
         </account>
```

### Precompiled Contracts

Each of the precompiled contracts pays a fixed cost per word of data passed to the contract plus a constant.

```k
    rule <k> #compute [ ECREC, SCHED ]  => 3000 ... </k>
    rule <k> #compute [ SHA256, SCHED ] =>  60 +Int  3 *Int bytesInWords(maxInt(LEN, intSize(DATA))) ... </k> <callData> LEN , DATA , .Ints </callData>
    rule <k> #compute [ RIP160, SCHED ] => 600 +Int 30 *Int bytesInWords(maxInt(LEN, intSize(DATA))) ... </k> <callData> LEN , DATA , .Ints </callData>
    rule <k> #compute [ ID, SCHED ]     =>  0 ... </k>

    rule #compute [ ECADD, SCHED ] => 500
    rule #compute [ ECMUL, SCHED ] => 40000
    rule <k> #compute [ ECPAIRING, SCHED ] => 100000 +Int (#sizeRegs(DATA) /Int 6) *Int 80000 ... </k> <callData> DATA </callData>
```

There are several helpers for calculating gas.

Note: These are all functions as the operator `#compute` has already loaded all the relevant state.

```k
    syntax Int ::= Csstore ( Schedule , Int , Int , Int ) [function]
 // ----------------------------------------------------------
    rule Csstore(SCHED, INDEX, VALUE, OLDVALUE) => Gsstore < SCHED > +Int Gsstorekey < SCHED > *Int intSize(INDEX) +Int Gsstoreword < SCHED > *Int intSize(VALUE) +Int #if VALUE =/=Int 0 andBool OLDVALUE ==Int 0 #then Gsstoresetkey < SCHED > *Int intSize(INDEX) +Int Gsstoreset < SCHED > *Int intSize(VALUE) #else maxInt(0, Gsstoreset < SCHED > *Int (intSize(VALUE) -Int intSize(OLDVALUE))) #fi

    syntax Operand ::= Ccall    ( Schedule , BExp , Int , Int , Int , Int , Int )  [strict(2)]
                     | Ccallgas ( Schedule , BExp , Int , Int , Int , Int , Int )  [strict(2)]
    syntax Int ::= Cgascap  ( Schedule , Int , Int , Int )                         [function]
                 | Cextra   ( Schedule , Bool , Int , Int , Int )                  [function]
                 | Cxfer    ( Schedule , Int )                                     [function]
                 | Cnew     ( Schedule , Bool , Int )                              [function]
                 | Ccallmem ( Schedule , Int , Int )                               [function]
 // -----------------------------------------------------------------------------------------
    rule Ccall(SCHED, ISEMPTY:Bool, GCAP, GAVAIL, VALUE, RETS, ARGS) => Cextra(SCHED, ISEMPTY, VALUE, RETS, ARGS) +Int Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ISEMPTY, VALUE, RETS, ARGS))

    rule Ccallgas(SCHED, ISEMPTY:Bool, GCAP, GAVAIL, 0, RETS, ARGS)     => Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ISEMPTY,     0, RETS, ARGS))
    rule Ccallgas(SCHED, ISEMPTY:Bool, GCAP, GAVAIL, VALUE, RETS, ARGS) => Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ISEMPTY, VALUE, RETS, ARGS)) +Int Gcallstipend < SCHED > requires VALUE =/=K 0

    rule Cgascap(SCHED, GCAP, GAVAIL, GEXTRA) => minInt(#allBut64th(GAVAIL -Int GEXTRA), GCAP) requires GAVAIL >=Int GEXTRA andBool notBool Gstaticcalldepth << SCHED >>
    rule Cgascap(SCHED, GCAP, GAVAIL, GEXTRA) => GCAP                                          requires GAVAIL <Int  GEXTRA orBool Gstaticcalldepth << SCHED >>

    rule Cextra(SCHED, ISEMPTY, VALUE, RETS, ARGS) => Gcall < SCHED > +Int Cnew(SCHED, ISEMPTY, VALUE) +Int Cxfer(SCHED, VALUE) +Int Ccallmem(SCHED, RETS, ARGS)

    rule Cxfer(SCHED, 0) => 0
    rule Cxfer(SCHED, N) => Gcallvalue < SCHED > requires N =/=K 0

    rule Cnew(SCHED, ISEMPTY:Bool, VALUE) => Gnewaccount < SCHED >
      requires         ISEMPTY andBool VALUE =/=Int 0
    rule Cnew(SCHED, ISEMPTY:Bool, VALUE) => 0
      requires notBool ISEMPTY orBool  VALUE  ==Int 0

    rule Ccallmem(SCHED, RETS, ARGS) => Gmove < SCHED > *Int RETS +Int Gcopy < SCHED > *Int ARGS

    syntax Int ::= Cselfdestruct ( Schedule , BExp , Int ) [strict(2)]
 // ------------------------------------------------------------------
    rule Cselfdestruct(SCHED, ISEMPTY:Bool, BAL) => Gselfdestruct < SCHED > +Int Gnewaccount < SCHED >
      requires ISEMPTY andBool (        Gselfdestructnewaccount << SCHED >>) andBool BAL =/=Int 0
    rule Cselfdestruct(SCHED, ISEMPTY:Bool, BAL) => Gselfdestruct < SCHED >
      requires ISEMPTY andBool (notBool Gselfdestructnewaccount << SCHED >>  orBool  BAL  ==Int 0)
    rule Cselfdestruct(SCHED, ISEMPTY:Bool, BAL) => Gselfdestruct < SCHED >
      requires notBool ISEMPTY

    syntax KResult ::= Bool
    syntax BExp ::= Bool
                  | #accountEmpty(Int)
                  | #accountEmpty(Contract, Int, Int) [klabel(accountEmpty), function]
 // ----------------------------------------------------------------------------------
    rule <k> #accountEmpty(ACCT) => #accountEmpty(CODE, NONCE, BAL) ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> CODE </code>
           <nonce> NONCE </nonce>
           <balance> BAL </balance>
           ...
         </account>
    rule <k> (.K => #loadAccount ACCT) ~> #accountEmpty(ACCT) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool ACCT in ACCTS

    rule #accountEmpty(CODE, NONCE, BAL) => CODE ==K #emptyCode andBool NONCE ==Int 0 andBool BAL ==Int 0

    syntax Int ::= #allBut64th ( Int ) [function]
 // ---------------------------------------------
    rule #allBut64th(N) => N -Int (N /Int 64)

    syntax Int ::= G0 ( Schedule , WordStack , Ints ) [function, klabel(G0create)]
                 | G0 ( Schedule , String , Ints )    [function, klabel(G0call)]
                 | G0 ( Schedule , WordStack , Bool ) [function, klabel(G0aux)]
 // ---------------------------------------------------------------------------
    rule G0(SCHED, .WordStack, true)  => Gtxcreate    < SCHED >
    rule G0(SCHED, .WordStack, false) => Gtransaction < SCHED >

    rule G0(SCHED, WS, ARGS) => G0(SCHED, #parseByteStackRaw(#rlpEncodeLength(#rlpEncodeString(#unparseByteStack(WS)) +String #rlpEncodeInts(ARGS), 192)), true)
    rule G0(SCHED, FUNC, ARGS) => G0(SCHED, #parseByteStackRaw(#rlpEncodeLength(#rlpEncodeString(FUNC) +String #rlpEncodeInts(ARGS), 192)), false)

    rule G0(SCHED, 0 : REST, ISCREATE::Bool) => Gtxdatazero    < SCHED > +Int G0(SCHED, REST, ISCREATE)
    rule G0(SCHED, N : REST, ISCREATE::Bool) => Gtxdatanonzero < SCHED > +Int G0(SCHED, REST, ISCREATE) requires N =/=Int 0

    syntax Int ::= "G*" "(" Int "," Int "," Int ")" [function]
 // ----------------------------------------------------------
    rule G*(GAVAIL, GLIMIT, REFUND) => GAVAIL +Int minInt((GLIMIT -Int GAVAIL)/Int 2, REFUND)

    syntax Int ::= Cmul     ( Schedule , Int , Int )             [function]
                 | Ckara    ( Int , Int )                        [function]
                 | Cdiv     ( Schedule , Int , Int )             [function]
                 | Cexp     ( Schedule , Int , Int , Int )       [function]
                 | Cexpmod  ( Schedule , Int , Int , Int , Int ) [function]
 // -----------------------------------------------------------------------
    rule Cmul(SCHED, L1, L2) => Cmul(SCHED, L2, L1)
      requires L2 >Int L1

    rule Cmul(SCHED, L1, L2) =>
        Gmulkara < SCHED > *Int Ckara(L1, L2) +Int
        Gmulword < SCHED > *Int (L1 +Int L2) +Int
        Gmul < SCHED >
      [owise]
      // Note that if L2 is low enough (< 32) then #overApproxKara(L2) = L2 * L2

    rule Ckara(L1, L2) => L1 *Int #overApproxKara(L2) /Int L2
      requires L1 >=Int L2

    rule Ckara(L1, L2) => L2 *Int #overApproxKara(L1) /Int L1 [owise]

    rule Cdiv(SCHED, L1, L2) =>
        Gdivkara < SCHED > *Int Ckara(L1 -Int L2 +Int 1, L2) +Int
        Gdivword < SCHED > *Int L1 +Int
        Gdiv < SCHED >
      requires L1 >=Int L2
    
    rule Cdiv(SCHED, L1, L2) =>
        Gdivword < SCHED > *Int L1 +Int
        Gdiv < SCHED >
      [owise]

    rule Cexp(SCHED, L1, W1, W2) =>
        Gexpkara < SCHED > *Int #overApproxKara(#adjustedBitLength(L1, W1) *Int W2 /Int 64) +Int
        Gexpword < SCHED > *Int L1 +Int
        Gexp < SCHED >

    rule Cexpmod(SCHED, LB, LEX, LM, EX) =>
        Gexpmodkara < SCHED > *Int #overApproxKara(LM) *Int #adjustedBitLength(LEX, EX) +Int
        Gexpmod < SCHED >
      requires LB <=Int LM

    rule Cexpmod(SCHED, LB, LEX, LM, EX) =>
        Gexpmodkara < SCHED > *Int #overApproxKara(LM) *Int #adjustedBitLength(LEX, EX) +Int
        Cdiv(SCHED, LB, LM) +Int
        Gexpmod < SCHED >
      [owise]

```

#### Approximating `x^log_2 3`

Say we want to approximate `x^log_2 3` with a family of quadratic functions, say of the form `a2*x^2+a1*x+a0`.

* we use the fact that `(2^k)^(log 3 / log 2) = 3^k`
* then, for `x = 2^k`, it means we want to approximate `3^k` with `a2*4^k(+...)`, whence `a2 ~= 1/(4/3)^k`.
* looking for powers of 2 (to make `a2*x^2` a shift) which are smaller, but close to `(4/3)^k`, we see that `(4/3)^5 ~= 4.21` and` (4/3)^10 ~=17.75`
* we then can take candidates `x^2 for x <=32`; `x^2/4 + a1* x + a0 for 32 <= x <=1024`, and  `x^2/16 + b1 * x + b0 for x >= 1024 `
* now, if we want the approximation to be differentiable, its derivative, `2* x for x <=32; x/2 + a1 for 32 <= x <=1024, x/8 + b1 for x >= 1024`,  must be continuous, so
* `a1 = 2 * 32 - 32/2 = 48`, and `b1 = 1024/2 + a1 - 1024 / 8 = 432`
* next, the approximation must also be continuous, so
* `a0 = 32^2 - 32^2/4 - 48*32 = -768`, and `b0 = 1024^2/4+ 48*1024 -768 - 1024^2/16 - 432*1024 = - 197376`

```k
    syntax Int ::= #overApproxKara ( Int )                 [function]
 // -----------------------------------------------------------------
    rule #overApproxKara(N) => #if N <=Int   32 #then N *Int N
                         #else #if N <=Int 1024 #then N *Int N /Int  4 +Int  48 *Int N -Int    768
                         #else                        N *Int N /Int 16 +Int 432 *Int N -Int 197376
                         #fi #fi
```

#### Approximating exponentiation

Exponentiation algorithms work by sucessively performing at most two multiplication operations per bit in the exponent.

Because exponents could be very large, we approximate this length by counting the number of words in the exponent and multiplying by 64,
the number of bits in a word. However, in order to create more precision on smaller inputs, if the number is less than 2^64,
we compute down to the very last bit, by examining the individual bits of the low order word.

This same function can be used to approximate the bit size of an exponentiation base for non-modular exponentiation,
which is used to compute a more accurate approximation of the length of the result than a measurement in words.

```k
    syntax Int ::= #adjustedBitLength(Int, Int) [function]
                 | #adjustedBitLength(Int) [function, klabel(#adjustedBitLengthAux)]
 // --------------------------------------------------------------------------------
    rule #adjustedBitLength(LEX, EX) => maxInt(1, #if LEX <=Int 1 #then 0 #else 64 *Int (LEX -Int 1) #fi +Int #adjustedBitLength(twos(8, EX)))

    rule #adjustedBitLength(0) => 0
    rule #adjustedBitLength(1) => 0
    rule #adjustedBitLength(N) => 1 +Int #adjustedBitLength(N /Int 2) requires N >Int 1

```

Gas Model Parameters
--------------------

The IELE semantics is designed to be extensible in future hard forks while still maintaining an accurate semantics of the language prior
to the fork. As such, we introduce a number of parameters to the gas model which are dependent on the gas schedule used.
Here we introduce only two gas schedules, the "DEFAULT" schedule, provided solely for backwards-compatibility with the EVM VMTests test suite,
and the "ALBE" schedule, representing the initial release of IELE. The name Albe is chosen due to its significance as the name for one of the Romanian Iele.
You can specify which profile is used by passing in the argument `-cSCHEDULE=<FEE_SCHEDULE>` when calling `krun` (the available `<FEE_SCHEDULE>` are supplied here).

A `ScheduleFlag` is a boolean determined by the fee schedule; applying a `ScheduleFlag` to a `Schedule` yields whether the flag is set or not.

```k
    syntax Bool ::= ScheduleFlag "<<" Schedule ">>" [function]
 // ----------------------------------------------------------

    syntax ScheduleFlag ::= "Gselfdestructnewaccount" | "Gstaticcalldepth"
 // ----------------------------------------------------------------------
```

A `ScheduleConst` is a constant determined by the fee schedule; applying a `ScheduleConst` to a `Schedule` yields the correct constant for that schedule.

```k
    syntax Int ::= ScheduleConst "<" Schedule ">" [function]
 // --------------------------------------------------------
 
    syntax ScheduleConst ::= "Gcopy"      | "Gmove"        | "Greadstate" | "Gadd"          | "Gaddword"       | "Gmul"          | "Gmulword"     | "Gmulkara"
                           | "Gdiv"       | "Gdivword"     | "Gdivkara"   | "Gexpkara"      | "Gexpword"       | "Gexp"          | "Gmulmod"      | "Gexpmodkara"
                           | "Gexpmod"    | "Gnot"         | "Gnotword"   | "Gbitwise"      | "Gbitwiseword"   | "Gbyte"         | "Gsign"        | "Gsignword"
                           | "Giszero"    | "Gcmp"         | "Gcmpword"   | "Gbr"           | "Gbrcond"        | "Gblockhash"    | "Gsha3"        | "Gsha3word"
                           | "Gloadcell"  | "Gload"        | "Gloadword"  | "Gstorecell"    | "Gstore"         | "Gstoreword"    | "Gbalance"     | "Gextcodesize" 
                           | "Glog"       | "Glogdata"     | "Glogtopic"  | "Gsstore"       | "Gsstoreword"    | "Gsstorekey"    | "Gsstoreset"   | "Gsstoresetkey"
                           | "Gsload"     | "Gsloadkey"    | "Gsloadword" | "Gselfdestruct" | "Gcallmemory"    | "Gcallreg"      | "Glocalcall"   | "Gret"
                           | "Gcall"      | "Gcallstipend" | "Gcallvalue" | "Gnewaccount"   | "Gcreate"        | "Gcopycreate"   | "Gcodedeposit" | "Gmemory"
                           | "Gquadcoeff" | "Gtransaction" | "Gtxcreate"  | "Gtxdatazero"   | "Gtxdatanonzero" | "Rselfdestruct" | "Rb"           | "Smemallowance"
 // ---------------------------------------------------------------------------------------------------------------------------------------------------------------
```

### Default Schedule

This schedule is used to execute the EVM VM tests, and contains minor variations from the actual schedule used for execution.

```k
    syntax Schedule ::= "DEFAULT"
 // -----------------------------
    rule Gcopy          < DEFAULT > => 3
    rule Gmove          < DEFAULT > => 3
    rule Greadstate     < DEFAULT > => 2
    rule Gadd           < DEFAULT > => 0
    rule Gaddword       < DEFAULT > => 3
    rule Gmul           < DEFAULT > => 0
    rule Gmulword       < DEFAULT > => 2
    rule Gmulkara       < DEFAULT > => 3
    rule Gdiv           < DEFAULT > => 0
    rule Gdivword       < DEFAULT > => 2
    rule Gdivkara       < DEFAULT > => 3
    rule Gexpkara       < DEFAULT > => 50
    rule Gexpword       < DEFAULT > => 10
    rule Gexp           < DEFAULT > => 0
    rule Gmulmod        < DEFAULT > => 0
    rule Gexpmodkara    < DEFAULT > => 4
    rule Gexpmod        < DEFAULT > => 0
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
    rule Gloadcell      < DEFAULT > => 3
    rule Gload          < DEFAULT > => 0
    rule Gloadword      < DEFAULT > => 3
    rule Gstorecell     < DEFAULT > => 3
    rule Gstore         < DEFAULT > => 0
    rule Gstoreword     < DEFAULT > => 3
    rule Gbalance       < DEFAULT > => 400
    rule Gextcodesize   < DEFAULT > => 700
    rule Glog           < DEFAULT > => 375
    rule Glogdata       < DEFAULT > => 8
    rule Glogtopic      < DEFAULT > => 375
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

This is the initial schedule of IELE.

```k
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

