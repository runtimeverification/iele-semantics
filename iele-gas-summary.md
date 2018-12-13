IELE Gas Cost
====================

The authoritative reference for gas costs is
[iele-gas.md](iele-gas.md). It's a
[literate program](https://en.wikipedia.org/wiki/Literate_programming)
that can't get out of date with the source because it *is*
the source. It also contains text explaining the source, but that text
assumes you know [K](https://runtimeverification.com/k/). So it's too
hard to read for most people.

If you're "most people", this document provides information you can
use to understand a contract's gas cost.

## Background

There are two main architectural differences between IELE and the
[Ethereum Virtual Machine](https://en.wikipedia.org/wiki/Ethereum#Virtual_Machine)
(EVM) that affect gas costs: variable-sized "words" and a simpler
model for memory.

#### Terminology

In EVM, there are
[three places to store data](https://solidity.readthedocs.io/en/v0.4.24/introduction-to-smart-contracts.html?highlight=memory#storage-memory-and-the-stack):
*storage*, *memory*, and *stack*. The first two names are unfortunate
because they're also generic terms. (It's hard to talk about where a
program puts data without using either "memory" or "storage.") We
disambiguate by referring to *blockchain storage* and *transaction
memory*.

The EVM's three types of storage are made from two different sorts of
"stuff." Stack and blockchain storage are made from 256-bit
words. Memory can be thought of as an arbitrarily extensible byte
array, with different values identified by their byte index and
length.

In IELE, all types of storage are made of arbitrarily extensible
*flexwords*. A flexword is scaled to fit its contents. That is, if you
replace the number 0 in a flexword with a number that requires 1024
bits, the flexword will grow to accommodate the number. (Overflow is
impossible.)

####  Flexwords

Because the hardware underlying the virtual machine has 64-bit words,
it's most efficient if the smallest flexword is 64 bits long and
expands in 64-bit units.

Because of this, it's relatively rare for a flexword to be as big as
an EVM stack word (256 bits or four hardware
words). Most often, that much space is required only for a
cryptographic hash. Addresses (as in EVM) are 160 bits, so they fit in
three machine words. Scratch variables like a loop index or running
total will usually fit in one hardware word and are very unlikely to
ever expand beyond it.

The gas cost of a IELE instruction depends, in part, on the size of the
flexwords it operates on. Adding two 256-bit flexwords requires more
computation than two 64-bit flexwords, so it costs more. (But not by
much: in the case of addition, an additional word costs only 0.001
extra gas.)

#### The IELE memory model and gas prices

IELE replaces EVM's stack with named flexwords called *registers*. A
register computation looks like this:

    %result = add %first, %second

Rather than being a single byte array (as in EVM), the transaction
memory is composed of an arbitrary number of flexwords, indexed by
number. (So registers are named flexwords and transaction memory is
made of numbered flexwords. The flexwords themselves are
indistinguishable.)

Rather than having different gas charges for stack and transaction
memory, IELE charges for the maximum total number of machine words
used by registers and transaction memory. Storage is storage, and it's all charged the same.

That might seem unfair, since the EVM stack is free, whereas each
register has to be paid for. It's better, though, to think of the EVM
as giving you a certain amount of memory for free: a stack sized to 1024
256-bit words plus a preallocated amount of transaction memory. IELE
gives you the same total amount of free space, which the compiler apportions
however it needs.

As with EVM, when a IELE transaction exceeds the amount of cost-free
memory, the cost grows very rapidly (roughly quadratically in the
amount of excess sortage required).


## Instruction tables 

The cost for every instruction is the cost of *computation* plus the
cost of *data*. We'll summarize based on the most likely scenario. For
example, when adding two registers, the most likely case is adding two
64-bit words without overflow, which costs 2.801 gas. Where more complicated
cases are simple to explain, they will be. For example, each 64-bit
increment in the size of either addition argument adds another 0.001 gas.

Tables are arranged into groups of related instructions. To save
space, shorthand is used for some ideas.

* <a id="size">**size n**</a> is in 64 bit units.  "Size 2"
  means a flexword of length 2 (128 bits). There are no fractional sizes; you
  can have a 64-bit flexword and a 128-bit flexword, but nothing
  in between.
  
* <a id="wordcost">**wordcost n**</a> means each additional machine
  word adds *n* to the cost. *n* is usually very small.

* <a id="maxargs" class="anchor">**maxargs**</a> means the relevant size is the
  largest of the operands.
  
* <a id="minargs" class="anchor">**minargs**</a> means the relevant size is the
  smallest of the operands.
  
* <a id="sumargs" class="anchor">**sumargs**</a> means the relevant size is the sum
  of the sizes of the operands. 

In all cases where a value is assigned to a destination register, we
assume that register doesn't need to be resized into a larger
flexword. For example, for arithmetic operations, we assume no
overflow.

In the table below, "typical cost" means that all operands are of
[size](#size) 1. Costs are displayed to three digits of precision so
that the small [wordcosts](#wordcost) have a non-zero value.

### 1. Register operations

#### Assignment to a register

This covers assignment from another register or a constant. Loading
and storing transaction memory are covered below.

| Instruction | Typical Cost | Notes    |
|-------------|--------------|----------|
| dest = src  | 2.000        | Independent of the size of `src`. |

#### Arithmetic

| Instruction | Typical Cost | Notes                                                                |
|-------------|--------------| ---------------------------------------------------------------------|
| add         | 2.801    | fixed cost 2.800, [wordcost](#wordcost) 0.001, [maxargs](#maxargs)   |
| mul         | 4.912    | grows fast for larger flexwords                                      | 
| div         | 4.913    | cost calculations are complicated                                    | 
| mod         | 4.913    | same as `div`                                                          | 
| exp         | 5.332    | assumes the result fits in 1 word, otherwise grows very quickly      | 

#### Comparison operators

| Instruction | Typical Cost   | Notes                            |
|-------------|----------------|----------------------------------|
| iszero      |  1.800      | independent of word size            |
| cmp         |  2.501      | [wordcost](#wordcost) 0.01          |

#### Bitwise arithmetic

| Instruction | Typical Cost | Notes                                              |
|-------------|--------------|----------------------------------------------------|
| not         | 2.703     | fixed cost 2.700, [wordcost](#wordcost) 0.003                   |
| and         | 2.901     | [*minargs*](#minargs), fixed cost 2.900, [wordcost](#wordcost) 0.001        |
| or          | 2.901     | [maxargs](#maxargs), fixed cost 2.900, [wordcost](#wordcost) 0.001          |
| xor         | 2.703     | [maxargs](#maxargs), fixed cost 2.700, [wordcost](#wordcost) 0.003                   |
| shift       | 2.902     | [sumargs](#sumargs)                                            | 
| log2        | 2.301     | [wordcost](#wordcost) 0.001                                     |

#### Byte access

| Instruction | Typical Cost | Notes                     |
|-------------|--------------|---------------------------|
| byte        | 2.500     | independent of word size     |
| twos        | 3.101     | [wordcost](#wordcost) 0.001  |
| sext        | 3.305     | [wordcost](#wordcost) 0.005  |
| bswap       | 3.310     | [wordcost](#wordcost) 0.01   |


#### Modular arithmetic

We'll look at two common cases: [size](#size) 1 and [size](#size) 4.

| Instruction | 1-word Cost                | Notes                                                            |
|-------------|----------------------------|------------------------------------------------------------------|
| addmod      | 7.727                  | cost of addition + cost of `mod`                                 |
| mulmod      | 9.838                  | cost of multiplication + cost of `mod`                                 |
| expmod      | 6.203 to 6.797         | depends on actual number of bits necessary to represent modulus  |


| Instruction | 4-word cost                         | Notes                                                            |
|-------------|--------------------|------------------------------------------------------------------|
| addmod      |  7.793             |                                                                  |
| mulmod      |  10.096            |                                                                  |
| expmod      |  12.897 to 14.913  | depends on actual number of bits necessary to represent modulus  |




#### Jump statements

| Instruction          | Typical Cost  | Notes       |
|----------------------|---------------|-------------|
| br (unconditional)   | 5             |     |
| br (conditional)     | 5             |     |


#### Function call/return

The cost of function call `call @foo (ARGS)` is 

    [1.000 * N_REGISTERS] + 6.800

where N_REGISTERS is the number of registers used by the function being called. 

For example, suppose we are attempting to call a function `foo (arg1, arg2)` which makes use of 3 registers.
Then we can express the cost as: 

    [ 1.000 * 3 ] + 6.800 = 3.000 + 6.800 = 9.800

The cost of return (`ret ARGS`) is zero.


#### SHA3

| Instruction | Typical Cost    | Notes           |
|-------------|-----------------|-----------------|
| sha3        |  8.320      | [wordcost](#wordcost) 0.020  |

#### Local and network state operations

| Instruction       | Typical Cost | Notes |
|-------------------|--------------|-------|
| @iele.balance     | 400          |       |
| @iele.extcodesize | 700          |       |
| @iele.blockhash   | 20           |       |
| calladdress       | 700          |       |
| @iele.address     | 2            |       |
| @iele.beneficiary | 2            |       |
| @iele.caller      | 2            |       |
| @iele.callvalue   | 2            |       |
| @iele.codesize    | 2            |       |
| @iele.difficulty  | 2            |       |
| @iele.gas         | 2            |       |
| @iele.gaslimit    | 2            |       |
| @iele.gasprice    | 2            |       |
| @iele.msize       | 2            |       |
| @iele.number      | 2            |       |
| @iele.origin      | 2            |       |
| @iele.timestamp   | 2            |       |


### 2. Operations on [transaction memory](#terminology)

#### Loading and storing

It's worth describing the four instructions before looking at their cost.

1. `%register = load %address`

    `%address` is a register that contains a number identifying a
    transaction memory flexword. The content of that flexword is
    transferred into the register named `%register`.
    
    The address can also be a constant (as in `load 5`). It makes no
    difference to the cost.

2. `%register = load %address, %start, %length`

   Instead of loading the entire flexword at `%address`, transfer
   `%length` bytes starting at byte `%start`.

3. `store %register, %address`

   Transfer the contents of `%register` into `%address`.

4. `store %register, %address, %start, %length`

   Replace the part of `%address` identified by `%start` and `%length` with the
   contents of `%register`.
   

| Instruction                                | Typical Cost | Notes              |
|--------------------------------------------|--------------|--------------------|
| load %address                              | 2.903     | [wordcost](#wordcost) 0.003     |
| load %address, %start, %length             | 3.303     | [wordcost](#wordcost) 0.003     |
| store %register, %address                  | 2.804     | [wordcost](#wordcost) 0.004     |
| store %register, %address, %start, %length | 3.904     | [wordcost](#wordcost) 0.004     |

#### Logging

The `log` instruction takes an address in transaction memory plus up
to four topics.

| Instruction              | Typical Cost | Notes                           |
|--------------------------|--------------|---------------------------------|
| log address              | 383          | [wordcost](#wordcost) 8 (of memory data)     |
| log address, topic       | 758          | each topic adds 375             |
| log address, t, t        | 1133         |                                 |
| log address, t, t, t     | 1508         |                                 |
| log address, t, t, t, t  | 1883         |                                 |


### 3. Blockchain operations

#### Loading and storing

The typical cost assumes the offset into blockchain storage fits
inside one machine word, as does the content to be transferred.


| Instruction     | Typical Cost |
|-----------------|--------------|
| sload %offset   | 200          |  

Fixed cost is 190.       
Each machine word used by `offset` adds 8.  
Each machine word transferred adds 2. 

| Instruction                                    | Typical Cost       | Notes                                            |
|------------------------------------------------|--------------------|--------------------------------------------------|
| sstore %value, %offset **creates** an entry    | 8900.700     | [wordcost](#wordcost) 1875, [sumargs](#sumargs)                           |
| sstore %value, %offset **changes** an entry    | 4959.700     | [wordcost](#wordcost) 1875 |       |
| sstore 0, %offset **deletes** an entry         | 4959.700     |                                                  |

Note: when updating an entry, the wordcost is charged if the
    blockchain entry must grow to hold the new value.

#### Contract call (external function call)

The cost of a contract call is the same as in EVM, with the exception
of a cost of 1.000 per argument or return value register (regardless
of their size).

Here are the components of the cost:

| Component                                  | Cost 
|--------------------------------------------|------
| Base cost for any call                     | 700
| Addition if a non-zero value is sent       | 9000*
| Addition if a new empty account is created | 25000
| Addition for each argument or return value | 1

\* Of the 9000 gas, 2300 is delivered to the called contract as a
[stipend](https://ethereum.stackexchange.com/questions/5992/how-much-computation-can-be-done-in-a-fallback-function)
to fund minimal computation.

#### Contract creation 

| Instruction      | Constant cost    | Additional cost for contract bytes |
|------------------|------------------|------------------------------------|
| create           | 32000            | [wordcost](#wordcost) 2                      |
| copycreate       | 33000            | [wordcost](#wordcost) 2                      |

#### Contract destruction


| Instruction       | Cost        | Notes                                            |
|-------------------|-------------|--------------------------------------------------|
| selfdestruct      | 0           | account to receive funds exists                  |
| selfdestruct      | 25000       | account to receive funds must be created         |


#### Precompiled contracts

| Contract                                                      | Constant cost       | Notes |
|---------------------------------------------------------------|---------------------|--------------------------------|
| ecrecover (recovery of ECDSA signature)                       | 3000.0              |                                |
| sha256 (hash function)                                        | 25.030              | [wordcost](#wordcost) 0.03, [maxargs](#maxargs)    |
| ripemd160 (hash function)                                     | 25.030              | [wordcost](#wordcost) 0.03, [maxargs](#maxargs)    |
| identity                                                      | 0                   |                                |
| ecpairing (checking a pairing equation on curve alt_bn128)    | 126000              | [wordcost](#wordcost) 26000    |
| ecadd (addition on elliptic curve alt_bn128)                  | 35.000              |                                |
| ecmul (Scalar multiplication on elliptic curve alt_bn128)     | 1700.000            |                                |

Note: the contract for modular exponentiation has been replaced by the
`expmod` instruction.
