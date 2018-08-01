This page summarizes the IELE gas model for those uninterested in learning the specific details of K in order to interpret [iele-gas.md](iele-gas.md), the formal specification of the gas model in K.

# Memory

IELE has arbitrary precision integer registers as well as local memory, so this must be taken into consideration when determining the memory consumption of a smartcontract. However, unlike EVM, it does not have a word stack. To compensate for this, we allocate 32 KB of "free" memory for each contract call, equivalent to the 1024 32-byte stack slots in EVM. Once a user uses more memory than this in their call frame, the cost of memory starts out at 1 gas per 64-bit word and gradually becomes quadratic after roughly 90 words.

Additionally, because it is not always easy to determine the exact size in bytes of the result of an instruction prior to executing it, we estimate this size using a specific set of rules and use the estimated cost to charge for gas. This estimation may be in some cases slightly larger than the real cost, but it should be close. Note that we adjust for the actual cost after the instruction is executed, so even if you pay for more memory than you actually use, you can still use that additional memory without paying anything further.

If you are interested in the exact details of how these memory estimates are computed, refer to the formal specification. A typical contract is not likely to use more than 32 kilobytes of memory and thus is unlikely to actually require this type of cost.

# Computation gas

Each instruction also has a computational gas cost, the cost in gas of executing the instruction itself (as well as paying for any network bandwidth or disk usage it might incur).

This cost is in many cases a function of the number of words in the size of the operands and result. Thus, in the interests of simplifying, we present here the upper bound on costs for 1-word (64-bit) and 4-word (256-bit) operands and results. Unless your operation overflows these bounds, you will not end up paying more gas than this.

| Instruction       | 1-word | 4-word |
| not               |      3 |     12 |
| and               |      3 |     12 |
| or                |      3 |     12 |
| xor               |      3 |     12 |
| shift             |      3 |     12 |
| log2              |      3 |     12 |
| iszero            |      3 |      3 |
| cmp               |      3 |     12 |
| add               |      3 |     12 |
| sub               |      3 |     12 |
| mul               |      5 |     28 |
| div               |      5 |     26 |
| mod               |      5 |     26 |
| exp               |     10 |   7429 |
| addmod            |     13 |     38 |
| mulmod            |     15 |    140 |
| expmod            |    252 |  16320 |
| byte              |      3 |      3 |
| twos              |      5 |     20 |
| sext              |      5 |     20 |
| bswap             |      5 |     20 |
| @iele.gas         |      2 |      2 |
| @iele.gasprice    |      2 |      2 |
| @iele.gaslimit    |      2 |      2 |
| @iele.number      |      2 |      2 |
| @iele.msize       |      2 |      2 |
| @iele.codesize    |      2 |      2 |
| @iele.beneficiary |      2 |      2 |
| @iele.address     |      2 |      2 |
| @iele.origin      |      2 |      2 |
| @iele.caller      |      2 |      2 |
| @iele.timestamp   |      2 |      2 |
| @iele.difficulty  |      2 |      2 |
| @iele.callvalue   |      2 |      2 |
| @iele.blockhash   |     20 |     20 |
| @iele.balance     |    400 |    400 |
| @iele.extcodesize |    700 |    700 |
| calladdress at    |    700 |    700 |
| %i = %j           |      3 |     12 |
| %i = <immediate>  |      3 |     12 |
| br <label>        |      8 |      8 |
| br %cond, <label> |     10 |     10 |
| revert            |      0 |      0 |
