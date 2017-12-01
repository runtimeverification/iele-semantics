This page discusses the design of the IELE (after the [Mythological Iele](https://en.wikipedia.org/wiki/Iele)) virtual machine.

The design of IELE was based on our experience with formally defining [dozens of languages in K](https://github.com/kframework), but especially on recent experience and lessons learned while formally defining two other virtual machines in K, namely:

* [KEVM](https://github.com/kframework/evm-semantics), our semantics of the [Ethereum Virtual Machine](https://github.com/ethereum/yellowpaper) (EVM); and

* KLLVM, our semantics of [LLVM](http://llvm.org); the latest version of the LLVM semantics will be made public when complete and published, but an earlier version [is available](https://github.com/kframework/llvm-semantics).

Unlike the EVM, which is a stack-based machine, IELE is a register-based machine, like LLVM. It has an unbounded number of registers and also supports unbounded integers. There are some tricky but manageable aspects with respect to gas calculation, a critical part of the design.

We first discuss the rationale behind the design of IELE, then the major design decisions from the perspective of differences from EVM and LLVM, respectively.

# Design Rationale

Here are the forces that drove the design of IELE:

1. To serve as a uniform, lower-level platform for translating and executing smart contracts from higher-level languages, which can also interact with each other by means of an ABI (Application Binary Interface). The ABI is a core element of IELE, and not just a convention on top of it.  The unbounded integers and unbounded number of registers should make compilation from higher-level languages more straightforward and elegant and, looking at the success of LLVM, more efficient in the long term.  Indeed, many of the LLVM optimizations are expected to carry over.  For that reason, IELE followed the design choices and representation of LLVM as much as possible.  The team also included LLVM experts from the University of Illinois (where LLVM was created).

2. To provide a uniform gas model, across all languages.  The general design philosophy of gas calculation in IELE is "no limitations, but pay for what you consume".  For example, the more registers a IELE program uses, the more gas it consumes. Or the larger the numbers computed at runtime, the more gas it consumes.  The more memory it uses, in terms of both locations and size of data stored at locations, the more gas it consumes.  And so on.

3. To make it easier to write secure smart contracts.  This includes writing requirements specifications that smart contracts must obey as well as making it easier to develop automated techniques that mathematically verify / prove smart contracts correct with respect to to such specifications.  For example, pushing a possibly computed number on the stack and then jumping to it regarded as an address makes verification hard, and thus security weaker, with current smart contract paradigms.  IELE has named labels, like LLVM, and jump statements can only jump to those labels.  Also, avoiding the use of a bounded stack and not having to worry about stack or arithmetic overflow makes specification and verification of smart contracts significantly easier.

Like [KEVM](https://github.com/kframework/evm-semantics), the formal semantics of EVM that we previously defined, validated and evaluated using the [K framework](http://kframework.org), the design of IELE was also done in a semantics-based style, using K. Together with a fast (LLVM-based) execution backend for K that is still under development, it is expected that the interpreter obtained automatically from the semantics of IELE will be sufficiently efficient to serve as a reference implementation of IELE.


# Designed Changes Relative to EVM

Here are the IELE design decisions described from the perspective of differences from EVM.

## Registers

IELE is a register-based bytecode language, unlike EVM, which is a stack-based bytecode. As a result, the following alterations have been made:

* Each instruction in the bytecode takes register operands representing the arguments and result of the instruction.
* There is an unlimited number of registers available and they can be referred to by user-defined names, similar to the virtual registers of LLVM. The more registers a contract uses, the more gas it consumes.
* Because the stack no longer exists, it is no longer possible for the VM to throw an exception due to stack underflow or overflow.
* Because the stack no longer exists, the POP, DUP, SWAP, and PUSH instructions no longer exist. They are replaced with an assignment instruction that can be used to load a constant value into a register (see section on arbitrary-precision integers for more details), and/or copy a value from one register to another.
* Registers are divided into two categories: global registers and local registers. Local registers are visible only within the function in which they are used, while global registers are visible across all functions of a given contract. Similar to LLVM, global registers names are prefixed by `@`, while local register names are prefixed by `%`.
* Local registers have a lifetime equal to the lifetime of a function call to the function that contains them, while global registers have a lifetime that starts with the creation of an account with an instance of the contract that contains them and ends with the destruction of said account.

## Program Structure

Unlike EVM, in which a contract is a sequence of instructions, a IELE contract consists of a header giving the contract a name, followed by one or more function definitions and optionally one or more external contract declarations.

* An external contract declaration simply declares the name of another contract. The contract under definition can only create accounts with deployed code being a copy of the contract itself or one of the contracts that have been declared as external (see section on contract creation for details). Each externally declared contract should have been defined in the same file and above the contract that externally declares it.
* A function definition includes the function signature, the function body and whether or not the function is public. A function signature includes a function name (which is also represented as a global register, as it is visible across the contract) and names of formal arguments (which are represented as local registers, as they are only visible within the function body).
* A public function can be called by other accounts, while a non-public one can only be called by other functions within the same contract.
* A special function named `@init` is automatically added to the contract (and can be overridden by the programmer), and will be called when an account is created with this contract.
* An account to which code has never been deployed contains an implicit public function `@deposit` which takes no arguments, returns no values, and does nothing. This function exists to allow accounts to receive payment even if they do not have a contract deployed to them. Note that a contract can forbid payments by refusing to declare the `@deposit` function, and explicitly raising an exception if any of its entry points are invoked with a balance transfer.
* The contract is malformed if it contains multiple function definitions with the same name.

## Static Jumps

IELE no longer has dynamic jump instructions. They are replaced with jumps that take a named label as an argument, and with a call/return instruction system to allow function calls that return to the caller.

* The code within a function contains named labels. Any instruction can be prefixed by a named label that makes this instruction a valid target of a jump.
* The `br` instruction has two variants. It can used with a named label as a single argument for unconditional jumps within a function body, or with an additional register holding a condition value for conditional jumps when the condition value is not zero. The instruction prefixed by the named label is the target of the jump. Therefore all jump targets can be known statically
* The contract is malformed if multiple labels within the same function have the same name.
* The contract is malformed if the label argument of the `br` instruction is not found as a label in the function containing the instruction. This is unlike the corresponding behavior in EVM, where an exception is thrown if the argument of JUMP or JUMPI is not the program counter of a JUMPDEST instruction

## Function Call/Return

IELE function calls take an arbitrary number of register arguments and return values instead of a memory range.

* IELE has a `call` instruction used for local calls to other functions within the contract. `call` takes a function name, an arbitrary number of register arguments, and an arbitrary number of register return values, and jumps to the start of the function in the current contract with the corresponding name. It pushes the current instruction position, the values of all local registers, and the register operands of the return values onto the local call stack. The arguments of the call are copied into local registers corresponding to the formal arguments of the function being called.
* The `ret` and `revert` instructions take an arbitrary number of register operands. `revert` reverts the contract, but `ret` will return to the instruction position on the top of the local call stack if one exists, restoring the values of local registers and copying the return values into the return value operands of the call instruction. If the local call stack is empty, it returns from the contract.
* IELE has a `call .. at` and a `staticcall .. at` instruction used for calls to public functions of other contracts (the target account address is a register operand of the instruction, as is the amount of gas to be spent during the call, and the value to send in case of `call .. at`). `call .. at` and `staticcall .. at` take an arbitrary number of arguments and return values in the form of register operands instead of memory ranges. The arguments are copied into local registers corresponding to the formal arguments of the function being called. When the contract returns, the returned values are copied into the return value registers of the call instruction.
* If the name of the function being called does not correspond to a public function of the contract being called, an exception occurs.
* In all cases, if a mismatch occurs between the number of arguments or return values of a function and the matching `call` or `ret` instruction, an exception occurs.
* Because they are no longer needed, we remove the CALLDATA* instructions and the RETURNDATA* instructions.
* Because of security concerns, we remove the CALLCODE and DELEGATECALL instructions.

## Arbitrary Precision Words

Unlike EVM, which uses 32-byte unsigned words, IELE has arbitrary-precision signed words.

* Because words are now explicitly signed, the SDIV, SMOD, SLT, and SGT instructions are removed, and DIV, MOD, LT, and GT are replaced with signed versions.
* An `expmod` instruction is added to perform exponentiation modulo a particular base, and the MODEXP precompiled contract is removed.
* `sext` (corresponding to EVM's SIGNEXTEND) is changed to convert an N-byte twos-complement representation of a signed number into its signed value.
* a `twos` instruction is added to convert a signed integer into an N-byte twos-complement representation of the number.
* Because integers are arbitrary-precision, the integer operand of `byte` (corresponding to EVM's BYTE) now counts from the least-significant byte instead of the most-significant.

## Instructions and Precompiled Contracts

Unlike EVM, where each instruction is just an opcode operating in the values found in the top of the stack, IELE instructions follow a syntax similar to LLVM: each instruction is an opcode followed by some arguments in the form of registers. If the instruction produces a result, it is formed as an assignment where the LHS contains the register(s) to hold the result(s) and the RHS contains the opcode and the arguments. Other changes with respect to EVM include:

* IELE has one integer comparison instruction, namely `cmp`, that accepts various predicates. In addition to the predicates `lt`, `gt`, and `eq`, which have corresponding EVM instructions, the predicates `le`, `ge`, and `ne` are also supported.
* Various EVM instructions that query local and/or network state (e.g. GAS, BALANCE, etc.) have been replaced by IELE builtins that can be called using the `call` instruction (e.g. `%balance = call @iele.balance(%bank.account)`. The names of all IELE builtins start with the prefix `@iele.`, following a similar to the LLVM intrinsics. All names for globals (registers and/or functions) starting with this prefix are reserved: A contract is malformed if it contains user-defined registers and/or functions with names starting with `@iele.`.
* Precompiled contracts in IELE are also invoked using corresponding builtins (e,g, `@iele.ecpairing`).

## Contract Creation

Unlike EVM, both for ease of verification and due to security concerns, we disallow creation of dynamic code for dynamically created contracts, and instead restrict dynamically-created-contracts to code that consists of some previously validated contract.

* The `create` instruction is modified from its EVM counterpart. Instead of taking a memory start and a memory width containing initialization code, the `create` instruction takes a contract name, and an arbitrary number of register arguments to be passed to the `@init` function of the contract to be created. If the contract name is not an externally declared contract name, the contract is malformed.
* Since each externally declared contract should have been defined in the same file and above the contract that externally declares it, the exact code that will be deployed by a `create` instruction is statically known and available.
* The `create` instruction copies the corresponding contract code into the code storage of the account that contains the created contract. Then the `@init` function is invoked in the context of the created account to initialize the contract. It is passed the values in the registers as arguments in the same fashion as a `call .. at` or `staticcall .. at` instruction. If the `@init` function of a contract returns a value, the contract is malformed.
* A separate instruction, `copycreate`, is introduced for the case of copying an entire contract, or duplicating your own contract. The `copycreate` instruction behaves like `create`, except that instead of taking an contract name, it takes a register operand containing an account address, and deploys the entire contract deployed to said account to the newly created account.
* Both instructions, similarly to EVM, return the address of the created contract on success and zero on failure.
* Because we no longer actually need to address portions of code by program counter in order to create contracts, we remove the EVM's CODECOPY and EXTCODECOPY instructions. Because these are also the only remaining instructions which require the code to be addressed by a particular byte offset, we also remove the EVM's PC instruction.
* Transactions that create a contract now explicitly provide the binary data of the contract instead of the binary data of the initialization function. Contracts created in this way are not subject to the restriction that dynamic contract data be inserted, and still invoke the `@init` function to initialize the contract. If the contract is malformed, the transaction fails.

# Designed Changes Relative to LLVM

Here are the IELE design decisions described from the perspective of differences from LLVM.

## Type system

Unlike LLVM, which uses finite-precision words as values of its various integer and floating point types, IELE values are arbitrary-precision words. Moreover, we decided to support initially only one type, `int`, describing arbitrary-precision integer values. This suffices to capture the functionality of EVM in terms of values. In the future we may add a type system supporting aggregate types (such as arrays and structures) as well as (higher-order) function types, similar to the LLVM type system.

## Static Single Assignment (SSA) form

IELE local and global registers are not in Static Single Assignment (SSA) form, meaning that they can be (statically) assigned more than once and that there is no need for IELE phi functions. This departure from the LLVM SSA style was dictated by the fact that IELE, as the target interpreted language for smart contracts, should allow programs to minimize the number of registers they use the consequent gas costs. A compiler that generates IELE can still use an SSA IELE representation internally to ease optimization, but we do not enforce SSA in the final IELE program to allow compiler and/or hand-written code to optimize their register usage.

## Control flow

We decided to relax the LLVM restrictions about basic block structure, where code is organized as a control-flow graph of maximal basic blocks with explicit and statically defined successors/predecessors. In IELE, we maintain static labels as the only allowed targets of jumps, but we do not enforce any particular structure for the code in a function's body. For example, IELE code may fall through from the instruction before a label to a labeled instruction, whereas in LLVM the first instruction of a basic block can only be reached through a jump. We made this decision in order to minimize the size of the code of IELE programs, since these restrictions in LLVM programs usually result in additional branch instructions.

# Partially-Designed Changes

## Memory

Initial plans for memory are to replace it with an arbitrary-width array of arbitrary-length byte buffers, and to create instructions to save memory at either an explicit byte width or as an arbitrary-precision number.

## Gas calculations

We still need to construct a gas model for IELE.

## Failure data
We intend to modify the mechanism by which failure data is returned from CALL, CALLCODE, DELEGATECALL, STATICCALL, CREATE, and COPYCREATE instructions using the REVERT instruction.
