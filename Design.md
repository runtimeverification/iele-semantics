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

3. To make it easier to write secure smart contracts.  This includes writing requirements specifications that smart contracts must obey as well as making it easier to develop automated techniques that mathematically verify / prove smart contracts correct wrt to such specifications.  For example, pushing a possibly computed number on the stack and then jumping to it regarded as an address makes verification hard, and thus security weaker, with current smart contract paradigms.  IELE has named labels, like LLVM, and jump statements can only jump to those labels.  Also, avoiding the use of a bounded stack and not having to worry about stack or arithmetic overflow makes specification and verification of smart contracts significantly easier.

Like [KEVM](https://github.com/kframework/evm-semantics), the formal semantics of EVM that we previously defined, validated and evaluated using the [K framework](http://kframework.org), the design of IELE was also done in a semantics-based style, using K. Together with a fast (LLVM-based) execution backend for K that is still under development, it is expected that the interpreter obtained automatically from the semantics of IELE will be sufficiently efficient to serve as a reference implementation of IELE.


# Designed Changes Relative to EVM

Here are the IELE design decisions described from the perspective of differences from EVM.

## Registers

IELE is a register-based bytecode language, unlike EVM, which is a stack-based bytecode. As a result, the following alterations have been made:

* Each instruction in the bytecode takes register operands representing the arguments and result of the instruction.
* A REGISTERS instruction is added to the language. The contract is malformed if REGISTERS is not placed at the beginning of the contract. It sets the bit width of a register operand in the bytecode. If no REGISTERS instruction exists, the bit width defaults to 5 bits
* Because the stack no longer exists, it is no longer possible for the VM to throw an exception due to stack underflow or overflow.
* Because the stack no longer exists, the POP, DUP, SWAP, and PUSH instructions no longer exist. They are replaced with a LOADPOS, LOADNEG, and MOVE instruction. LOADPOS and LOADNEG perform the function of loading an immediate value into a register (see section on arbitrary-precision integers for more details), and MOVE copies a value from one register to another.
* Registers are divided into two categories: global registers and local registers. Half of the declared registers are global; the other half are local.

## Program Structure

Unlike EVM, in which a contract is a sequence of instructions, a IELE contract consists of a header, followed by a data segment, followed by one or more functions.

* See above for description of the REGISTERS instruction, which is currently the only instruction in the program header.
* Following the header is zero or more data segment entries. A data segment entry is either a FUNCTION or CONTRACT instruction (see below). The contract is malformed if a data segment entry appears anywhere but the data segment.
* When a contract is created, each data segment entry is assigned an index starting from one and incrementing by one for each successive entry. Data segment entry 0 does not exist (it represents the function label corresponding to the initialization routine of the contract)
* Following the data segment are one or more functions. A function begins with the CALLDEST or EXTCALLDEST instruction, and continues until the next CALLDEST or EXTCALLDEST instruction, or until the end of the contract. The contract is malformed if a CALLDEST or EXTCALLDEST instruction appears before the end of the data segment.
* A CALLDEST instruction takes an immediate function label, and an immediate number representing the number of arguments to the function. This function is not externally exported. As a special case, function label zero is the initialization routine of the contract. It is an exception if a LOCALCALL instruction invokes this function. If this function is not declared, the contract is malformed.
* A EXTCALLDEST instruction takes an immedate index in the data segment, and an immediate number representing the number of arguments to the function. It is malformed if the data segment index does not refer to a FUNCTION instruction. The FUNCTION instruction provides the external name of the function as its string value. This function is externally exported. The function label of this function is its data segment index.
* The contract is malformed if multiple functions declare the same function label.

## Static Jumps

IELE no longer has dynamic jump instructions. They are replaced with jumps that take an immediate value as an argument, and with a call/return instruction system to allow function calls that return to the caller.

* The JUMPDEST instruction takes an immediate argument representing a jump label.
* The JUMP and JUMPI instructions take an immediate argument representing a jump label instead of a dynamic operand.
* A jump table is computed for each function mapping 16-bit jump labels to the instruction position of the JUMPDEST instruction which declares that label. The contract is malformed if multiple JUMPDEST instructions in the same function have the same label.
* The JUMP and JUMPI instructions jump to the instruction positon in the jump table for their respective jump label.
* Instead of throwing an exception if the argument of JUMP or JUMPI is not the program counter of a JUMPDEST instruction, the contract is malformed if the immediate argument of the JUMP or JUMPI instruction is not in the jump table for the function containing the jump.

## Function Call/Return

IELE function calls take an arbitrary number of register arguments and return values instead of a memory range.

* Add a LOCALCALL instruction. LOCALCALL takes an immediate function label, an arbitrary number of register arguments, and an arbitrary number of register return values, and jumps to the start of the function in the current contract whose data segment index or function label is the same as the immediate operand. It pushes the current instruction position, the values of all local registers, and the register operands of the return values onto the local call stack. The arguments of the call are copied into local registers 0 to N-1 of the function being called.
* change the RETURN and REVERT instructions to take an arbitrary number of register operands. REVERT reverts the contract, but RETURN will return to the instruction position on the top of the local call stack if one exists, restoring the values of local registers and copying the return values into the return value operands of the call instruction. If the local call stack is empty, it returns from the contract.
* change the CALL, CALLCODE, DELEGATECALL, and STATICCALL instructions to take an arbitrary number of register arguments and return values instead of memory ranges. The arguments are copied into local registers 0 to N-1 of the function being called. When the contract returns, the returned values are copied into the return value operands of the call instruction.
* the CALL, CALLCODE, DELEGATECALL, and STATICCALL instructions also take an immediate data segment offset. The contract is malformed if the offset does not refer to a FUNCTION instruction. The instruction contains the name of the function being called.
* if the name of the function being called is not in the data segment for the contract being called, it is an exception. If the name of the function is in the data segment but the function is not exported as external, it is also an exception. Otherwise, the called contract begins at the start of the function whose function label is the data segment offset of the name of the function in the called contract.
* In all cases, if a mismatch occurs between the number of arguments or return values of a function and the matching CALL or RETURN instruction, an exception occurs.
* Because it is no longer needed, we remove the CALLDATA* instructions and the RETURNDATA* instructions.
* Because the function being called is now explicitly part of the signature of calling a contract, we condense the 7 remaining precompiled contracts into a single contract with seven functions.

## Arbitrary Precision Words

Unlike EVM, which uses 32-byte unsigned words, IELE has arbitrary-precision signed words.

* Because words are now explicitly signed, the SDIV, SMOD, SLT, and SGT instructions are removed, and DIV, MOD, LT, and GT are replaced with signed versions.
* An EXPMOD instruction is added to perform exponentiation modulo a particular base, and the MODEXP precompiled contract is removed.
* SIGNEXTEND is changed to convert an N-byte twos-complement representation of a signed number into its signed value.
* a TWOS instruction is added to convert a signed integer into an N-byte twos-complement representation of the number.
* Because integers are arbitrary-precision, the integer operand of BYTE now counts from the least-significant byte instead of the most-significant.

## Contract Creation

Unlike EVM, for ease of verification we disallow creation of dynamic code for dynamically created contracts, and instead restrict dynamically-created-contracts to code that consists of some previously validated contract.

* The CREATE instruction is modified. Instead of taking a memory start and a memory width containing initialization code, the CREATE instruction takes an immediate data segment offset, and an arbitrary number of register arguments to the constructor. If the data segment offset is not the offset of a CONTRACT instruction, the contract is malformed.
* The CONTRACT instruction contains a sequence of instructions in binary form as its string value. If the contract specified is malformed, the contract containing the instruction is malformed.
* The CREATE function copies the contract contained in its respective CONTRACT instruction into the code storage of the account that contains the created contract. Then the function at function label zero is invoked in the context of the created account to initialize the contract. It is passed the values in the registers as arguments in the same fashion as a CALL, CALLCODE, DELEGATECALL, or STATICCALL instruction. If this function returns a value, the contract is malformed.
* A separate instruction, COPYCREATE, is created for the case of copying an entire contract, or duplicating your own contract. The COPYCREATE instruction behaves like CREATE, except that instead of taking an immediate data segment offset, it takes a register operand containing the address, and copies the entire code storage of that contract.
* Both instructions, like before, return the address of the created contract on success and zero on failure.
* Because we no longer actually need to address portions of code by program counter in order to create contracts, we remove the CODECOPY and EXTCODECOPY instructions. Because these are also the only remaining instructions which require the code to be addressed by a particular byte offset, we also remove the PC instruction.
* Transactions that create a contract now explicitly provide the binary data of the contract instead of the binary data of the initialization function. Contracts created in this way are not subject to the restriction that dynamic contract data be inserted, and still invoke the function at function label 0 to initialize the contract. If the contract is malformed, the transaction fails.

## Code Size

The maximum length of a IELE contract is expanded to 65536 bytes.

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
