Gas Model
=========

General considerations
----------------------

One change from EVM to IELE is that integers are no longer limited to 256 bits,
being allowd to grow arbitrarily large.
Therefore, the memory required for their storage might become non-neglectable;
for example, numbers could effectively be used to encode sequential memory.

Moreover, IELE allows registers and memory cells to refer to these arbitrarily
large numbers.  To address that, we will assume registers and memory cells
do not (usually) hold actual numbers, but metadata information such as sign,
length in *limbs*, zeroness, and a reference to the memory representation.
At the cost of one extra level of indirection, this would allow for better
memory representation (using allocation pools) and better computation and
resources accounting.
If another value needs to be stored into a register/memory cell, a
memory region large enough to hold the value can be reserved, the memory region
holding the current value can be marked as free, and the metadata structure
could be updated accordingly.
One could even conceive implementations where small enough numbers would be
directly stored in the register/memory cell with indirection being used only
for larger numbers.

To align with the above, the storage representation of a number will consist of
the metadata information followed by the number representation.

Another difference from EVM is that the amount of memory/storage required for
representing the value of a register or memory/storage cell, instead of being
fixed at 256 bits, now varies during its usage.  This was previously only
considered for storage (resetting a stored value to 0 would generate a refund).

To address this variance, we charge memory accesses as EVM, but only charge
memory allocation w.r.t. the peak memory usage.

Another change brought upon by arbitrary length integers is that arithmetic
operations need to take into account the size of the operands.

### Operations with return register

The memory variation introduced by updating the register *r* with value *v*
```
  registerLoadDelta(r, v) = limbLength(v) - registerSize(r)
```

### High level view of the gas model

For each operation there are two costs (for computation and memory),
similarly to the EVM gas model.
* the computation cost --- abstracting the amount of hardware computation
  required to perform the given computation.
* the memory cost --- if the operation accesses/affects memory

Additionally, we also track the variation of the total amount of memory
required store the registers in use.
* the register stack variation --- if the operation saves results to registers,
  these might need to be reallocated, increasing or reducing the
  total amount of memory required for registers.

The current amount of memory required by the register stack is maintained by
the semantics and updated using the *registerLoadDelta* function above.

The peak register stack memory is also maintained and increased whenever the
current register stack level passes the peak level.

If the peak level reaches the register memory allowance, each time it increases
the increment is considered as memory cost and gas is computed accordingly.

### Expressions

For an arithmetic operation we distinguish two variations:
* the gas cost reflecting the complexity of the computation; and
* the variation of memory due to updating the result register.

To make sure that the user has enough allowance for storing the result of the
operation, we estimate the result size (*estimatedResultSize*) and, prior to
executing, we require that there are enough resources (registry allowance,
gas/memory) to contain that size.

#### Bitwise arithmetic
* `NOT rREG wREG`
  - Computation requires all words of `wREG` to be processed, so
    ```hs
    computationCost(NOT rREG wREG) = registerSize(wREG) * wordCost
    ```
  - Size is the same as that of the input
    ```hs
    estimatedResultSize(NOT rREG wREG) = registerSize(wREG)
    ```
* `AND RREG wREG1 wREG2`
  - Computation requires pairwise AND of the registers' words so:
    ```hs
    computationCost(AND rREG wREG1 wREG2) = constBaseAnd * wordCost * m
    ```
  - Size of result is the minimum of the two (as and-ing with 0 yields 0)
    ```hs
    estimatedResultSize(AND rREG wREG1 wREG2) = m
      where m = min (registerSize wREG1) (registerSize wREG2)
    ```
* `OR RREG wREG1 wREG2`
  - Computation requires pairwise OR of the registers' words so:
    ```hs
    computationCost(OR rREG wREG1 wREG2) = constBaseOr * wordCost * m
    ```
  - Size of result is the maximum of the two
    ```hs
    estimatedResultSize(OR rREG wREG1 wREG2) = m
      where m = max (registerSize wREG1) (registerSize wREG2)
    ```
* `XOR RREG wREG1 wREG2`
  - Computation requires pairwise XOR of the registers' words so:
    ```hs
    computationCost(XOR rREG wREG1 wREG2) = constBaseXOr * wordCost * m
    ```
  - Size of result is the maximum of the two
    ```hs
    estimatedResultSize(XOR rREG wREG1 wREG2) = m
      where m = max (registerSize wREG1) (registerSize wREG2)
    ```

#### Comparison operators
* `ISZERO rREG wREG`
  - Computation requires inspecting just one bit of `wREG`:
    ```hs
    computationCost(ISZERO rREG wREG) = bitCost + copyWordCost
    ````
  - Result size is 1
    ```hs
    estimatedResultSize(ISZERO rREG wREG) = 1
    ```
* `CMP RREG wREG1 wREG2` where `CMP` in `[LT, GT, EQ]`
  - Computation requires pairwise comparison of the registers' words so:
    ```hs
    computationCost(LT rREG wREG1 wREG2)
      | registerSize wREG1 /= registerSize wREG2 = lenCost + copyWordCost
      | otherwise                                = constBaseLT * wordCost * registerSize wREG1
    ```
  - Result size is 1
    ```hs
    estimatedResultSize(LT rREG wREG1 wREG2) = 1
    ```

#### Regular arithmetic
* `ADD RREG wREG1 wREG2`
  - Computation requires pairwise addition of the registers' words so:
    ```hs
    computationCost(ADD rREG wREG1 wREG2) = constBaseAdd * wordCost * m
    ```
  - Size of result is about the maximum of the two
    ```hs
    estimatedResultSize(ADD RREG wREG1 wREG2) = m + 1
      where m = max (registerSize wREG1) (registerSize wREG2)
    ```
* `SUB rREG wREG1 wREG2`
  - Subtraction has the same complexity as addition
    ```hs
    computationCost(SUB rREG wREG1 wREG2) = constBaseSub * wordCost * m
    ```
  - Size of result is about the maximum of the two
    ```hs
    estimatedResultSize(SUB rREG wREG1 wREG2) = m + 1
      where m = max (registerSize wREG1) (registerSize wREG2)
    ```
* `MUL rREG wREG1 wREG2`
  -  Multiplication complexity depends on the algorithm used.
    ```hs
    computationCost(MUL rREG wREG1 wREG2)
      | isZero wREG1 || isZero wREG2    = 2 * bitCost + wordCost
      | otherWise                       = mulCost l1 l2
    ```
  - Size of result is the sum of sizes of the operands, except when one of them is 0
    ```hs
    estimatedResultSize(MUL rREG wREG1 wREG2)
      | isZero wREG1 || isZero wREG2 = 0
      | otherwise = l1 + l2
      where l1 = registerSize wREG1
            l2 = registerSize wREG2
    ```
* `DIV rREG wREG1 wREG2`
  - if the value of `wREG2` is 0, or the second is larger than the first,
    then result is 0; otherwise, we approximate the cost to that of the
    basecase division method.
    ```hs
    computationCost(DIV rREG wREG1 wREG2)
      | isZero wREG2 = bitCost + wordCopyCost
      | l2 > l1      = lenCost + wordCopyCost
      | otherwise    = divCost l1 l1
    ```
  - if the result is 0 its size is 1, otherwise it's about the difference
    between sizes
    ```hs
    estimatedResultSize(DIV rREG wREG1 wREG2)
      | isZero wREG2 = 1
      | l2 > l1      = 1
      | otherwise    = l1 - l2 + 1
      where l1 = registerSize wREG1
            l2 = registerSize wREG2
    ```
* `MOD rREG wREG1 wREG2`
  - The complexity of `MOD` is similar to that of `DIV`
    ```hs
    computationCost(MOD rREG wREG1 wREG2)
      | isZero wREG2 = bitCost + wordCopyCost
      | l2 > l1      = lenCost + l1*wordCopyCost
      | otherwise    = divCost l1 l2
    ```
  - if the result is 0 its size is 1, otherwise it's the minimum of the
    operands sizes
    ```hs
    estimatedResultSize(MOD rREG wREG1 wREG2)
      | isZero wREG2 = 1
      | otherwise    = min l1 l2
      where l1 = registerSize wREG1
            l2 = registerSize wREG2
    ```
* `EXP rREG wREG1 wREG2`
  - Exponentiation is done by repeatedly squaring `WREG1` and multiplying those
    intermediate results for the non-zero bits of `WREG2`.
    ```hs
    computationCost(EXP rREG wREG1 wREG2)
      | isZero wREG2 = bitCost + wordCopyCost
      | otherwise    = constExp * wordCost * (0.5 * approxKara l * (approxKara e - 1) + 2 * l * (e - 1)))
      where l = registerSize wREG1
            e = value wREG2
    ```
  - Size of the result is the product between the exponent and the size of the base
    ```hs
    estimatedResultSize(EXP rREG wREG1 wREG2)
      | isZero wREG2 = 1
      | otherwise    = l * e
      where l = registerSize wREG1
            e = value wREG2
    ```

#### Modular arithmetic

* `ADDMOD RREG wREG1 wREG2 wREG3`
  - Computation requires an addition possibly followed by a subtraction.
    ```hs
    computationCost(ADDMOD rREG wREG1 wREG2 wREG3) = constBaseAddMod * wordCost * (m + 1) + divCost (m + 1) l3
    ```
  - Size of result is that of the module
    ```hs
    estimatedResultSize(ADDMOD RREG wREG1 wREG2) = l3
      where m = max (registerSize wREG1) (registerSize wREG2)
            l3 = registerSize wREG3
    ```
* `MULMOD rREG wREG1 wREG2 wREG3`
  -  Multiplication is followed by a mod operation
    ```hs
    computationCost(MULMOD rREG wREG1 wREG2 wREG3)
      | isZero wREG1
      || isZero wREG2  = 2 * bitCost + wordCopyCost
      | l1 + l2 < l3   = cc
      | otherwise      = divCost l1 l3 + divCost l2 l3 + cc + divCost (ml1 + ml2) l3
    ```
  - Size of result is that of the module
    ```hs
    estimatedResultSize(MULMOD rREG wREG1 wREG2 wREG3)
      | isZero wREG1 || isZero wREG2 = 0
      | otherwise = l3
      where l1 = registerSize wREG1
            l2 = registerSize wREG2
            l3 = registerSize wREG3
            ml1 = min l1 l3
            ml2 = min l2 l3
            cc = mulCost ml1 ml2
    ```
* `EXPMOD rREG wREG1 wREG2 wREG3`
  - Exponentiation is done by repeatedly squaring `WREG1` and multiplying those
    intermediate results for the non-zero bits of `WREG2`, all modulo `WREG3`.
    ```hs
    computationCost(EXPMOD rREG wREG1 wREG2 wREG3)
      | isZero wREG2 = bitCost + wordCopyCost
      | otherwise    = divCost l1 l3 + constExpMod * 2 * l2 * (cc + divCost (l3+l3) l3)
      where l1 = registerSize wREG1
            l2 = registerSize wREG2
            l3 = registerSize wREG3
            cc = mulCost l3 l3
    ```
  - Size of the result is the product between the exponent and the size of the base
    ```hs
    estimatedResultSize(EXPMOD rREG wREG1 wREG2)
      | isZero wREG2 = 1
      | otherwise    = l3
      where l3 = registerSize wREG3
    ```

#### SHA3
* `SHA3 rREG wMEMSTART wMEMWIDTH`
  - Computation cost is the similar as for EVM
    ```hs
    computationCost(SHA3 rREG wMEMSTART wMEMWIDTH) = constSHA3  + w * constSHA3Word
      where w = ceiling (value wMEMWIDTH * 8 / limbSize)
    ```
  - Result size is 256 bits
    ```hs
    estimatedResultSize(SHA3 rREG wMEMSTART wMEMWIDTH) = 256 `div` limbSize
    ```
  - Memory expansion is the same as for EVM

#### Byte access
* `BYTE rREG wINDEX w`
  - Getting a byte is done by a simple lookup and some word manipulation
    ```hs
    computationCost(BYTE rREG wINDEX w) = wordCost
    ```
  - The size of a byte is 1
    ```hs
    estimatedResultSize(BYTE rREG wINDEX w) = 1
    ```
* `SIGNOP rREG wN wREG` where `SIGNOP` in `[TWOS, SIGNEXTEND]`
  - Both operation process a maximum of `2^wN` bits
    ```hs
    computationCost(SIGNOP rREG wN wREG) = constTwos * wordCost * l2N
    ```
  - the new size is `2^wN` bits
    ```hs
    estimatedResultSize(TWOS rREG wN wREG) = l2N
      where l2N  = value wN `div` limbSize
    ```

#### Local State
We assume that all operations interrogating the local state have complexity
*localStateCost* and their result fits a machine word.

* `LOCALOP rREG`, where `LOCALOP` in `[GAS, GASPRICE, GASLIMIT, COINBASE, NUMBER, MSIZE, CODESIZE]`
    ```hs
    computationCost(LOCALOP rREG) = localStateCost
    estimatedResultSize(LOCALOP rREG) = 1
    ```
* `LOCALADDROP rREG`, where `LOCALOP` in `[COINBASE, ADDRESS, ORIGIN, CALLER]`
    ```hs
    computationCost(LOCALOP rREG) = localStateCost + addressSize*copyWordCost
    estimatedResultSize(LOCALOP rREG) = addressSize
    ```
* `TIMESTAMP rREG`
    ```hs
    computationCost(LOCALOP rREG) = localStateCost + timeStampSize*copyWordCost
    estimatedResultSize(LOCALOP rREG) = timeStampSize
    ```
* `DIFFICULTY rREG`
    ```hs
    computationCost(LOCALOP rREG) = localStateCost + difficultySize()*copyWordCost
    estimatedResultSize(LOCALOP rREG) = difficultySize()
    ```
* `CALLVALUE rREG`
    ```hs
    computationCost(CALLVALUE rREG) = localStateCost + callValueSize()*copyWordCost
    estimatedResultSize(CALLVALUE rREG) = callValueSize()
    ```
* `BLOCKHASH rREG wN`
  - Computation cost is a constant; result size is *blockHashSize*
    ```hs
    computationCost(CALLVALUE rREG) = blockHashCost + blockHashSize*copyWordCost
    estimatedResultSize(CALLVALUE rREG) = blockHashSize
    ```

#### JUMPs

* `JUMPDEST`
    ```hs
    computationCost(JUMPDEST(_)) = jumpDestCost
    ```
* `JUMP`
    ```hs
    computationCost(JUMP(_)) = jumpCost
    ```
* `JUMPI`
    ```hs
    computationCost(JUMPI(_) cREG) = bitCost + jumpCost
    ```
* `LOCALCALL`
  Save the local context (including the current register stack memory
  requirements), copy the arguments into the new context and jump to
  the call site.

  We're assuming that, in an efficient implementation, saving the caller's
  registers is a small constant time operation. One way or another, an
  efficient implementation will, in the general case, hold the actual register
  data in memory, while, maybe, using machine registers for the machine
  metadata. Regardless of how these optimizations are done, saving the caller
  context and creating the callee context takes
  constantTime + someConstant * registerCount, and a similar amount of memory.

  ```hs
  computationCost(LOCALCALL(_, nARGS, _) _ rARGS) =
    returnAddressSaveCost + byteSetCost * registerMetadataSize * REGISTERS +
    wordCopyCost * (sum [registerSize r | r <- rARGS])
    + jumpCost + callStackDepthCheckCost
  memoryDelta(LOCALCALL(_, nARGS, _) _ rARGS) =
    currentRegisterMemory + sum [registerSize(r) | r <- rARGS]
    + returnAddressSize -- return address
    + registerMetadataSize * REGISTERS -- unused register table
  ```

  `REGISTERS` is the maximum number of registers as declared in the contract
  prefix. TODO: Use the caller's actual register count or callee's max register
  count instead.

* `RETURN`
  Copy values from return registers to caller locations restore local context,
  including (the current register stack memory requirements), mark registers'
  data for reclaiming, and jump back to call site. Note that, since the
  callee registers' data will be reclaimed, copying the return data in the
  callers' registers is not needed; just metadata needs to be copied.
  ```hs
  computationCost(RETURN(nRETURNS) rVALUES) =
    restoreContextCost + metadataCopyCost * nRETURNS + jumpCost
  requiredRegisterMemory(RETURN(nRETURNS) rVALUES) = callerRegisterMemory +
    sum [registerLoadDelta(r, v) | (r, v) <- getReturns() `zip` rVALUES]
  ```

#### `RETURN` (account call version), `INVALID`, `STOP`, `REVERT` and exceptions

A `RETURN` is a an account call one if the call stack is empty.

`INVALID` generates an exception in the callee, whose cost is pre-paid.

`STOP` is an explicit instruction, but reaching the end of a function would
also do an implicit stop. `STOP` has `0` actual return values.

For `STOP`, `RETURN` and `REVERT`: if the actual return values count is
different fron the expected return value count, the caller experiences an
exception, in which case one should use the exception-tagged cost. Note that
in such cases it does not make sense to set the output values and return code
since the exception means that the caller can't handle them.

All the costs below are billed to the caller and not taken out of the callee
gas limit. The `- exceptionCost` part refers to the callee prepaid exception
cost which is being refunded since any possible exception belongs to the caller.

* `INVALID` and exceptions in general
  ```hs
  computationCost(INVALID|exception) = 0
  ```
* `STOP`.
  ```hs
  computationCost(STOP) =
    environmentRestoreCost(with-world) + wordCopyCost * (registersize 1) +
    refundCost + returnValueCountComparisonCost - exceptionCost
  computationCost(exception, STOP) =
    environmentRestoreCost(with-world) + refundCost +
    returnValueCountComparisonCost - exceptionCost
  ```
* `REVERT`
  ```hs
  computationCost(REVERT(nRETURNS) rVALUES) =
    environmentRestoreCost + wordCopyCost * (registersize 0) +
    wordCopyCost * sum [registerSize(r) | r <- rVALUES] +
    returnValueCountComparisonCost - exceptionCost
  computationCost(exception, REVERT(nRETURNS) rVALUES) =
    environmentRestoreCost + returnValueCountComparisonCost - exceptionCost
  ```
* `RETURN`
  ```hs
  computationCost(RETURN(nRETURNS) rVALUES) =
    environmentRestoreCost + wordCopyCost * (registersize 0) +
    wordCopyCost * sum [registerSize(r) | r <- rVALUES] +
    returnValueCountComparisonCost - exceptionCost
  computationCost(exception, RETURN(nRETURNS) rVALUES) =
    environmentRestoreCost + returnValueCountComparisonCost - exceptionCost
  ```

environmentRestoreCost is the cost of re-establishing the callStack, worldState
and subState pointers to what they were before the current contract call.

TODO: Do we count any cost for freeing memory? This occurs at any operation
which resizes a register or a memory value, but it's most obvious at calls,
returns and similar things. I guess that it's better to include the memory
freeing cost in the allocation cost since all memory seems to be freed after the
top call finishes. We should check that this is the case.

TODO:  Figure out more precise costs for these

#### `CALL`, `CALLCODE`, `DELEGATECALL` and `STATICCALL`
* `CALL`, `CALLCODE`, `DELEGATECALL` and `STATICCALL`
  A call is split in several parts. There are some checks which are done before
  everything else (account ammount and call depth), which may finish everything
  with an exception. These (including the exception) have an O(1) cost. Here the
  execution splits in three cases:
  * The called account does not exist -> we continue executing an empty code,
     which has only an empty "deposit" function, without any
     end/return/whatever. TODO: What happens when executing an empty function?
  * The account exists, but the code is not loaded. The code is assumed to be
     in a format suitable for execution. This has a cost in O(code size).
  * There are some special accounts with 'precompiled' code. Calls to these
     accounts are likely to resolve to native calls. These are not handled
     below because they are likely to have completely different cost structure.
     TODO: handle precompiled code.

  After loading the code there are more checks and setup work. The checks
  (the function exists and the argument count matches) can end with an
  exception. These checks are done at the end in the semantics, but I am
  assuming that they can be moved earlier.

  Here are the parameters relevant to `CALL` with a bit more detail than needed.
  * `REG` is the error code.
  * `REGS` are the return registers.
  * `ARGS` are the arguments.
  * `GCAP` is the gas limit.
  * `VALUE` is the amount to transfer.
  * `APPVALUE` can be read by a contract with `CALLVALUE` and, unless specified
               otherwise, is equal to `VALUE`.
  * `ACCTFROM` is the current account, by default making the call and making the
               payments.
  * `ACCTO` is explained below.
  * `ACCTAPPFROM` is relevant for `DELEGATECALL` and it's the account calling
                  the contract which contains `DELEGATECALL`.

Here are the differences between the various calls.

* `CALL` loads the code from `ACCTO`, runs it as `ACCTO` and pays `ACCTO`.
* `CALLCODE` loads the code from `ACCTO` and runs it as `ACCFROM` and pays
             `ACCFROM`.
* `DELEGATECALL` makes the call as `ACCTAPPFROM`, loads the code from `ACCTO`
                 and runs it as `ACCFROM`. Pays `0` to `ACCFROM`, `APPVALUE`
                 is `CALLVALUE`.
* `STATICCALL` loads the code from `ACCTO`, runs it as `ACCTO` and pays `0` to
               `ACCTO`. `APPVALUE` is also `0`.

Below I'll assume that no payment actually happens for `CALLCODE`,
`DELEGATECALL` and `STATICCALL`. I'll also assume that calls to an inexistent
account (which will be created) are executed without an actual call, since
the only available function is an empty 'deposit'.

If the contract code contains a function index when stored, then one does not
need to load the entire code in order to fail with a function check. However,
we should not optimize for the error case, so the question becomes what is
more efficient for the non-error case. That is not trivial to answer, but for
now we can "hide" this cost in the codeLoadingCost/codeByteLoadingCost values,
and we'll make the error case less efficient.
TODO: Clarify codeLoadingCost/codeByteLoadingCost.

Although the caller pays for code loading one way or another, one may argue
that this should be paid from the `GCAP`. However, it seems conceptually easier
to consider that `GCAP` pays for the running cost of the contract, so it will
not include the code loading cost.

Also, it may seem counterintuitive to pay for the code loading cost, one may
expect to pay a flat fee as in the EVM model. A flat fee is usually less
efficient, so we're using a size-based fee. There is a similar memory
consumption associated with loading the code which, arguably, might be included
in a flat-fee cost.

  ```hs
  -- CALLOP in [CALL, CALLCODE, DELEGATECALL, STATICCALL]

  computationCost(early-failing, CALLOP(LABEL,_,_) REG GCAP ACCTTO VALUE REGS ARGS) =
    initialCallCheckCost(CALLOP) + errorCodeSettingCost
  computationCost(not-early-failing, CALLOP(LABEL,_,_) REG GCAP ACCTTO VALUE REGS ARGS) =
    initialCallCheckCost(CALLOP) + computationCost(#callWithCode(....))

  computationCost(inexistent-acccount-late-failure, #callWithCode(...)) =
    accountTypeCheckCost + methodCallCheckCost + errorSettingCost
  computationCost(existent-acccount-late-failure, #callWithCode(...)) =
    accountTypeCheckCost + codeLoadingCost + methodCallCheckCost +
    errorSettingCost

  computationCost(inexistent-account, #callWithCode(...)) =
    accountTypeCheckCost + methodCallCheckCost + newAccountSetupCost(CALLOP)
  computationCost(existent-account, #callWithCode(...)) =
    accountTypeCheckCost + codeLoadingCost + methodCallCheckCost +
    callStateSavingCost + accountTransferCost(CALLOP) + constantCallSetupCost +
    callDataSetupCost + callFeeCost + exceptionCost

  codeLoadingCost = codeByteLoadingCost * codeSize
  callDataSetupCost = wordCopyCost * callDataSize
  callDataSize = sum [registerSize r | r <- rARGS]

  initialCallCheckCost(CALL) = accountValueCheckCost + stackCheckCost
  initialCallCheckCost(CALLCODE|DELEGATECALL|STATICCALL) = stackCheckCost

  accountTransferCost(CALL) = accountTransferCost
  accountTransferCost(CALLCODE|DELEGATECALL|STATICCALL) = 0

  newAccountSetupCost(CALL) = accountCreationCost + accountTransferCost
  newAccountSetupCost(CALLCODE|DELEGATECALL|STATICCALL) = 0

  -- Memory increase/decrease follows a similar pattern

  memoryDelta(early-failling, CALLOP) =
      registerSize 0 - registerSize REG
  memoryDelta(inexistent-account-late-failure) =
      registerSize 0 - registerSize REG
  memoryDelta(inexistent-account-success) = 0
      registerSize 0 - registerSize REG

  -- Temporary memory increase, i.e. the memory increases until the error is
  -- detected, then it goes back to its initial value. I.e. it might be a good
  -- idea to just update the max memory usage (if needed) and not the currently
  -- used memory.
  memorySpikeSize(existent-account-late-failure) = codeSize
  -- The spike is followed by a normal memory delta.
  memoryDelta(existent-account-late-failure) =
      registerSize 0 - registerSize REG

  memoryDeltaCaller(existent-account-success) = codeSize + constantMemorySize
  memorySizeCallee(existent-account-success) = callDataSize
  ```

  The calee starts with a memory usage of memorySizeCallee (absolute, not a
  delta). There is a free memory allowance for each new contract call, equal to
  the maximum EVM stack size, that will be taken into account separately.

  `constantMemorySize` is the size of whatever is part of the saved state
  except for the code and callData (calldepth, callValue, id, gas, caller,
  static)

TODO: I (virgil) think that these costs are more reasonable than some costs
      which follow the semantics in detail. However, we should be able to use
      them inside the semantics, so we should make sure that this is possible.

TODO: CALL loads some data in `<callData>`, but that's not used anywhere.
      Above I included a possible definition (`callDataSetupCost`), but we
      should find out what happens to that and include the right cost.

TODO: Is the callFeeCost fixed or based on the stack size?

TODO: Returning can be either implicit or explicit, should take that into
      account

#### Exception costs

Many things generate exceptions, including being out of gas, which means that
there may not be any gas left to pay for the exception. Therefore it is
preferable that all exception handling costs are pre-paid by `CALL`-like
operations and returned by `RETURN`-like operations. TODO: Make sure that this
actually happens.

```hs
exceptionCost = environmentRestoreCost + wordCopyCost * (registersize 0)
```

#### Builtin calls costs
* `ECREC` - ECDSA public key recovery. http://www.secg.org/sec1-v2.pdf section
  4.1.6

  ```hs
  computationCost(ECREC) = ecrecCost + keyCopyCost  -- 3000 for EVM
  ```
* `SHA256`
  ```hs
  -- 60 + 12 * upper(len(data)/32) for EVM
  computationCost(SHA256(LEN, DATA)) =
    builtinCallConstantCost +
    sha256ConstantCost + sha256ChunkCost * upper(max(len, log256(data))/32) +
    32 * byteCopyCost -- hashCopyCost
  ```
  TODO: can `len` be less than `log256(data)`?
* `RIP160`
  ```hs
  -- 600 + 120 * upper(len(data)/32) for EVM
  computationCost(RIP160(LEN, DATA)) =
    builtinCallConstantCost +
    rip160ConstantCost + rip160ChunkCost * upper(max(len, log256(data))/32) +
    20 * byteCopyCost -- hashCopyCost
  ```
* `ID`
  ```
  -- 15 + 3 * upper(len(data)/32) for EVM
  computationCost(ID(DATA)) =
    builtinCallConstantCost +
    byteCopyCost * log256(DATA)
  ```
* `ECADD`
  ```
  -- 500 in the Byzantium EVM semantics
  computationCost(ECADD(X,Y,Z)) =
    builtinCallConstantCost +
    ecAddComputationCost +
    64 * byteCopyCost -- result, 2x32-byte coordinates
  ```
* `ECMUL`
  ```
  -- 40000 in the Byzantium EVM semantics
  computationCost(ECMUL(X,Y,Z)) =
    builtinCallConstantCost +
    ecMulComputationCost +
    64 * byteCopyCost -- result, 2x32-byte coordinates
  ```
* `ECPAIRING`
  ```
  -- 100000 +Int (#sizeWordStack(DATA) /Int 192) *Int 80000  in the Byzantium EVM semantics
  computationCost(ECPAIRING(DATA)) =
    builtinCallConstantCost +
    ecPairingConstantCost + upper(log256(DATA)/192) * ecPairingChunkCost +
    32 * byteCopyCost -- result copy cost
  ```

#### Logging
We use the same schema from the yellow paper, but taking into account the size
of the logged registers.

* `LOG`
  ```hs
  computationCost(LOG0 rMEMSTART rMEMWIDTH) =
    logCost + value rMEMWIDTH * logDataCost
  computationCost(LOG1 rMEMSTART rMEMWIDTH rW0) =
    logCost + value rMEMWIDTH * logDataCost +
    registerSize rW0 * logTopicWordCost
  computationCost(LOG2 rMEMSTART rMEMWIDTH rw0 rW1) =
    logCost + value rMEMWIDTH * logDataCost +
    registerSize rW0 * logTopicWordCost + registerSize rW1 * logTopicWordCost
  computationCost(LOG3 rMEMSTART rMEMWIDTH rw0 rW1 rW2) =
    logCost + value rMEMWIDTH * logDataCost +
    registerSize rW0 * logTopicWordCost + registerSize rW1 * logTopicWordCost +
    registerSize rW2 * logTopicWordCost
  computationCost(LOG4 rMEMSTART rMEMWIDTH rw0 rW1 rW2 rW3) =
    logCost + value rMEMWIDTH * logDataCost +
    registerSize rW0 * logTopicWordCost + registerSize rW1 * logTopicWordCost +
    registerSize rW2 * logTopicWordCost + registerSize rW3 * logTopicWordCost
  ```

#### Memory operations
* `MLOAD` In order to load the data from the memory, we first need to inspect
  the metadata to make sure we have enough gas to load it.  Hence, we will
  compute some `preComputationCost`, after which we assume to have access to
  the size of the value stored at the given index.
  ```hs
  preComputationCost(MLOAD rREG wINDEX) = mloadWordCost
  computationCost(MLOAD rREG wINDEX) = mLoadCost + mloadWordCost * memoryCellSize(value wIndex)
  estimatedResultSize(MLOAD rREG wINDEX) = memoryCellSize(value wIndex)
  ```
* `MLOADN`
  ```hs
  computationCost(MLOADN rREG, wINDEX1, wINDEX2, wWIDTH) =
    mLoadNCost + mLoadWordCost * wWIDTH
  memoryDelta(MLOADN rREG, wINDEX1, wINDEX2, wWIDTH) =
    wWIDTH - registerSize rREG
  ```
* `MSTORE` when we store a new value over an old one, we compute the difference
  in size between the two values.  Similar to changes to registers, this difference is used to update the
  current memory requirements, and, if it increases, it might update the top
  memory requirement and thus require extra gas
  charge for extra memory.
  ```hs
  computationCost(MSTORE wINDEX wVALUE) =
    mStoreCost + mStoreWordCost * registerSize wVALUE
  memoryCost(MSTORE wINDEX wVALUE) =
    (registerSize wVALUE - storeCellSize(value wIndex))
  ```
* `MSTOREN`
  ```hs
  computationCost(MSTOREN wINDEX1 wINDEX2 wVALUE wWIDTH) =
    mStoreNCost +
    mStoreWordCost * max(0, wINDEX2 - storeCellSize(value wIINDEX1))
    mStoreWordCost * wWIDTH
  memoryDelta(MSTOREN wINDEX1 wINDEX2 wVALUE wWIDTH) =
    wWIDTH + max(0, wINDEX2 - storeCellSize(value wIINDEX1))
  ```

#### Register manipulations
* `MOVE` copies a value from one register to another
  ```hs
  computationcost(MOVE(destREG, sourceREG)) =
      wordCopyCost * registerSize(sourceREG)
  memoryDelta(MOVE(destREG, sourceREG)) =
      registerSize(sourceREG) - registerSize(destREG)
  ```
* `LOADPOS` loads an immediate (positive) value into a register
  ```hs
  computationcost(LOADPOS(destREG, SOURCE)) =
      wordCopyCost * registerSize(SOURCE)
  memoryDelta(LOADPOS(destREG, SOURCE)) =
      registerSize(SOURCE) - registerSize(destREG)
  ```
* `LOADNEG` loads an immediate (positive) value into a register, negating it
  first.

  This assumes either a sign + number representation or a two's complement
  with the source already in that form.

  ```hs
  computationcost(LOADPOS(destREG, SOURCE)) =
      wordCopyCost * registerSize(SOURCE) + wordCost
  memoryDelta(LOADPOS(destREG, SOURCE)) =
      registerSize(SOURCE) - registerSize(destREG)
  ```

#### Account operations

* `BALANCE`
  ```hs
  computationCost(existing-account, BALANCE) = balanceCost
  computationCost(non-existing-account, BALANCE) = newAccountCost + balanceCost
  estimatedResultSize(BALANCE) = balanceSize
  ```
* `EXTCODESIZE`
  ```hs
  computationCost(existing-account, EXTCODESIZE) = extCodeSizeCost
  computationCost(non-existing-account, EXTCODESIZE) = newAccountCost + extCodeSizeCost
  estimatedResultSize(EXTCODESIZE) = extCodeSizeSize
  ```
* `SLOAD` In order to load the data from the storage, we first need to inspect
  the metadata to make sure we have enough gas to load it.  Hence, we will
  compute some `preComputationCost`, after which we assume to have access to
  the size of the value stored at the given index.
  ```hs
  preComputationCost(SLOAD rREG wINDEX) = sloadWordCost
  computationCost(SLOAD rREG wINDEX) = sLoadCost + sloadWordCost * storeCellSize(value wIndex)
  estimatedResultSize(SLOAD rREG wINDEX) = storeCellSize(value wIndex)
  ```
* `SSTORE` when we store a new value over an old one, we compute the difference
  in size between the two values and based on it we decide whether to
  charge for extra storage (if it's positive) or to refund (if it's negative).
  ```hs
  computationCost(SSTORE wINDEX wVALUE) =
    sStoreCost + sStoreWordCost * registerSize wVALUE
  storeCost(SSTORE wINDEX wVALUE) =
    refundableStoreCost * (registerSize wVALUE - storeCellSize(value wIndex))
  ```

Definitions
-----------

* *limbSize* - size in bits of a machine word
  (e.g., 32 for 32-bit architectures, 64 for 64-bit architectures)

* *limbLength(v) = ceil(log(limbSize)(v+1))* - the minimum number
  of machine words required to represent value *v*

* *registerMetadataSize* - size in machine words required for the structure
  holding metadata information for registers
  (e.g., size, sign, pointer to location)

* *registerSize(r)* - machine words allocated for the value of register *r*
  If *r* was not previously used, *registerSize(r) = registerMetadataSize* to
  reflect that, upon allocation, that amount of memory would be additionally
  required.

* *blockHashSize* is the size in machine words of a block hash (256/limbSize)

* *callValueSize()* - the size in machine words of the call value

* Multiplication cost function *mulCost*.
  ```hs
  mulCost l1 l2
    | m < treshBaseMul = constBaseMul * wordCost * l1 * l2
    | otherwise        = constKaratsubaMul * wordCost * (3 * m ^ constKara - 2 * m)
    where m = max l1 l2
  ```

* *constKara = log 3 / log 2* - the exponent of the complexity for the Karatsuba
  algorithm

* `x ^ constKara` can be approximated by quadratic polynomials, successively.
  That is, we want a polynomial a * x ^ 2 + b * x + c
  To choose the approximation points we use the fact that
  `(2^k)^(log 3 / log 2) = 3^k`, so we choose them as successive powers of 2.
  Note that a polynomial is fully determined by 3 approximation points:
  ```
   2^1  2^2  2^3   : a = 1/4, b = 3/2, c = -1
   2^3  2^4  2^5   : a = 9/64, b = 27/8, c = -9
   2^5  2^6  2^7   : a = 81/1024, b = 243/32, c = -81
   2^7  2^8  2^9   : a = 729/16384, b = 2187/128, c = -729
   2^9  2^10 2^11  : a = 6561/262144, b = 19683/512, c = -6561
   2^11 2^12 2^13  : a = 59049/4194304, b = 177147/2048, c = -59049
   ```
   Past 2^13 (sizes larger than 2^13 limbs), we will use the same polynomial
   although we know it quickly diverges, to discourage usage.
   ```hs
     approxKara x
       | 8 <= x < 32 = 9/64*x^2 + 28/8*x - 9                        -- 16%
       | x <= 128     = 81/1024*x^2 + 247/32*x - 81                 -- 20%
       | x <= 512     = 729/16384*x^2 + 2215/128*x - 729            -- 40%
       | x <= 2048    = 6561/262144*x^2 + 19934/512*x - 6561        -- 90%
       | otherwise   = 59049/4194304*x^2 + 177147/2048 * x -59049   -- > 200%
   ```
   Note: I've increased b for all cases to ensure the estimation is always
   larger than the actual value  -  percent on right shows how much larger.

* Division / modulo cost function *divCost*
  ```hs
  divCost l1 l2 = constDiv * wordCost * (l1 - l2) * l2
  ```

### TODOS

* Check that GMP can check for 0 in constant time or update costs accordingly
* Check that GMP can give number of limbs in constant time or update costs accordingly
* Check that all background costs are accounted for (e.g. updating a register's)
  metadata after an assignment.
* Check that memory deltas are used properly everywhere (e.g. MLOAD).
* Bill each use of #newAccount as using account storage space.

### TODOS: Instructions to add

* CREATE
* SELFDESTRUCT
* COPYCREATE

### TODOS: Instructions to consider if they should be added

* REGISTERS
* FUNCTION

### TODOS: Make sure we don't need these

* CALLDATALOAD
* CALLDATASIZE
* CALLDATACOPY
* LOCALRETURN
* LOCALCALLI
* CALLDEST
* EXTCALLDEST
* RETURNDATASIZE
* RETURNDATACOPY
* CODECOPY
* EXTCODECOPY
