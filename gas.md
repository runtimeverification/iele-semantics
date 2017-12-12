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

### Operations with return register

The memory variation introduced by updating the register *r* with value *v*
```
  registerLoadDelta(r, v) = limbLength(v) - registerDataSize(r)
```

Another change brought upon by arbitrary length integers is that arithmetic
operations need to take into account the size of the operands.

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
* `EXP rREG wBASE wEXPONENT`
  - Exponentiation is done by repeatedly squaring `wBASE` and multiplying those
    intermediate results for the non-zero bits of `wEXPONENT`.
    This leads to a result of size `lb*e` where `lb` is the length of `wBASE`
    and `e` is the value of `wEXPONENT`.
    ```hs
    computationCost(EXP rREG wBASE wEXPONENT)
      | isZero wEXPONENT = bitCost + wordCopyCost
      | otherwise    = constExp * wordCost * (0.5 * approxKara lb * (approxKara e - 1) + 2 * lb * (e - 1)))
      where lb = registerSize wBASE
            e = value wEXPONENT
    ```
  - Size of the result is the product between the exponent and the size of the base
    ```hs
    estimatedResultSize(EXP rREG wBASE wEXPONENT)
      | isZero wEXPONENT = 1
      | otherwise    = lb * e
      where l = registerSize wBASE
            e = value wEXPONENT
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
  -  Multiplication can be done by first modulo-ing the arguments;
     then `mulModCost l3` computes the cost for multiplying and
     taking modulo again.
    ```hs
    computationCost(MULMOD rREG wREG1 wREG2 wREG3)
      | isZero wREG1
      || isZero wREG2  = 2 * bitCost + wordCopyCost
      | l1 + l2 < l3   = cc
      | otherwise      = divCost l1 l3 + divCost l2 l3 + mulModCost l3
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
  - Exponentiation is done by repeatedly squareing `WREG1` and multiplying those
    intermediate results for the non-zero bits of `WREG2`, all modulo `WREG3`.
    Modulo is taken by computing the `(2^limbSize)^(2*l3) / (value wREG3)` once,
    then performing division by multiplying with the inverse (see `mulModCost0`).
    ```hs
    computationCost(EXPMOD rREG wBASE wEXP wMOD)
      | isZero wREG2 = bitCost + wordCopyCost
      | otherwise    = divCost lBASE lMOD + lMOD * wordCopyCost + divModCost lMOD + 2 * blEXP * (mulModCost0 lMOD) + constExpMod
      where lBASE = registerSize wBASE
            blEXP = limbSize * registerSize wEXP
            lMOD = registerSize wMOD
    ```
  - Size of the result is capped by the size of the modulo
    ```hs
    estimatedResultSize(EXPMOD rREG wBASE wEXP wMOD)
      | isZero wEXP = 1
      | otherwise    = lMOD
      where lMOD = registerSize wMOD
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

* `LOCALOP rREG`, where `LOCALOP` in `[PC, GAS, GASPRICE, GASLIMIT, COINBASE, NUMBER, MSIZE, CODESIZE]`
    ```hs
    computationCost(LOCALOP rREG) = localStateCost
    estimatedResultSize(LOCALOP rREG) = 1
    ```
* `LOCALADDROP rREG`, where `LOCALOP` in `[COINBASE, ADDRESS, ORIGIN, CALLER]`
    ```hs
    computationCost(LOCALOP rREG) = localStateCost + addressSize*copyWordCost
    estimatedResultSize(LOCALOP rREG) = addressSize
    ```
* `TMESTAMP rREG`
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
  ```hs
  computationCost(LOCALCALL(_, nARGS, _) _ rARGS) =
    saveContextCost + wordCopyCost * (sum [registerSize r | r <- rARGS]) + jumpCost
  ```

* `RETURN`
  Copy values from return registers to caller locations restore local context,
  including (the current register stack memory requirements), mark registers'
  data for reclaiming, and jump back to call site. Note that, since the
  callee registers' data will be reclaimed, copying the return data in the
  callers' registers is not needed; just metadata needs to be copied.
  ```hs
  computationCost(RETURN(nRETURNS) rVALUES) =
    restoreContextCost + metadataCopyCost * nRETURNS + jumpCost
  requiredRegisterMemory = callerRegisterMemory +
    sum [registerLoadDelta(r, v) | (r, v) <- getReturns() `zip` rVALUES]
  ```

#### `STOP` and `REVERT`

* `STOP`
  ```hs
  computationCost(STOP) = stopCost() -- defined in terms of the current execution context
  ```
* `REVERT`
  ```hs
  computationCost(REVERT(nRETURNS) rVALUES) = wordCopyCost * registerSize(rValues) +
    revertCost() -- defined in terms of the current execution context.
  ```

TODO:  Figure out more precise costs for these


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



#### Account operations

* `BALANCE`
  ```hs
  computationCost(BALANCE) = balanceCost
  estimatedResultSize(BALANCE) = balanceSize
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

  The Karatsuba method for multiplying two numbers of size `2*n` (base `b`).

  * Split the numbers, into two numbers, each of size `n`
    `x = b^n * x1 + x0,  y = b^n * y1 + y0`
  * Then `x * y = (b^n * x1 + x0) * (b^n * y1 + y0)`, leading to
    `x * y  = b^2n * x1 * y1 + b^n * (x1 * y0 + x2 * y0) + x0 * y0`
  * Note that `x1 * y0 + x0 * y1 = (x1 + x0) * (y1 + y0) - x1 * y1 - x0 * y0`
  * Let `z0 = x0 * y0`, `z2 = x1 * y1`, `t = (x1 + x0) * (y1 + y0)`, and
    `z1 = t - z0 - z2`.
  * Suppose `mul x` is the cost of computing the multiplication of two numbers
    of `x` limbs and `add x` is the cost of computing the addition of two
    numbers of size `x`. Then,
    * `time z0 = time z2 = mul n`, and `size z0 = size z2 = 2 * n`
    * `time (x1 + x0) = time (y1 + y0) = add n` and
      `size (x1 + x0) = size (y1 + y0) = n + 1`
    * `time t = 2 * add n + mul (n + 1)` and `size t = 2 * n + 2`
    * `time z1 = time t + 2 * add (2 * n + 2)` and `size z1 = 2 * n + 2`
    * Since the shifting is done by limbs, we can aggregate the cost for shifting
      into that for addition, and have:

      ```hs
      mul (2*n) = time (x * y) = time z0 + time z1 + time z2 + 2 * add (4 * n) + const0
                 = 2 * mul n + mul (n + 1) + 2 * add n + 2 * add (2 * n + 2) + 2 * add (4 * n) + const0
      size x * y = 4 * n
      ```

  * Using that add is linear, we can assume constants `cAdd1` and `cAdd0` s.t.
    `add x = cAdd1 * x + cAdd0`, we can rewrite `mul (2*n)` to:

    `mul (2*n) = 2 * mul n + mul (n + 1) + 2 * (cAdd1 * n + cAdd0) + 2 * (cAdd1 * (2 * n + 2) + cAdd0) + 2 * (cAdd1 * 4 * n + cAdd0) + const0`, which simplifies to
    ```hs
    mul n = 2 * mul (nd2 1)  + mul (nd2 1 + 1) + n * cAdd1 * 7 + const1
       where nd2 k = ceiling (n / 2 ^ k)
             const1 = const0 +  cAdd0 * 6 + cAdd1 * 4
    ```

    Now, to simplify computation, we approximate `mul (nd2 1)` with `mul (nd2 1 + 1)`,
    and `ceiling ((nd2 1 + 1) / 2)` with `nd2 2`
    ```hs
    mul n = 3 * mul (nd2 1 + 1) + 7 * cAdd1 * n + const1
          = 3 * (3 * mul (nd2 2 + 1) + 7 * cAdd1 * (nd2 1 + 1) + const1) + 7 * cAdd1 * n + const1
    ```
    Let `const2 = 7 * cAdd1 + const1`. Then we can replace `const1` by `const2 - 7 * cAdd1`
    ```hs
    mul n = - 7 * cAdd1 + 7 * cAdd1 * n + const2
                       + (7 * cAdd1 * nd2 1 + const2) * 3 ^ 1
                       + (7 * cAdd1 * nd2 2 + const2) * 3 ^ 2 +
                       .................
                       + (7 * cAdd1 * nd2 k + const2) * 3 ^ k
      where k = log n / log 2
    ```
    By computing the sums, we obtain
    ```hs
    mul n = -7 * cAdd1 + 7 * cAdd1 * n * ((3/2)^(k+1) -1)(3/2 - 1) + const2*(3 ^ (k+1) -1) / 2
    ```
    Considering that `2^k = n` and `3 ^ k = n ^ constKara`, we reduce to
    ```hs
    mul n = 14 * cAdd1 * n * (3/2 * n ^ constKara / n - 1) + const2/2 * (3 * n^constKara - 1) - 7 * cAdd1
          = 21 * n ^ constKara + 3/2 * const2 * n^constKara - 14 * cAdd1 * n - 7 * cAdd1 - const2/2
          = mMul2 * n ^ constKara + mMul1 * n + mMul0
      where mMul2 = 21 + 3 * const2 / 2 = 21 + 3 / 2 * (11 * cAdd1 + 6 * cAdd0 + const0)
            mMul1 = -14 * cadd1
            mMul0 = -7*cAdd1 - const2 / 2 = -1/2 * (25 * cAdd1 + 6 * cAdd0 + const0)
            const2 = 11 * cAdd1 + 6 * cAdd0 + const0
    ```
* Division cost: `a divmod m` when size of `a` is twice the size of `m`, say `n` division
  can be done in a divide and conquer manner, with a complexity of `mul n * log n`
  ```hs
  divModCost n = mul n * (log n / log 2)
  ```
* Multiplication modulo cost. Assuming size of arguments is equal to size of modulo, which is `n`
  ```hs
  mulModCost n = mul n + divCost (2 * n) n = mul n * (1 + log n)
  ```
  When doing multiple multiplications modulo `m` of size `n` for really large `n`,
  one can compute only once the "inverse" `base^(2*n) / m`, with a cost of `divModCost n` as above, followed by
  repeated operations of cost `mulModCost0 n`
  (Barrett reduction [https://en.wikipedia.org/wiki/Barrett_reduction]).
  ```hs
  mulModCost0 n = mul n + mul (n + 1) + 4 * add n
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