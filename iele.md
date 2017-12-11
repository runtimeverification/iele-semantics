IELE Execution
==============

IELE is a register-based abstract machine over some simple opcodes.
Most of the opcodes are "local" to the execution state of the machine, but some of them must interact with the world state.
This file only defines the local execution operations. A separate `cardano.md` will eventually be created to describe the remainder
of the transaction protocol.

```{.k .uiuck .rvk}
requires "data.k"
requires "iele-syntax.k"

module IELE
    imports STRING
    imports IELE-DATA
    imports IELE-COMMON
```

Configuration
-------------

We've broken up the configuration into two components; those parts of the state that mutate during execution of a single transaction and those that are static throughout.
In the comments next to each cell, we explain the purpose of the cell.

```{.k .uiuck .rvk}
    configuration <k> $PGM:IELESimulation </k>                       // Current computation
                  <exit-code exit=""> 1 </exit-code>                 // Exit code of interpreter procses
                  <mode> $MODE:Mode </mode>                          // Execution mode: VMTESTS or NORMAL
                  <schedule> $SCHEDULE:Schedule </schedule>          // Gas Schedule: DEFAULT or ALBE
                  <checkGas> true </checkGas>                        // Enables/disables gas check in test driver

                  // IELE Specific
                  // =============

                  <iele>

                    // Mutable during a single transaction
                    // -----------------------------------

                    <output>        .Ints </output>                  // Return registers of current call frame
                    <callStack>     .List </callStack>               // Inter-contract call stack
                    <interimStates> .List </interimStates>           // Checkpointed network state for rollback
                    <substateStack> .List </substateStack>           // Checkpointed substate for rollback

                    // A single contract call frame
                    // ----------------------------
                    <callFrame>
                      // The loaded state of a IELE program
                      // ----------------------------------
                      <program>
                        <functions>
                          <function multiplicity="*" type="Map">
                            <funcId>       deposit </funcId>         // The name of the function
                            <nparams>      0       </nparams>        // The number of parameters of the function
                            <instructions> (.Instructions .LabeledBlocks):Blocks </instructions> // The blocks of the function
                            <jumpTable>    .Map    </jumpTable>      // Map from jump label to blocks, for branch instruction
                          </function>
                        </functions>
                        <funcIds>     .Set </funcIds>                // Set of all names of functions in <functions> cell
                        <exported>    .Set </exported>               // Set of all names of functions defined with define public
                        <programSize> 0    </programSize>            // Size in bytes of currently loaded contract
                        <contractCode> .Contract </contractCode>     // Disassembled entire contract
                      </program>
                      <callDepth>    0          </callDepth>         // Inter-contract call stack depth
                      <localCalls>   .List      </localCalls>        // Intra-contract call stack 

                      // I_*
                      <id>        0     </id>                         // Currently executing contract
                      <caller>    0     </caller>                     // Contract that called current contract
                      <callData>  .Ints </callData>                   // Copy of register arguments
                      <callValue> 0     </callValue>                  // Value in funds passed to contract

                      // \mu_*
                      <regs>        .Array  </regs>                   // Current values of registers
                      <localMem>    .Memory </localMem>               // Current values of local memory
                      <memoryUsed>  .Map    </memoryUsed>             // Local memory usage
                      <fid>         deposit      </fid>               // Name of currently executing function
                      <gas>         0       </gas>                    // Current gas remaining
                      <previousGas> 0       </previousGas>            // Gas remaining prior to last decrease

                      <static> false </static>                        // Whether the call frame came from a staticcall
                    </callFrame>

                    // A_* (execution substate)
                    <substate>
                      <selfDestruct> .Set  </selfDestruct>            // Set of contract ids that were destroyed by this transaction
                      <logData>  .List </logData>                     // Log entries for this transaction
                      <refund>       0     </refund>                  // Refund for this transaction
                    </substate>

                    // Immutable during a single transaction
                    // -------------------------------------

                    <gasPrice> 0 </gasPrice>                          // Price of gas for this transaction
                    <origin>   0 </origin>                            // Sender of current transactiont 

                    // I_H* (block information)
                    <beneficiary>      0          </beneficiary>      // Miner of current block
                    <difficulty>       0          </difficulty>       // Difficulty of current block
                    <number>           0          </number>           // Number of current block
                    <gasLimit>         0          </gasLimit>         // Gas limit of current block
                    <gasUsed>          0          </gasUsed>          // Gas used by current block
                    <timestamp>        0          </timestamp>        // Timestamp of current block
                    <blockhash>         .List     </blockhash>        // List of previous block's hashes

                  </iele>

                  // IELE Network Layer
                  // ==================
                  // Currently still integrated only with Ethereum Network for backwards-compatibility testing.

                  <network>

                    // Accounts Record
                    // ---------------

                    <activeAccounts> .Map </activeAccounts> // Mapping from account ids to boolean representing whether the account is empty
                    <accounts>
                      <account multiplicity="*" type="Map">
                        <acctID>   0          </acctID>     // ID of account
                        <balance>  0          </balance>    // Balance of funds in account
                        <code>     #emptyCode </code>       // Disassembled contract of account
                        <storage>  .Map       </storage>    // Permanent storage of account (for sload/sstore)
                        <nonce>    0          </nonce>      // Nonce of account
                      </account>
                    </accounts>

                    // Transactions Record
                    // -------------------

                    <txOrder>   .List </txOrder>            // Order of transactions in block
                    <txPending> .List </txPending>          // Remaining transactions in block

                    <messages>
                      <message multiplicity="*" type="Map">
                        <msgID>      0          </msgID>              // Unique ID of transaction
                        <txNonce>    0          </txNonce>            // Nonce of transaction (not checked)
                        <txGasPrice> 0          </txGasPrice>         // Gas price of trasaction
                        <txGasLimit> 0          </txGasLimit>         // Gas limit of transaction
                        <sendto>     .Account   </sendto>             // Destination of transaction (.Account for account creation)
                        <func>       deposit    </func>               // Function to call by transaction
                        <value>      0          </value>              // Value in funds to transfer by transaction
                        <v>          0          </v>                  // Transaction signature (v)
                        <r>          .WordStack </r>                  // Transaction siganture (r)
                        <s>          .WordStack </s>                  // Transaction signature (s)
                        <data>       .WordStack </data>               // Arguments to function called by transaction
                        <args>       .Ints      </args>
                      </message>
                    </messages>

                  </network>

    syntax IELESimulation
 // ---------------------
```

Modal Semantics
---------------

Our semantics is modal, with the initial mode being set on the command line via `-cMODE=EXECMODE`.

-   `NORMAL` executes as a client on the network would.
-   `VMTESTS` skips `call*` and `create*` operations.

```{.k .uiuck .rvk}
    syntax Mode ::= "NORMAL" | "VMTESTS"
```

-   `#setMode_` sets the mode to the supplied one.

```{.k .uiuck .rvk}
    syntax Mode ::= "#setMode" Mode
 // -------------------------------
    rule <k> #setMode EXECMODE => . ... </k> <mode> _ => EXECMODE </mode>
    rule <k> EX:Exception ~> (#setMode _ => .) ... </k>
```

Hardware
--------

The `callStack` cell stores a list of previous VM execution states.

-   `#pushCallStack` saves a copy of VM execution state on the `callStack`.
-   `#popCallStack` restores the top element of the `callStack`.
-   `#dropCallStack` removes the top element of the `callStack`.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#pushCallStack"
 // --------------------------------------
    rule <k> #pushCallStack => . ... </k>
         <callFrame> FRAME </callFrame>
         <callStack> (.List => ListItem(<callFrame> FRAME </callFrame>)) ... </callStack>

    syntax InternalOp ::= "#popCallStack"
 // -------------------------------------
    rule <k> #popCallStack => . ... </k>
         <callFrame> _ => FRAME </callFrame>
         <callStack> (ListItem(<callFrame> FRAME </callFrame>) => .List) ... </callStack>

    syntax InternalOp ::= "#dropCallStack"
 // --------------------------------------
    rule <k> #dropCallStack => . ... </k> <callStack> (ListItem(_) => .List) ... </callStack>
```

The `interimStates` cell stores a list of previous world states.

-   `#pushWorldState` stores a copy of the current accounts at the top of the `interimStates` cell.
-   `#popWorldState` restores the top element of the `interimStates`.
-   `#dropWorldState` removes the top element of the `interimStates`.

```{.k .uiuck .rvk}
    syntax Accounts ::= "{" AccountsCell "|" Map "}"
 // ------------------------------------------------

    syntax InternalOp ::= "#pushWorldState"
 // ---------------------------------------
    rule <k> #pushWorldState => .K ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <accounts> ACCTDATA </accounts>
         <interimStates> (.List => ListItem({ <accounts> ACCTDATA </accounts> | ACCTS })) ... </interimStates>

    syntax InternalOp ::= "#popWorldState"
 // --------------------------------------
    rule <k> #popWorldState => .K ... </k>
         <interimStates> (ListItem({ <accounts> ACCTDATA </accounts> | ACCTS }) => .List) ... </interimStates>
         <activeAccounts> _ => ACCTS </activeAccounts>
         <accounts> _ => ACCTDATA </accounts>

    syntax InternalOp ::= "#dropWorldState"
 // ---------------------------------------
    rule <k> #dropWorldState => . ... </k> <interimStates> (ListItem(_) => .List) ... </interimStates>
```

The `substateStack` cell stores a list of previous substate logs.

-   `#pushSubstate` stores a copy of the current substate at the top of the `substateStack` cell.
-   `#popSubstate` restores the top element of the `substateStack`.
-   `#dropSubstate` removes the top element of the `substateStack`.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#pushSubstate"
 // -------------------------------------
    rule <k> #pushSubstate => .K ... </k>
         <substate> SUBSTATE </substate>
         <substateStack> (.List => ListItem(<substate> SUBSTATE </substate>)) ... </substateStack>

    syntax InternalOp ::= "#popSubstate"
 // ------------------------------------
    rule <k> #popSubstate => .K ... </k>
         <substate> _ => SUBSTATE </substate>
         <substateStack> (ListItem(<substate> SUBSTATE </substate>) => .List) ... </substateStack>

    syntax InternalOp ::= "#dropSubstate"
 // -------------------------------------
    rule <k> #dropSubstate => .K ... </k> <substateStack> (ListItem(_) => .List) ... </substateStack>
```

Simple commands controlling exceptions provide control-flow.

-   `#end` is used to indicate the (non-exceptional) end of execution.
-   `#exception` is used to indicate exceptional states (it consumes any operations to be performed after it).
-   `#revert` is used to indicate the contract terminated with the revert instruction (exceptional return with error message, refunding gas).

```{.k .uiuck .rvk}
    syntax KItem ::= Exception
    syntax Exception ::= "#exception" Int | "#end" | "#revert" Int
 // --------------------------------------------------------------
    rule <k> EX:Exception ~> (_:Int    => .)      ... </k>
    rule <k> EX:Exception ~> (_:Instruction => .) ... </k>
    rule <k> EX:Exception ~> (_:Blocks => .)      ... </k>
    rule <k> EX:Exception ~> (_:InternalOp => .)  ... </k>

    syntax Int ::= "FUNC_NOT_FOUND"
                 | "FUNC_WRONG_SIG"
                 | "CONTRACT_NOT_FOUND"
                 | "USER_ERROR" 
                 | "OUT_OF_GAS" 
                 | "ACCT_COLLISION" 
                 | "OUT_OF_FUNDS" 
                 | "CALL_STACK_OVERFLOW"
 // ------------------------------------
    rule FUNC_NOT_FOUND      => 1 [macro]
    rule FUNC_WRONG_SIG      => 2 [macro]
    rule CONTRACT_NOT_FOUND  => 3 [macro]
    rule USER_ERROR          => 4 [macro]
    rule OUT_OF_GAS          => 5 [macro]
    rule ACCT_COLLISION      => 6 [macro]
    rule OUT_OF_FUNDS        => 7 [macro]
    rule CALL_STACK_OVERFLOW => 8 [macro]
```

Description of registers.

-   Registers begin with `%`
-   Registers are evaluated using heating to the values they contain.
-   `#regRange(N)` generates the registers 0 to N-1.
-   `#sizeRegs(R)` returns the number of regsiters in a list of registers.

```{.k .uiuck .rvk}

    syntax KResult ::= Int
 // ----------------------
    rule <k> % REG:Int => REGS [ REG ] ... </k> <regs> REGS </regs>

    syntax Operands ::= Ints
    syntax NonEmptyOperands ::= Operands
    syntax KResult ::= Ints
 // -----------------------
    rule isKResult(.Operands) => true

    syntax String ::= IeleName2String ( IeleName ) [function, hook(STRING.token2string)]
 // ------------------------------------------------------------------------------------
    rule % NAME:NumericIeleName => % String2Int(IeleName2String(NAME)) requires notBool isInt(NAME)

    syntax LValues ::= #regRange ( Int ) [function]
                     | #regRange ( Int , Int ) [function, klabel(#regRangeAux)]
 // ---------------------------------------------------------------------------
    rule #regRange(N) => #regRange(0, N)
    rule #regRange(_, 0) => .LValues
    rule #regRange(N, 1) => % N , .LValues
    rule #regRange(N, M) => % N , #regRange(N +Int 1, M -Int 1) [owise]

    syntax Int ::= #sizeRegs ( Operands ) [function]
                 | #sizeRegs ( Operands , Int ) [function, klabel(#sizeRegsAux)]
 // ----------------------------------------------------------------------------
    rule #sizeRegs(REGS) => #sizeRegs(REGS, 0)
    rule #sizeRegs(REG , REGS, N) => #sizeRegs(REGS, N +Int 1)
    rule #sizeRegs(.Operands, N) => N
```

Instruction Execution Cycle
---------------------------

-   `#execute` loads the code of the currently-executing function into the `k` cell, where it executes until the function returns.
    It is an exception if we try to execute a function that does not exist.

```{.k .uiuck .rvk}
    syntax KItem ::= "#execute"
 // ---------------------------
    rule <k> #execute => CODE                      ... </k> <fid> FUNC </fid> <funcId> FUNC </funcId> <instructions> CODE </instructions>
    rule <k> #execute => #exception FUNC_NOT_FOUND ... </k> <fid> FUNC </fid> <funcIds> FUNCS </funcIds> requires notBool FUNC in FUNCS
```

Execution follows a simple cycle where first the state is checked for exceptions, then if no exceptions will be thrown the opcode is run.

-   If we reach the end of the function, we execute an implicit `ret void` instruction.

```{.k .uiuck .rvk}
    rule <k> .LabeledBlocks => ret .NonEmptyOperands .Instructions .LabeledBlocks ... </k>
```

-   The rule for sequential composition checks if the instruction is exceptional, runs it if not, then executes the next instruction (assuming
    the instruction doesn't execute a transfer of control flow).

```{.k .uiuck .rvk}
    rule OP::Instruction OPS::Instructions BLOCKS::LabeledBlocks
      => #exceptional? [ OP ] ~> OP
      ~> OPS BLOCKS
```

When execution reaches the end of a block, we fall through to the immediately next block.

```{.k .uiuck .rvk}
    rule .Instructions BLOCKS::LabeledBlocks => BLOCKS
```

### Exceptional Ops

Some checks if an opcode will throw an exception are relatively quick and done up front.

-   `#exceptional?` checks if the operator is invalid.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#exceptional?" "[" Instruction "]"
 // ---------------------------------------------------------
    rule <k> #exceptional? [ OP ] => #invalid? [ OP ] ~> #badJumpDest? [ OP ] ~> #static? [ OP ] ~> #negativeCall? [ OP ] ... </k>
```

-   `#invalid?` checks if it's the designated invalid opcode.

```{.k .uiuck .rvk}
    syntax K ::= "#invalid?" "[" Instruction "]" [function]
 // -------------------------------------------------------
    rule #invalid? [ _ = call @iele.invalid(.Operands) ] => #exception USER_ERROR
    rule #invalid? [ OP ] => . [owise]
```

-   `#badJumpDest?` determines if the opcode will result in a bad jump destination.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#badJumpDest?" "[" Instruction "]"
 // ---------------------------------------------------------
    rule <k> #badJumpDest? [ OP           ] => . ... </k> requires notBool isJumpOp(OP)
    rule <k> #badJumpDest? [ br LABEL     ] => . ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires LABEL in_keys(JUMPS)
    rule <k> #badJumpDest? [ br _ , LABEL ] => . ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires LABEL in_keys(JUMPS)

    rule <k> #badJumpDest? [ br LABEL     ] => #exception USER_ERROR ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires notBool LABEL in_keys(JUMPS)
    rule <k> #badJumpDest? [ br _ , LABEL ] => #exception USER_ERROR ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires notBool LABEL in_keys(JUMPS)

    syntax Bool ::= isJumpOp ( Instruction ) [function]
 // ---------------------------------------------------
    rule isJumpOp(br _) => true
    rule isJumpOp(br _ , _) => true
    rule isJumpOp(...) => false [owise]
```

-   `#static?` determines if the opcode should throw an exception due to the static flag (i.e., an attempt to change state inside a contract called with `staticcall`)

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#static?" "[" Instruction "]"
 // ----------------------------------------------------
    rule <k> #static? [ OP ] => .                     ... </k>                     <static> false </static>
    rule <k> #static? [ OP ] => .                     ... </k> <regs> REGS </regs> <static> true  </static> requires notBool #changesState(OP, REGS)
    rule <k> #static? [ OP ] => #exception USER_ERROR ... </k> <regs> REGS </regs> <static> true  </static> requires         #changesState(OP, REGS)

    syntax Bool ::= #changesState ( Instruction , Array ) [function]
 // ----------------------------------------------------------------
    rule #changesState(log _, _) => true
    rule #changesState(log _ , _, _) => true
    rule #changesState(sstore _ , _, _) => true
    rule #changesState(_ = call _ at _ (_) send VALUE , gaslimit _, REGS) => true requires REGS [ VALUE ] =/=K 0
    rule #changesState(_ , _ = create _ (_) send _, _) => true
    rule #changesState(_ , _ = copycreate _ (_) send _, _) => true
    rule #changesState(selfdestruct _, _) => true
    rule #changesState(...) => false [owise]
```

-    `#negativeCall?` throws an exception if we are making a contract call with negative value or gas limit.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#negativeCall?" "[" Instruction "]"
 // ----------------------------------------------------------
    rule <k> #negativeCall? [ OP ] => . ... </k> requires notBool isAccountCallInst(OP) andBool notBool isCreateInst(OP)

    rule <k> #negativeCall? [ _     = call       _ at _ ( _ ) send % REG1 , gaslimit % REG2 ] => #if {REGS [ REG1 ]}:>Int <Int 0 orBool {REGS [ REG2 ]}:>Int <Int 0 #then #exception USER_ERROR #else . #fi ... </k> <regs> REGS </regs>

    rule <k> #negativeCall? [ _     = staticcall _ at _ ( _ ) gaslimit % REG ] => #if {REGS [ REG ]}:>Int <Int 0 #then #exception USER_ERROR #else . #fi ... </k> <regs> REGS </regs>
    rule <k> #negativeCall? [ _ , _ = create     _ ( _ ) send % REG ]          => #if {REGS [ REG ]}:>Int <Int 0 #then #exception USER_ERROR #else . #fi ... </k> <regs> REGS </regs>
    rule <k> #negativeCall? [ _ , _ = copycreate _ ( _ ) send % REG ]          => #if {REGS [ REG ]}:>Int <Int 0 #then #exception USER_ERROR #else . #fi ... </k> <regs> REGS </regs>
```

### Execution Step

When an instruction reaches the top of the K cell, we first heat its registers, then compute gas cost, then execute the instruction itself.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#exec" Instruction
 // ----------------------------------------
    rule <k> OP:Instruction => #gas [ #addr?(OP) ] ~> #exec #addr?(OP) ... </k> requires isKResult(OP)
```

The following types of instructions do not require any register heating.

```{.k .uiuck .rvk}
    syntax KResult ::= AssignInst
    syntax KResult ::= JumpInst
```

Some instructions require an argument to be interpereted as an address (modulo 160 bits), so the `#addr?` function performs that check.

```{.k .uiuck .rvk}
    syntax Instruction ::= "#addr?" "(" Instruction ")" [function]
 // --------------------------------------------------------------
    rule #addr?(REG = call @iele.balance(W))                                => REG = call @iele.balance(#addr(W))
    rule #addr?(REG = call @iele.extcodesize(W))                            => REG = call @iele.extcodesize(#addr(W))
    rule #addr?(REG1 , REG2 = copycreate W0 (REGS1) send W1)                => REG1 , REG2 = copycreate #addr(W0) (REGS1) send W1
    rule #addr?(selfdestruct W)                                             => selfdestruct #addr(W)
    rule #addr?(REGS1 = call LABEL at W0 (REGS2) send W1 , gaslimit W2)     => REGS1 = call LABEL at #addr(W0) (REGS2) send W1 , gaslimit W2
    rule #addr?(REGS1 = staticcall LABEL at W0 (REGS2) gaslimit W1)         => REGS1 = staticcall LABEL at #addr(W0) (REGS2) gaslimit W1
    rule #addr?(OP)                                                         => OP [owise]
```

-   `#gas` calculates how much gas this operation costs, and takes into account the memory consumed.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#gas" "[" Instruction "]" | "#deductGas" | #deductMemory ( Int )
 // ---------------------------------------------------------------------------------------
    rule <k> #gas [ OP ] => #memory(OP, MU, #memIndex(OP)) ~> #deductMemory(#memIndex(OP)) ~> #gasExec(SCHED, OP) ~> #deductGas ... </k> <memoryUsed> MU </memoryUsed> <schedule> SCHED </schedule>
      requires #usesMemory(OP)
    rule <k> #gas [ OP ] => #gasExec(SCHED, OP) ~> #deductGas ... </k> <gas> GAVAIL </gas> <previousGas> _ => GAVAIL </previousGas> <schedule> SCHED </schedule>
      requires notBool #usesMemory(OP)

    rule <k> MU':Int ~> #deductMemory(_)        => #exception OUT_OF_GAS ... </k> requires MU' >=Int pow256
    rule <k> MU':Int ~> #deductMemory(MEMINDEX) => (Cmem(SCHED, MU [ MEMINDEX <- MU' ]) -Int Cmem(SCHED, MU)) ~> #deductGas ... </k>
         <memoryUsed> MU => MU [ MEMINDEX <- MU' ] </memoryUsed> <schedule> SCHED </schedule>
      requires MU' <Int pow256

    rule <k> G:Int ~> #deductGas => #exception OUT_OF_GAS ... </k> <gas> GAVAIL                  </gas> requires GAVAIL <Int G
    rule <k> G:Int ~> #deductGas => .                     ... </k> <gas> GAVAIL => GAVAIL -Int G </gas> <previousGas> _ => GAVAIL </previousGas> requires GAVAIL >=Int G

    syntax Int ::= Cmem ( Schedule , Map ) [function]
                 | Cmem ( Schedule , Int ) [function, memo, klabel(CmemAux)]
                 | #msize ( Map )          [function]
 // -------------------------------------------------
    rule Cmem(SCHED, MU) => Cmem(SCHED, #msize(MU))
    rule Cmem(SCHED, N)  => (N *Int Gmemory < SCHED >) +Int ((N *Int N) /Int Gquadcoeff < SCHED >)

    rule #msize(_ |-> N MU) => N +Int #msize(MU)
    rule #msize(.Map)       => 0
```

### Substate Log

After executing a transaction, it's necessary to have the effect of the substate log recorded.

-   `#finalizeTx` makes the substate log actually have an effect on the state.

```{.k .uiuck .rvk}
    syntax InternalOp ::= #finalizeTx ( Bool )
 // ------------------------------------------
    rule <k> #finalizeTx(true) => . ... </k>
         <selfDestruct> .Set </selfDestruct>

    rule <k> #finalizeTx(false => true) ... </k>
         <mode> VMTESTS </mode>
         <id> ACCT </id>
         <refund> BAL => 0 </refund>
         <account>
           <acctID> ACCT </acctID>
           <balance> CURRBAL => CURRBAL +Int BAL </balance>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if BAL >Int 0 #then false #else EMPTY #fi) ... </activeAccounts>

    rule <k> (.K => #newAccount MINER) ~> #finalizeTx(_)... </k>
         <mode> NORMAL </mode>
         <beneficiary> MINER </beneficiary>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool MINER in_keys(ACCTS)

    rule <k> #finalizeTx(false) ... </k>
         <mode> NORMAL </mode>
         <gas> GAVAIL => G*(GAVAIL, GLIMIT, REFUND) </gas>
         <refund> REFUND => 0 </refund>
         <txPending> ListItem(MSGID:Int) ... </txPending>
         <message>
            <msgID> MSGID </msgID>
            <txGasLimit> GLIMIT </txGasLimit>
            ...
         </message>
      requires REFUND =/=Int 0

    rule <k> #finalizeTx(false => true) ... </k>
         <mode> NORMAL </mode>
         <origin> ORG </origin>
         <beneficiary> MINER </beneficiary>
         <gas> GAVAIL </gas>
         <refund> 0 </refund>
         <account>
           <acctID> ORG </acctID>
           <balance> ORGBAL => ORGBAL +Int GAVAIL *Int GPRICE </balance>
           ...
         </account>
         <account>
           <acctID> MINER </acctID>
           <balance> MINBAL => MINBAL +Int (GLIMIT -Int GAVAIL) *Int GPRICE </balance>
           ...
         </account>
         <txPending> ListItem(TXID:Int) => .List ... </txPending>
         <message>
           <msgID> TXID </msgID>
           <txGasLimit> GLIMIT </txGasLimit>
           <txGasPrice> GPRICE </txGasPrice>
           ...
         </message>
         <activeAccounts> ... ORG |-> (ORGEMPTY => #if GAVAIL *Int GPRICE >Int 0 #then false #else ORGEMPTY #fi) MINER |-> (MINEMPTY => #if (GLIMIT -Int GAVAIL) *Int GPRICE >Int 0 #then false #else MINEMPTY #fi) ... </activeAccounts>
      requires ORG =/=Int MINER

    rule <k> #finalizeTx(false => true) ... </k>
         <mode> NORMAL </mode>
         <origin> ACCT </origin>
         <beneficiary> ACCT </beneficiary>
         <refund> 0 </refund>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL => BAL +Int GLIMIT *Int GPRICE </balance>
           ...
         </account>
         <txPending> ListItem(MsgId:Int) => .List ... </txPending>
         <message>
           <msgID> MsgId </msgID>
           <txGasLimit> GLIMIT </txGasLimit>
           <txGasPrice> GPRICE </txGasPrice>
           ...
         </message>
         <activeAccounts> ... ACCT |-> (EMPTY => #if GLIMIT *Int GPRICE >Int 0 #then false #else EMPTY #fi) ... </activeAccounts>

    rule <k> #finalizeTx(true) ... </k>
         <selfDestruct> ... (SetItem(ACCT) => .Set) </selfDestruct>
         <activeAccounts> ... (ACCT |-> _ => .Map) </activeAccounts>
         <accounts>
           ( <account>
               <acctID> ACCT </acctID>
               ...
             </account>
          => .Bag
           )
           ...
         </accounts>
```

IELE Programs
=============

Lists of instructions form functions in a contract.

IELE Instructions
-----------------

Each subsection has a different class of instructions.
Organization is based roughly on what parts of the execution state are needed to compute the result of each operator.

### Internal Operations

-   `#newAccount_` allows declaring a new empty account with the given address (and assumes the rounding to 160 bits has already occured).
    If the account already exists with non-zero nonce or non-empty code, an exception is thrown.
    Otherwise, if the account already exists, the storage is cleared.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#newAccount" Int
 // ---------------------------------------
    rule <k> #newAccount ACCT => #exception ACCT_COLLISION ... </k>
         <account>
           <acctID> ACCT  </acctID>
           <code>   CODE  </code>
           <nonce>  NONCE </nonce>
           ...
         </account>
      requires CODE =/=K #emptyCode orBool NONCE =/=K 0

    rule <k> #newAccount ACCT => . ... </k>
         <account>
           <acctID>  ACCT       </acctID>
           <code>    #emptyCode </code>
           <nonce>   0          </nonce>
           <storage> _ => .Map  </storage>
           ...
         </account>

    rule <k> #newAccount ACCT => . ... </k>
         <activeAccounts> ACCTS (.Map => ACCT |-> true) </activeAccounts>
         <accounts>
           ( .Bag
          => <account>
               <acctID>   ACCT       </acctID>
               <balance>  0          </balance>
               <code>     #emptyCode </code>
               <storage>  .Map       </storage>
               <nonce>    0          </nonce>
             </account>
           )
           ...
         </accounts>
      requires notBool ACCT in_keys(ACCTS)
```

-   `#transferFunds` moves money from one account into another, creating the destination account if it doesn't exist.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#transferFunds" Int Int Int
 // --------------------------------------------------
    rule <k> #transferFunds ACCTFROM ACCTTO VALUE => . ... </k>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> ORIGFROM => ORIGFROM -Int VALUE </balance>
           <nonce> NONCE </nonce>
           <code> CODE </code>
           ...
         </account>
         <account>
           <acctID> ACCTTO </acctID>
           <balance> ORIGTO => ORIGTO +Int VALUE </balance>
           ...
         </account>
         <activeAccounts> ... ACCTTO |-> (EMPTY => #if VALUE >Int 0 #then false #else EMPTY #fi) ACCTFROM |-> (_ => ORIGFROM ==Int VALUE andBool NONCE ==Int 0 andBool CODE ==K #emptyCode) ... </activeAccounts>
      requires ACCTFROM =/=K ACCTTO andBool VALUE <=Int ORIGFROM

    rule <k> #transferFunds ACCTFROM ACCTTO VALUE => #exception OUT_OF_FUNDS ... </k>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> ORIGFROM </balance>
           ...
         </account>
      requires VALUE >Int ORIGFROM

    rule <k> (. => #newAccount ACCTTO) ~> #transferFunds ACCTFROM ACCTTO VALUE ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> ORIGFROM </balance>
           ...
         </account>
      requires ACCTFROM =/=K ACCTTO andBool notBool ACCTTO in_keys(ACCTS) andBool VALUE <=Int ORIGFROM

    rule <k> #transferFunds ACCT ACCT VALUE => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <balance> ORIGFROM </balance>
           ...
         </account>
      requires VALUE <=Int ORIGFROM
```

### Invalid Operator

We use an explicit call to `@iele.invalid` both for marking the designated invalid operator and for garbage bytes in the input program.
Executing the INVALID instruction results in an exception.

### Uninitialized Accounts

An account to which code has never been deployed has a size in bytes of zero, but contains an implicit public function `@deposit` which takes
no arguments, returns no values, and does nothing. This function exists to allow accounts to receive payment even if they do not have a contract
deployed to them. Note that a contract can forbid payments by refusing to declare the `@deposit` function, and explicitly raising an exception
if any of its entry points are invoked with a balance transfer.

```{.k .uiuck .rvk}
    syntax Contract ::= "#emptyCode"
    syntax IeleName ::= "Main" [token]
    syntax FunctionParameters ::= Int /* when desugared to just the number of parameters */
 // ---------------------------------------------------------------------------------------
    rule #emptyCode => contract Main !0 { define public @deposit ( 0 ) { ret .NonEmptyOperands .Instructions .LabeledBlocks } } .Contract [macro]
```

### Register Manipulations

Some operators don't calculate anything, they just manipulate the state of registers.

-   Assigning an integer to a register is used to load immediate values into registers.
-   Assigning one register to another copies the value of the source register into the destination.

-  `#loads` loads a list of integers into a list of registers. It is an exception if the number of values do not match.

```{.k .uiuck .rvk}
    rule <k> #exec REG = W:Int => #load REG W ... </k> 

    rule <k> #exec REG1 = % REG2 => #load REG1 { REGS [ REG2 ] }:>Int ... </k> <regs> REGS </regs>

    syntax InternalOp ::= "#load" LValue Int
 // ----------------------------------------
    rule <k> #load % REG VALUE => . ... </k>
         <regs> REGS => REGS [ REG <- VALUE ] </regs>

    syntax InternalOp ::= "#loads" LValues Ints
 // -------------------------------------------
    rule <k> #loads (REG , REGS) (VALUE , VALUES) => #load REG VALUE ~> #loads REGS VALUES ... </k>
    rule <k> #loads .LValues     .Ints            => .K                                    ... </k>
    rule <k> #loads (REG , REGS) .Ints            => #exception FUNC_WRONG_SIG             ... </k>
    rule <k> #loads .LValues     (VALUE , VALUES) => #exception FUNC_WRONG_SIG             ... </k>
```

### Local Memory

These operations are getters/setters of the local execution memory.

-   `REG = load CELL, OFFSET, WIDTH` loads WIDTH bytes starting at OFFSET in the specified memory CELL into REG, interpreted as a signed integer.
-   `REG = load CELL` loads the entire value in CELL into REG, interpreted as a signed integer.
-   `REG = store VALUE, CELL, OFFSET, WIDTH` stores VALUE into the specified memory CELL at OFFSET, interpreting it modulo 256^WIDTH.
-   `REG = store VALUE, CELL` stores VALUE into the specified memory CELL, overwriting the previous value of the entire cell.

```{.k .uiuck .rvk}
    rule <k> #exec REG = load CELL , OFFSET , WIDTH => #load REG #asSigned({LM [ chop(CELL) ]}:>WordStack [ OFFSET .. WIDTH ]) ... </k>
         <localMem> LM </localMem>

    rule <k> #exec REG = load CELL => #load REG #asSigned({LM [ chop(CELL) ]}:>WordStack) ... </k>
         <localMem> LM </localMem>

    rule <k> #exec store VALUE , CELL , OFFSET , WIDTH => . ... </k>
         <localMem> LM => LM [ chop(CELL) <- {LM [ chop(CELL) ]}:>WordStack [ OFFSET := #padToWidth(chop(WIDTH), #asUnsignedBytes(VALUE modInt (2 ^Int (chop(WIDTH) *Int 8)))) ] ] </localMem>

    rule <k> #exec store VALUE , CELL => . ... </k>
         <localMem> LM => LM [ chop(CELL) <- #asSignedBytes(VALUE) ] </localMem>
```

### Expressions

Expression calculations are simple and don't require anything but the arguments from the `regs` to operate.

-   `REG = iszero W` performs boolean negation on W.
-   `REG = not W` performs bitwise negation on W.

-   `REG = add W0, W1` performs arbitrary-precision addition on W0 and W1.
-   `REG = mul W0, W1` performs arbitrary-precision multiplication on W0 and W1.
-   `REG = div W0, W1` performs arbitrary-precision t-division on W0 and W1. It is an exception to divide by zero.
-   `REG = mod W0, W1` performs arbitrary-precision t-modulus on W0 and W1. It is an exception to modulus by zero.

-   `REG = addmod W0, W1, W2` performs addition of W0 and W1 modulo W2. It is an exception to modulus by zero.
-   `REG = mulmod W0, W1, W2` performs multiplication of W0 and W1 modulo W2. It is an exception to modulus by zero.
-   `REG = expmod W0, W1, W2` performs exponentiation of W0 and W1 modulo W2. It is an exception to modulus by zero.

-   `REG = and W0, W1` performs bitwise AND on W0 and W1.
-   `REG = or W0, W1` performs bitwise inclusive OR on W0 and W1.
-   `REG = xor W0, W1` performs bitwise exclusive OR on W0 and W1.

-   `REG = cmp lt W0, W1` computes if W0 is less than W1.
-   `REG = cmp le W0, W1` computes if W0 is less than or equal to W1.
-   `REG = cmp gt W0, W1` computes if W0 is greater than W1.
-   `REG = cmp ge W0, W1` computes if W0 is greater than or equal to W1.
-   `REG = cmp eq W0, W1` computes if W0 is equal to W1.
-   `REG = cmp ne W0, W1` computes if W0 is not equal to W1.

```{.k .uiuck .rvk}
    rule <k> #exec REG = iszero 0 => #load REG 1      ... </k>
    rule <k> #exec REG = iszero W => #load REG 0      ... </k> requires W =/=K 0
    rule <k> #exec REG = not W    => #load REG ~Int W ... </k>

    rule <k> #exec REG = add W0 , W1 => #load REG W0 +Int W1  ... </k>
    rule <k> #exec REG = mul W0 , W1 => #load REG W0 *Int W1  ... </k>
    rule <k> #exec REG = sub W0 , W1 => #load REG W0 -Int W1  ... </k>
    rule <k> #exec REG = div W0 , W1 => #load REG W0 /Int W1  ... </k> requires W1 =/=Int 0
    rule <k> #exec REG = div W0 ,  0 => #exception USER_ERROR ... </k>
    rule <k> #exec REG = exp W0 , W1 => #load REG W0 ^Int W1  ... </k> requires W1 >=Int 0
    rule <k> #exec REG = exp W0 , W1 => #exception USER_ERROR ... </k> requires W1 <Int 0
    rule <k> #exec REG = mod W0 , W1 => #load REG W0 %Int W1  ... </k> requires W1 =/=Int 0
    rule <k> #exec REG = mod W0 ,  0 => #exception USER_ERROR ... </k>

    rule <k> #exec REG = addmod W0 , W1 , W2 => #load REG (W0 +Int W1) %Int W2 ... </k> requires W2 =/=Int 0
    rule <k> #exec REG = addmod W0 , W1 ,  0 => #exception USER_ERROR          ... </k>
    rule <k> #exec REG = mulmod W0 , W1 , W2 => #load REG (W0 *Int W1) %Int W2 ... </k> requires W2 =/=Int 0
    rule <k> #exec REG = mulmod W0 , W1 ,  0 => #exception USER_ERROR          ... </k>
    rule <k> #exec REG = expmod W0 , W1 , W2 => #load REG powmod(W0,W1,W2)     ... </k> requires W2 =/=Int 0 andBool (W1 >=Int 0 orBool gcdInt(W0, W2) ==Int 1)
    rule <k> #exec REG = expmod W0 , W1 ,  0 => #exception USER_ERROR          ... </k>
    rule <k> #exec REG = expmod W0 , W1 , W2 => #exception USER_ERROR          ... </k> requires W1 <Int 0 andBool gcdInt(W0, W2) =/=Int 1

    rule <k> #exec REG = byte INDEX , W => #load REG byte(chop(INDEX), W)       ... </k>
    rule <k> #exec REG = sext WIDTH , W => #load REG signextend(chop(WIDTH), W) ... </k> requires W >=Int 0
    rule <k> #exec REG = sext WIDTH , W => #exception USER_ERROR                ... </k> requires W <Int 0
    rule <k> #exec REG = twos WIDTH , W => #load REG twos(chop(WIDTH), W)       ... </k>

    rule <k> #exec REG = and W0 , W1 => #load REG W0 &Int W1   ... </k>
    rule <k> #exec REG = or  W0 , W1 => #load REG W0 |Int W1   ... </k>
    rule <k> #exec REG = xor W0 , W1 => #load REG W0 xorInt W1 ... </k>

    rule <k> #exec REG = cmp lt W0 , W1 => #load REG 1 ... </k>  requires W0 <Int   W1
    rule <k> #exec REG = cmp lt W0 , W1 => #load REG 0 ... </k>  requires W0 >=Int  W1
    rule <k> #exec REG = cmp le W0 , W1 => #load REG 1 ... </k>  requires W0 <=Int  W1
    rule <k> #exec REG = cmp le W0 , W1 => #load REG 0 ... </k>  requires W0 >Int   W1
    rule <k> #exec REG = cmp gt W0 , W1 => #load REG 1 ... </k>  requires W0 >Int   W1
    rule <k> #exec REG = cmp gt W0 , W1 => #load REG 0 ... </k>  requires W0 <=Int  W1
    rule <k> #exec REG = cmp ge W0 , W1 => #load REG 1 ... </k>  requires W0 >=Int  W1
    rule <k> #exec REG = cmp ge W0 , W1 => #load REG 0 ... </k>  requires W0 <Int   W1
    rule <k> #exec REG = cmp eq W0 , W1 => #load REG 1 ... </k>  requires W0 ==Int  W1
    rule <k> #exec REG = cmp eq W0 , W1 => #load REG 0 ... </k>  requires W0 =/=Int W1
    rule <k> #exec REG = cmp ne W0 , W1 => #load REG 1 ... </k>  requires W0 =/=Int W1
    rule <k> #exec REG = cmp ne W0 , W1 => #load REG 0 ... </k>  requires W0 ==Int  W1

```

### Hashing

The sha3 instruction computes the keccak256 hash of an entire memory cell.

```{.k .uiuck .rvk}

    rule <k> #exec REG = sha3 MEMINDEX => #load REG keccak({LM [ chop(MEMINDEX) ]}:>WordStack) ... </k>
         <localMem> LM </localMem>
```

### Local State

These operators make queries about the current execution state.

-   `REG = call @iele.gas()` returns the gas remaining after executing the current instruction.
-   `REG = call @iele.gasprice()` returns the gas price of the transaction. This will be removed when we migrate to Cardano.
-   `REG = call @iele.gaslimit()` returns the gas limit of the current block.
-   `REG = call @iele.beneficiary()` returns the account who receives payment for mining the current transaction.
-   `REG = call @iele.timestamp()` returns the time stamp of the current block.
-   `REG = call @iele.number()` returns the block number of the current block.
-   `REG = call @iele.difficulty()` returns the difficulty of the current block. This will be removed when we migrate to Cardano.
-   `REG = call @iele.address()` returns the address of the currently executing account.
-   `REG = call @iele.origin()` returns the original sender of the current transaction.
-   `REG = call @iele.caller()` returns the caller of the current contract call.
-   `REG = call @iele.callvalue()` returns the value transfer of the current contract call.
-   `REG = call @iele.msize()` returns the current peak memory usage of the current contract call.
-   `REG = call @iele.codesize()` returns the size in bytes of the currently executing contract. 
-   `REG = call @iele.blockhash(N)` returns the hash of the block header of the Nth previous block.

```{.k .uiuck .rvk}
    rule <k> #exec REG = call @iele.gas      ( .Ints )  => #load REG GAVAIL ... </k> <gas> GAVAIL </gas>
    rule <k> #exec REG = call @iele.gasprice ( .Ints )  => #load REG GPRICE ... </k> <gasPrice> GPRICE </gasPrice>
    rule <k> #exec REG = call @iele.gaslimit ( .Ints )  => #load REG GLIMIT ... </k> <gasLimit> GLIMIT </gasLimit>

    rule <k> #exec REG = call @iele.beneficiary ( .Ints )  => #load REG CB   ... </k> <beneficiary> CB </beneficiary>
    rule <k> #exec REG = call @iele.timestamp   ( .Ints )  => #load REG TS   ... </k> <timestamp> TS </timestamp>
    rule <k> #exec REG = call @iele.number      ( .Ints )  => #load REG NUMB ... </k> <number> NUMB </number>
    rule <k> #exec REG = call @iele.difficulty  ( .Ints )  => #load REG DIFF ... </k> <difficulty> DIFF </difficulty>

    rule <k> #exec REG = call @iele.address   ( .Ints )  => #load REG ACCT ... </k> <id> ACCT </id>
    rule <k> #exec REG = call @iele.origin    ( .Ints )  => #load REG ORG  ... </k> <origin> ORG </origin>
    rule <k> #exec REG = call @iele.caller    ( .Ints )  => #load REG CL   ... </k> <caller> CL </caller>
    rule <k> #exec REG = call @iele.callvalue ( .Ints )  => #load REG CV   ... </k> <callValue> CV </callValue>

    rule <k> #exec REG = call @iele.msize    ( .Ints ) => #load REG 32 *Int #msize(MU) ... </k> <memoryUsed> MU </memoryUsed>
    rule <k> #exec REG = call @iele.codesize ( .Ints ) => #load REG SIZE               ... </k> <programSize> SIZE </programSize>

    rule <k> #exec REG = call @iele.blockhash ( N ) => #load REG #if N >=Int HI orBool HI -Int 256 >Int N orBool N <Int 0 #then 0 #else #parseHexWord(Keccak256(Int2String(N))) #fi ... </k> <number> HI </number> <mode> VMTESTS </mode>
    rule <k> #exec REG = call @iele.blockhash ( N ) => #load REG #blockhash(HASHES, N, HI -Int 1, 0) ... </k> <number> HI </number> <blockhash> HASHES </blockhash> <mode> NORMAL </mode>

    syntax Int ::= #blockhash ( List , Int , Int , Int ) [function]
 // ---------------------------------------------------------------
    rule #blockhash(_, N, HI, _) => 0 requires N >Int HI orBool N <Int 0
    rule #blockhash(_, _, _, 256) => 0
    rule #blockhash(ListItem(0) _, _, _, _) => 0
    rule #blockhash(ListItem(H) _, N, N, _) => H
    rule #blockhash(ListItem(_) L, N, HI, A) => #blockhash(L, N, HI -Int 1, A +Int 1) [owise]
```

### Branch and Local Call

The `br` instruction jumps to a specified label, either unconditionally, or if its argument is nonzero.

The call instruction for local calls (i.e. the form which does not specify an account, value, or gas limit), calls a function in the current contract.
The called function executes in the same contract call frame (i.e. with the same value, gas limit, and memory), but with a fresh set of local registers.
When execution of the callee reaches a `ret` instruction, control returns to the instruction following the call, and local registers are restored.

```{.k .uiuck .rvk}
    rule <k> _:IeleName : INSTRS BLOCKS::LabeledBlocks => INSTRS BLOCKS ... </k>

    rule <k> #exec br LABEL ~> _:Blocks => CODE ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> ... LABEL |-> CODE </jumpTable> </function>

    rule <k> #exec br I:Int , LABEL ~> _:Blocks => CODE ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> ... LABEL |-> CODE </jumpTable> </function> requires I =/=K 0
    rule <k> #exec br 0, LABEL          => .    ... </k>

    syntax LocalCall ::= "{" Blocks "|" IeleName "|" LValues "|" Array "}"
 // ----------------------------------------------------------------------

    rule <k> #exec RETURNS = call @ LABEL ( ARGS ) ~> OPS:Blocks => #loads #regRange(#sizeRegs(ARGS)) ARGS ~> #execute ... </k>
         <fid> FUNC => LABEL </fid>
         <regs> REGS => .Array </regs>
         <localCalls> .List => ListItem({ OPS | FUNC | RETURNS | REGS }) ... </localCalls>
      requires notBool isIeleBuiltin(LABEL)

    syntax Bool ::= isIeleBuiltin(IeleName) [function]
 // --------------------------------------------------
    rule isIeleBuiltin(iele.invalid) => true
    rule isIeleBuiltin(iele.gas) => true
    rule isIeleBuiltin(iele.gasprice) => true
    rule isIeleBuiltin(iele.gaslimit) => true
    rule isIeleBuiltin(iele.beneficiary) => true
    rule isIeleBuiltin(iele.timestamp) => true
    rule isIeleBuiltin(iele.number) => true
    rule isIeleBuiltin(iele.difficulty) => true
    rule isIeleBuiltin(iele.address) => true
    rule isIeleBuiltin(iele.origin) => true
    rule isIeleBuiltin(iele.caller) => true
    rule isIeleBuiltin(iele.callvalue) => true
    rule isIeleBuiltin(iele.msize) => true
    rule isIeleBuiltin(iele.codesize) => true
    rule isIeleBuiltin(iele.blockhash) => true
    rule isIeleBuiltin(iele.balance) => true
    rule isIeleBuiltin(iele.extcodesize) => true
    rule isIeleBuiltin(iele.ecrec) => true
    rule isIeleBuiltin(iele.sha256) => true
    rule isIeleBuiltin(iele.rip160) => true
    rule isIeleBuiltin(iele.id) => true
    rule isIeleBuiltin(iele.ecadd) => true
    rule isIeleBuiltin(iele.ecmul) => true
    rule isIeleBuiltin(iele.ecpairing) => true
    rule isIeleBuiltin( ... ) => false [owise]
```

### `ret` and `revert`

-   `ret` returns the values contained in the specified list of registers to the caller.
    If we are executing inside a previous local call, the contract call frame persists unchanged, and only the instruction position and local register are affected.
    If we are executing at the top-level local call of a contract call frame, we return to the contract call's caller.
-   `revert` returns the values contained in the specified list of registers to the contract call's caller, but signifies that the contract has failed, which rolls back state changes and returns an error code to the caller.

```{.k .uiuck .rvk}
    rule <k> #exec ret VALUES => #end ... </k>
         <output> _ => VALUES </output>
         <localCalls> .List </localCalls>

    rule <k> #exec ret VALUES ~> _:Blocks => #loads RETURNS VALUES ~> OPS ... </k>
         <fid> _ => FUNC </fid>
         <regs> _ => REGS </regs>
         <localCalls> ListItem({ OPS | FUNC | RETURNS | REGS }) => .List ... </localCalls>

    rule <k> #exec revert VALUE => #revert VALUE ... </k>
```

### Log Operations

During execution of a transaction some things are recorded in the substate log.
This is a right cons-list of `SubstateLogEntry` (which contains the account ID along with the specified portions of the `wordStack` and `localMem`).

The `log` instruction logs an entire memory cell to the substate log with zero to four log topics.

```{.k .uiuck .rvk}
    syntax SubstateLogEntry ::= "{" Int "|" WordStack "|" WordStack "}"
 // -------------------------------------------------------------------
```

```{.k .uiuck .rvk}
    rule #exec log MEMINDEX                     => #log MEMINDEX .WordStack
    rule #exec log MEMINDEX , W0                => #log MEMINDEX chop(W0) : .WordStack
    rule #exec log MEMINDEX , W0 , W1           => #log MEMINDEX chop(W0) : chop(W1) : .WordStack
    rule #exec log MEMINDEX , W0 , W1 , W2      => #log MEMINDEX chop(W0) : chop(W1) : chop(W2) : .WordStack
    rule #exec log MEMINDEX , W0 , W1 , W2 , W3 => #log MEMINDEX chop(W0) : chop(W1) : chop(W2) : chop(W3) : .WordStack

    syntax InternalOp ::= "#log" Int WordStack
 // ------------------------------------------
    rule <k> #log MEMINDEX WS => . ... </k>
         <id> ACCT </id>
         <localMem> LM </localMem>
         <logData> ... (.List => ListItem({ ACCT | WS | {LM [ chop(MEMINDEX) ]}:>WordStack })) </logData>
```

Network Ops
-----------

Operators that require access to the rest of the IELE network world-state can be taken as a first draft of a "blockchain generic" language.

### Account Queries

-   `REG = call @iele.balance(ACCT)` returns the balance of the specified account (zero if the account does not exist).
-   `REG = call @iele.extcodesize(ACCT)` returns the code si of the specified account (zero if the account does not exist).

```{.k .uiuck .rvk}
    rule <k> #exec REG = call @iele.balance ( ACCT ) => #load REG BAL ... </k>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>

    rule <k> #exec REG = call @iele.balance ( ACCT ) => #newAccount ACCT ~> #load REG 0 ... </k>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool ACCT in_keys(ACCTS)

    rule <k> #exec REG = call @iele.extcodesize ( ACCT ) => #load REG #contractSize(CODE, #mainContract(CODE)) ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> CODE </code>
           ...
         </account>

    rule <k> #exec REG = call @iele.extcodesize ( ACCT ) => #newAccount ACCT ~> #load REG 0 ... </k>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool ACCT in_keys(ACCTS)
```

### Account Storage Operations

These operations interact with the account storage.

-   `REG = sload INDEX` loads the word at INDEX from the account storage.
-   `sstore VALUE, INDEX` stores the VALUE at INDEX in the account storage.

```{.k .uiuck .rvk}
    rule <k> #exec REG = sload INDEX => #load REG 0 ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account> requires notBool INDEX in_keys(STORAGE)

    rule <k> #exec REG = sload INDEX => #load REG VALUE ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> ... INDEX |-> VALUE ... </storage>
           ...
         </account>

    rule <k> #exec sstore VALUE , INDEX => . ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> ... (INDEX |-> (OLD => VALUE)) ... </storage>
           ...
         </account>
         <refund> R => #if OLD =/=Int 0 andBool VALUE ==Int 0
                        #then R +Int Rsstoreclear < SCHED >
                        #else R
                       #fi
         </refund>
         <schedule> SCHED </schedule>

    rule <k> #exec sstore VALUE , INDEX => . ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE => STORAGE [ INDEX <- VALUE ] </storage>
           ...
         </account>
      requires notBool (INDEX in_keys(STORAGE))
```

### Call Operations

The various `call*` (and other inter-contract control flow) operations will be desugared into these `InternalOp`s.

```{.k .uiuck .rvk}

-   `#checkCall` checks that the current account has the balance necessary to invoke the contract call, and that the contract call stack depth limit has not been reached.
-   `#call_____` takes the calling account, the account to execute as, the account whose code should execute, the gas limit, the amount to transfer, the function to call, and the arguments.
-   `#callWithCode______` takes the calling account, the accout to execute as, the code to execute (as a map), the gas limit, the amount to transfer, the function to call, and the arguments.
-   `#return__` is a placeholder for the calling program, specifying where to place the returned data in registers.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#checkCall" Int Int Int
                        | "#call" Int Int IeleName Int Int Ints Bool
                        | "#callWithCode" Int Int ProgramCell IeleName Int Int Ints Bool
                        | "#mkCall" Int Int ProgramCell IeleName Int Int Ints Bool
 // ----------------------------------------------------------------------------------
    rule <k> #checkCall ACCT VALUE GCAP ~> #call _ _ _ GLIMIT _ _ _ => #refund GLIMIT ~> #pushCallStack ~> #pushWorldState ~> #pushSubstate ~> #exception (#if VALUE >Int BAL #then OUT_OF_FUNDS #else CALL_STACK_OVERFLOW #fi) ... </k>
         <callDepth> CD </callDepth>
         <output> _ => .Ints </output>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>
      requires VALUE >Int BAL orBool CD >=Int 1024

     rule <k> #checkCall ACCT VALUE GCAP => . ... </k>
         <callDepth> CD </callDepth>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>
      requires notBool (VALUE >Int BAL orBool CD >=Int 1024)

    rule <k> #call ACCTFROM ACCTTO FUNC GLIMIT VALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO #precompiled FUNC GLIMIT VALUE ARGS STATIC
         ...
         </k>
         <schedule> SCHED </schedule>
      requires ACCTTO ==Int #precompiledAccount

    rule <k> #call ACCTFROM ACCTTO FUNC GLIMIT VALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO #loadCode(CODE) FUNC GLIMIT VALUE ARGS STATIC
         ...
         </k>
         <schedule> SCHED </schedule>
         <acctID> ACCTTO </acctID>
         <code> CODE </code>
      requires ACCTTO =/=Int #precompiledAccount

    rule <k> #call ACCTFROM ACCTTO FUNC GLIMIT VALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO #loadCode(#emptyCode) FUNC GLIMIT VALUE ARGS STATIC
         ...
         </k>
         <activeAccounts> ACCTS </activeAccounts>
         <schedule> SCHED </schedule>
      requires ACCTTO =/=Int #precompiledAccount andBool notBool ACCTTO in_keys(ACCTS)

    rule #callWithCode ACCTFROM ACCTTO CODE FUNC GLIMIT VALUE ARGS STATIC
      => #pushCallStack ~> #pushWorldState ~> #pushSubstate
      ~> #transferFunds ACCTFROM ACCTTO VALUE
      ~> #mkCall ACCTFROM ACCTTO CODE FUNC GLIMIT VALUE ARGS STATIC

    rule <k> #mkCall ACCTFROM ACCTTO CODE FUNC GLIMIT VALUE ARGS STATIC:Bool
          => #initVM(ARGS) ~> #initFun(FUNC, #sizeRegs(ARGS), false)
         ...
         </k>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => ARGS </callData>
         <callValue> _ => VALUE </callValue>
         <id> _ => ACCTTO </id>
         <gas> _ => GLIMIT </gas>
         <caller> _ => ACCTFROM </caller>
         (<program> _ </program> => CODE:ProgramCell)
         <static> OLDSTATIC:Bool => OLDSTATIC orBool STATIC </static>

```

The VM starts out with empty memory, output, registers, and local call stack.
If the function being called is not public, does not exist, or has the wrong number of arguments, an exception is raised.

```{.k .uiuck .rvk}
    syntax KItem ::= #initVM ( Ints )
                   | #initFun ( IeleName , Int , Bool )
 // ---------------------------------------------------
    rule <k> #initVM(ARGS) => #loads #regRange(#sizeRegs(ARGS)) ARGS ... </k>
         <memoryUsed> _ => .Map    </memoryUsed>
         <output>     _ => .Ints   </output>
         <regs>       _ => .Array  </regs>
         <localMem>   _ => .Memory </localMem>
         <localCalls> _ => .List   </localCalls>

    rule <k> #initFun(LABEL, _, false) => #exception FUNC_NOT_FOUND ... </k>
         <exported> FUNCS </exported>
      requires notBool LABEL in FUNCS

    rule <k> #initFun(LABEL, _, _) => #exception (#if SIZE ==Int 0 #then CONTRACT_NOT_FOUND #else FUNC_NOT_FOUND #fi) ... </k>
         <funcIds> LABELS </funcIds>
         <programSize> SIZE </programSize>
      requires notBool LABEL in LABELS

    rule <k> #initFun(LABEL, NARGS, _) => #exception FUNC_WRONG_SIG ... </k>
         <id> ACCT </id>
         <funcId> LABEL </funcId>
         <nparams> NPARAMS </nparams>
      requires NARGS =/=Int NPARAMS andBool notBool (ACCT ==Int #precompiledAccount andBool LABEL ==K iele.ecpairing)

    rule <k> #initFun(LABEL, NARGS, ISCREATE:Bool) => #if EXECMODE ==K VMTESTS #then #end #else #execute #fi ... </k>
         <mode> EXECMODE </mode>
         <id> ACCT </id>
         <funcIds> ... SetItem(LABEL) </funcIds>
         <exported> FUNCS </exported>
         <fid> _ => LABEL </fid>
         <funcId> LABEL </funcId>
         <nparams> NPARAMS </nparams>
      requires (LABEL in FUNCS orBool ISCREATE) andBool (NPARAMS ==Int NARGS orBool (ACCT ==Int #precompiledAccount andBool LABEL ==K iele.ecpairing))

    syntax KItem ::= "#return" LValues LValue
 // -----------------------------------------
    rule <k> #exception STATUS ~> #return _ REG
          => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG STATUS
         ...
         </k>
         <output> _ => .Ints </output>

    rule <k> #revert OUT ~> #return _ REG
           => #popCallStack
           ~> #popWorldState
           ~> #popSubstate
           ~> #load REG OUT ~> #refund GAVAIL
          ...
         </k>
         <gas> GAVAIL </gas>

    rule <mode> EXECMODE </mode>
         <k> #end ~> #return REGS REG
          => #popCallStack
          ~> #if EXECMODE ==K VMTESTS #then #popWorldState #else #dropWorldState #fi
          ~> #dropSubstate
          ~> #load REG 0 ~> #refund GAVAIL ~> #if EXECMODE ==K VMTESTS #then .K #else #loads REGS OUT #fi
         ...
         </k>
         <output> OUT </output>
         <gas> GAVAIL </gas>

    syntax InternalOp ::= "#refund" Int
 // -----------------------------------
    rule <k> #refund G => . ... </k> <gas> GAVAIL => GAVAIL +Int G </gas>
```

For each `call*` operation, we make a corresponding call to `#call` and a state-change to setup the custom parts of the calling environment.

```{.k .uiuck .rvk}
    rule <k> #exec REG , REGS = call @ LABEL at ACCTTO ( ARGS ) send VALUE , gaslimit GCAP
          => #checkCall ACCTFROM VALUE GCAP
          ~> #call ACCTFROM ACCTTO LABEL Ccallgas(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, VALUE) VALUE ARGS false
          ~> #return REGS REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    rule <k> #exec REG , REGS = staticcall @ LABEL at ACCTTO ( ARGS ) gaslimit GCAP
          => #checkCall ACCTFROM 0 GCAP
          ~> #call ACCTFROM ACCTTO LABEL Ccallgas(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, 0) 0 ARGS true
          ~> #return REGS REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    rule #exec .LValues = call _ at _ ( _ ) send _ , gaslimit _ => #exception FUNC_WRONG_SIG
    rule #exec .LValues = staticcall _ at _ ( _ ) gaslimit _ => #exception FUNC_WRONG_SIG
```

### Account Creation/Deletion

-   `#create____` transfers the endowment to the new account and triggers the execution of the initialization code.
-   `#codeDeposit_` checks the result of initialization code and whether the code deposit can be paid, indicating an error if not.
-   `#checkCreate` checks that the account has sufficient balance for the balance transfer, that the call depth limit is not reached, and increments the nonce of the creator account.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#create" Int Int Int Int Contract Ints
                        | "#mkCreate" Int Int Contract Int Int Ints
                        | "#checkCreate" Int Int
 // --------------------------------------------
    rule <k> #checkCreate ACCT VALUE ~> #create _ _ GAVAIL _ _ _ _ => #refund GAVAIL ~> #pushCallStack ~> #pushWorldState ~> #pushSubstate ~> #exception (#if VALUE >Int BAL #then OUT_OF_FUNDS #else CALL_STACK_OVERFLOW #fi) ... </k>
         <callDepth> CD </callDepth>
         <output> _ => .Ints </output>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>
      requires VALUE >Int BAL orBool CD >=Int 1024

    rule <k> #checkCreate ACCT VALUE => . ... </k>
         <mode> EXECMODE </mode>
         <callDepth> CD </callDepth>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           <nonce> NONCE => #if EXECMODE ==K VMTESTS #then NONCE #else NONCE +Int 1 #fi </nonce>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if EXECMODE ==K VMTESTS #then EMPTY #else false #fi) ... </activeAccounts>
      requires notBool (VALUE >Int BAL orBool VALUE <Int 0 orBool CD >=Int 1024)

    rule #create ACCTFROM ACCTTO GAVAIL VALUE CODE ARGS
      => #pushCallStack ~> #pushWorldState ~> #pushSubstate
      ~> #newAccount ACCTTO
      ~> #transferFunds ACCTFROM ACCTTO VALUE
      ~> #mkCreate ACCTFROM ACCTTO CODE GAVAIL VALUE ARGS

    rule <mode> EXECMODE </mode>
         <k> #mkCreate ACCTFROM ACCTTO CODE GAVAIL VALUE ARGS
          => #initVM(ARGS) ~> #initFun(init, #sizeRegs(ARGS), true)
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT => ACCTTO </id>
         <gas> OLDGAVAIL => GAVAIL </gas>
         (<program> _ </program> => #loadCode(CODE))
         <caller> _ => ACCTFROM </caller>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => .Ints </callData>
         <callValue> _ => VALUE </callValue>
         <account>
           <acctID> ACCTTO </acctID>
           <nonce> NONCE => NONCE +Int 1 </nonce>
           ...
         </account>
         <activeAccounts> ... ACCTTO |-> (EMPTY => false) ... </activeAccounts>

    syntax Contract ::= #subcontract ( Contract , IeleName ) [function]
 // -------------------------------------------------------------------
    rule #subcontract ( (contract NAME ! _ { _ } #as CONTRACT) _, NAME ) => CONTRACT .Contract
    rule #subcontract ( CONTRACT CONTRACTS, NAME ) => CONTRACT #subcontract(CONTRACTS, NAME) [owise]

    syntax KItem ::= "#codeDeposit" Int Int Contract LValue LValue Bool
                   | "#mkCodeDeposit" Int Int Contract LValue LValue Bool
                   | "#finishCodeDeposit" Int Contract LValue LValue
 // ----------------------------------------------------------------
    rule <k> #exception STATUS ~> #codeDeposit _ _ _ REG _ _ => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG STATUS ... </k> <output> _ => .Ints </output>
    rule <k> #revert OUT ~> #codeDeposit _ _ _ REG _ _ => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ~> #load REG OUT ... </k>
         <gas> GAVAIL </gas>

    rule <mode> EXECMODE </mode>
         <k> #end ~> #codeDeposit ACCT LEN CODE STATUS ACCTOUT NEW => #mkCodeDeposit ACCT LEN CODE STATUS ACCTOUT NEW ... </k>

    rule <k> #mkCodeDeposit ACCT LEN CODE STATUS ACCTOUT NEW:Bool
          => #if EXECMODE ==K VMTESTS orBool notBool NEW #then . #else Gcodedeposit < SCHED > *Int LEN ~> #deductGas #fi
          ~> #finishCodeDeposit ACCT CODE STATUS ACCTOUT
         ...
         </k>
         <mode> EXECMODE </mode>
         <schedule> SCHED </schedule>
         <output> .Ints </output>

    rule <k> #finishCodeDeposit ACCT CODE STATUS ACCTOUT
          => #popCallStack ~> #if EXECMODE ==K VMTESTS #then #popWorldState #else #dropWorldState #fi ~> #dropSubstate
          ~> #refund GAVAIL ~> #load STATUS 0 ~> #load ACCTOUT ACCT
         ...
         </k>
         <mode> EXECMODE </mode>
         <gas> GAVAIL </gas>
         <account>
           <acctID> ACCT </acctID>
           <code> _ => CODE </code>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if CODE =/=K #emptyCode #then false #else EMPTY #fi) ... </activeAccounts>

    rule <k> #exception STATUS ~> #finishCodeDeposit _ _ REG _ => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG STATUS ... </k>
```

-   `create` will attempt to `#create` the named contract using the initialization code and cleans up the result with `#codeDeposit`.
-   `copycreate` will attempt to `#create` a copy of the contract at the specified address using the initialization code and cleans up the result with `#codeDeposit`

```{.k .uiuck .rvk}
    rule <k> #exec STATUS , ACCTOUT = create NAME ( ARGS ) send VALUE
          => #checkCreate ACCT VALUE
          ~> #create ACCT #newAddr(ACCT, NONCE) #if Gstaticcalldepth << SCHED >> #then GAVAIL #else #allBut64th(GAVAIL) #fi VALUE #subcontract(CODE, NAME) ARGS
          ~> #codeDeposit #newAddr(ACCT, NONCE) #contractSize(CODE, NAME) #subcontract(CODE, NAME) STATUS ACCTOUT false
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <gas> GAVAIL => #if Gstaticcalldepth << SCHED >> #then 0 #else GAVAIL /Int 64 #fi </gas>
         <account>
           <acctID> ACCT </acctID>
           <nonce> NONCE </nonce>
           ...
         </account>
         <contractCode> CODE </contractCode>

    rule <k> #exec STATUS , ACCTOUT = copycreate ACCTCODE ( ARGS ) send VALUE
          => #checkCreate ACCT VALUE
          ~> #create ACCT #newAddr(ACCT, NONCE) #if Gstaticcalldepth << SCHED >> #then GAVAIL #else #allBut64th(GAVAIL) #fi VALUE CODE ARGS
          ~> #codeDeposit #newAddr(ACCT, NONCE) #contractSize(CODE, #mainContract(CODE)) CODE STATUS ACCTOUT false
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <gas> GAVAIL => #if Gstaticcalldepth << SCHED >> #then 0 #else GAVAIL /Int 64 #fi </gas>
         <account>
           <acctID> ACCT </acctID>
           <nonce> NONCE </nonce>
           ...
         </account>
         <account>
           <acctID> ACCTCODE </acctID>
           <code> CODE </code>
           ...
         </account>
         requires ACCT =/=Int ACCTCODE

    rule <k> #exec STATUS , ACCTOUT = copycreate ACCT ( ARGS ) send VALUE
          => #checkCreate ACCT VALUE
          ~> #create ACCT #newAddr(ACCT, NONCE) #if Gstaticcalldepth << SCHED >> #then GAVAIL #else #allBut64th(GAVAIL) #fi VALUE CODE ARGS
          ~> #codeDeposit #newAddr(ACCT, NONCE) #contractSize(CODE, #mainContract(CODE)) CODE STATUS ACCTOUT false
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <gas> GAVAIL => #if Gstaticcalldepth << SCHED >> #then 0 #else GAVAIL /Int 64 #fi </gas>
         <account>
           <acctID> ACCT </acctID>
           <nonce> NONCE </nonce>
           <code> CODE </code>
           ...
         </account>

    rule <k> (.K => #newAccount ACCT) ~> #exec _ , _ = copycreate ACCT ( _ ) send _ ... </k> <activeAccounts> ACCTS </activeAccounts> requires notBool ACCT in_keys(ACCTS)
```

`selfdestruct` marks the current account for deletion and transfers funds out of the current account.
Self destructing to yourself, unlike a regular transfer, destroys the balance in the account, irreparably losing it.

```{.k .uiuck .rvk}
    rule <k> #exec selfdestruct ACCTTO => #transferFunds ACCT ACCTTO BALFROM ~> #end ... </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <selfDestruct> SDS (.Set => SetItem(ACCT)) </selfDestruct>
         <refund> RF => #if ACCT in SDS #then RF #else RF +Int Rselfdestruct < SCHED > #fi </refund>
         <account>
           <acctID> ACCT </acctID>
           <balance> BALFROM </balance>
           ...
         </account>
         <output> _ => .Ints </output>
      requires ACCT =/=Int ACCTTO

    rule <k> #exec selfdestruct ACCT => #end ... </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <selfDestruct> SDS (.Set => SetItem(ACCT)) </selfDestruct>
         <refund> RF => #if ACCT in SDS #then RF #else RF +Int Rselfdestruct < SCHED > #fi </refund>
         <account>
           <acctID> ACCT </acctID>
           <balance> BALFROM => 0 </balance>
           <nonce> NONCE </nonce>
           <code> CODE </code>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (_ => NONCE ==Int 0 andBool CODE ==K #emptyCode) ... </activeAccounts>
         <output> _ => .Ints </output>

```

Precompiled Contract
====================

-   `#precompiled` is a placeholder for the pre-compiled contracts at addresses 1.

```{.k .uiuck .rvk}
    syntax Int ::= "#precompiledAccount" [function]
 // -----------------------------------------------
    rule #precompiledAccount => 1

    syntax ProgramCell ::= "#precompiled" [function]
 // ------------------------------------------------
    rule #precompiled =>
         <program>
           <functions>
             <function> <funcId> iele.ecrec     </funcId> <instructions> ECREC     .Instructions .LabeledBlocks </instructions> <nparams> 4 </nparams> ... </function>
             <function> <funcId> iele.sha256    </funcId> <instructions> SHA256    .Instructions .LabeledBlocks </instructions> <nparams> 2 </nparams> ... </function>
             <function> <funcId> iele.rip160    </funcId> <instructions> RIP160    .Instructions .LabeledBlocks </instructions> <nparams> 2 </nparams> ... </function>
             <function> <funcId> iele.id        </funcId> <instructions> ID        .Instructions .LabeledBlocks </instructions> <nparams> 1 </nparams> ... </function>
             <function> <funcId> iele.ecadd     </funcId> <instructions> ECADD     .Instructions .LabeledBlocks </instructions> <nparams> 4 </nparams> ... </function>
             <function> <funcId> iele.ecmul     </funcId> <instructions> ECMUL     .Instructions .LabeledBlocks </instructions> <nparams> 3 </nparams> ... </function>
             <function> <funcId> iele.ecpairing </funcId> <instructions> ECPAIRING .Instructions .LabeledBlocks </instructions> <nparams> 6 </nparams> ... </function>
           </functions>
           <funcIds>
             SetItem(iele.ecrec)
             SetItem(iele.sha256)
             SetItem(iele.rip160)
             SetItem(iele.id)
             SetItem(iele.ecadd)
             SetItem(iele.ecmul)
             SetItem(iele.ecpairing)
           </funcIds>
           <exported>
             SetItem(iele.ecrec)
             SetItem(iele.sha256)
             SetItem(iele.rip160)
             SetItem(iele.id)
             SetItem(iele.ecadd)
             SetItem(iele.ecmul)
             SetItem(iele.ecpairing)
           </exported>
           ...
         </program>
```

-   `@iele.ecrec` performs ECDSA public key recovery.
-   `@iele.sha256` performs the SHA2-256 hash function.
-   `@iele.rip160` performs the RIPEMD-160 hash function.
-   `@iele.id` is the identity function (copies input to output).
-   `@iele.ecadd` is the BN128 elliptic curve addition function.
-   `@iele.ecmul` is the BN128 elliptic curve scalar multiplication function.
-   `@iele.ecpairing` is the BN128 elliptic curve pairing check function.

```{.k .uiuck .rvk}
    syntax Instruction ::= PrecompiledOp
    syntax KResult ::= PrecompiledOp

    syntax PrecompiledOp ::= "ECREC"
 // --------------------------------
    rule <k> #exec ECREC => #end ... </k>
         <callData> HASH , V , R , S , .Ints </callData>
         <output> _ => #ecrec(#sender(#unparseByteStack(#padToWidth(32, #asUnsignedBytes(HASH))), V, #unparseByteStack(#padToWidth(32, #asUnsignedBytes(R))), #unparseByteStack(#padToWidth(32, #asUnsignedBytes(S))))) </output>
         requires HASH >=Int 0 andBool V >=Int 0 andBool R >=Int 0 andBool S >=Int 0

    rule <k> #exec ECREC => #exception USER_ERROR ... </k>
         <callData> HASH , V , R , S , .Ints </callData>
         requires notBool (HASH >=Int 0 andBool V >=Int 0 andBool R >=Int 0 andBool S >=Int 0)

    syntax Ints ::= #ecrec ( Account ) [function]
 // ---------------------------------------------
    rule #ecrec(.Account) => -1 , .Ints
    rule #ecrec(N:Int)    =>  N , .Ints

    syntax PrecompiledOp ::= "SHA256"
 // ---------------------------------
    rule <k> #exec SHA256 => #end ... </k>
         <callData> LEN , DATA , .Ints </callData>
         <output> _ => #parseHexWord(Sha256(#unparseByteStack(#padToWidth(LEN, #asUnsignedBytes(DATA))))) , .Ints </output>
         requires LEN >=Int 0 andBool DATA >=Int 0

    rule <k> #exec SHA256 => #exception USER_ERROR ... </k>
         <callData> LEN , DATA , .Ints </callData>
         requires notBool (LEN >=Int 0 andBool DATA >=Int 0)

    syntax PrecompiledOp ::= "RIP160"
 // ---------------------------------
    rule <k> #exec RIP160 => #end ... </k>
         <callData> LEN , DATA , .Ints </callData>
         <output> _ => #parseHexWord(RipEmd160(#unparseByteStack(#padToWidth(LEN, #asUnsignedBytes(DATA))))) , .Ints </output>
         requires LEN >=Int 0 andBool DATA >=Int 0

    rule <k> #exec RIP160 => #exception USER_ERROR ... </k>
         <callData> LEN , DATA , .Ints </callData>
         requires notBool (LEN >=Int 0 andBool DATA >=Int 0)

    syntax PrecompiledOp ::= "ID"
 // -----------------------------
    rule <k> #exec ID => #end ... </k>
         <callData> DATA </callData>
         <output> _ => DATA </output>

    syntax PrecompiledOp ::= "ECADD"
 // --------------------------------
    rule <k> #exec ECADD => #ecadd((X1, Y1), (X2, Y2)) ... </k>
         <callData> X1 , Y1 , X2 , Y2 , .Ints </callData>

    syntax InternalOp ::= #ecadd(G1Point, G1Point)
 // ----------------------------------------------
    rule #ecadd(P1, P2) => #exception USER_ERROR
      requires notBool isValidPoint(P1) orBool notBool isValidPoint(P2)
    rule <k> #ecadd(P1, P2) => #end ... </k> <output> _ => #point(BN128Add(P1, P2)) </output>
      requires isValidPoint(P1) andBool isValidPoint(P2)

    syntax PrecompiledOp ::= "ECMUL"
 // --------------------------------
    rule <k> #exec ECMUL => #ecmul((X, Y), S) ... </k>
         <callData> X , Y , S , .Ints </callData>

    syntax InternalOp ::= #ecmul(G1Point, Int)
 // ------------------------------------------
    rule #ecmul(P, S) => #exception USER_ERROR
      requires notBool isValidPoint(P)
    rule <k> #ecmul(P, S) => #end ... </k> <output> _ => #point(BN128Mul(P, S)) </output>
      requires isValidPoint(P)

    syntax Ints ::= #point(G1Point) [function]
 // ------------------------------------------
    rule #point((X, Y)) => X , Y , .Ints

    syntax PrecompiledOp ::= "ECPAIRING"
 // ------------------------------------
    rule <k> #exec ECPAIRING => #ecpairing(.List, .List, DATA) ... </k>
         <callData> DATA </callData>
      requires #sizeRegs(DATA) %Int 6 ==Int 0
    rule <k> ECPAIRING => #exception FUNC_WRONG_SIG ... </k>
         <callData> DATA </callData>
      requires #sizeRegs(DATA) %Int 6 =/=Int 0

    syntax InternalOp ::= #ecpairing(List, List, Ints)
 // --------------------------------------------------
    rule (.K => #checkPoint) ~> #ecpairing((.List => ListItem((X, Y)::G1Point)) _, (.List => ListItem((A x B , C x D))) _, (X , Y , A , B , C , D , REGS => REGS))
    rule <k> #ecpairing(A, B, .Ints) => #end ... </k>
         <output> _ => bool2Word(BN128AtePairing(A, B)) , .Ints </output>

    syntax InternalOp ::= "#checkPoint"
 // -----------------------------------
    rule (#checkPoint => .) ~> #ecpairing(ListItem(AK::G1Point) _, ListItem(BK::G2Point) _, _)
      requires isValidPoint(AK) andBool isValidPoint(BK)
    rule #checkPoint ~> #ecpairing(ListItem(AK::G1Point) _, ListItem(BK::G2Point) _, _) => #exception USER_ERROR
      requires notBool isValidPoint(AK) orBool notBool isValidPoint(BK)
```

IELE Gas Calculation
====================

The gas calculation is designed to mirror the style of the yellowpaper.
Gas is consumed either by increasing the amount of memory being used, or by executing opcodes.

Memory Consumption
------------------

Memory consumed is tracked to determine the appropriate amount of gas to charge for each operation.
In the yellowpaper, each opcode is defined to consume zero gas unless specified otherwise next to the semantics of the opcode (appendix H).

-   `#memory` computes the new memory size given the old size and next operator (with its arguments).
-   `#memoryUsageUpdate` is the function `M` in appendix H of the yellowpaper which helps track the memory used.

```{.k .uiuck .rvk}
    syntax Int ::= #memory ( Instruction , Map , Int ) [function]
                 | #memIndex ( Instruction )           [function]
    syntax Bool ::= #usesMemory ( Instruction )    [function]
 // ----------------------------------------------------
    rule #memory(_ = load INDEX1 , INDEX2 , WIDTH,  _::Map MEMINDEX |-> MU, MEMINDEX) => #memoryUsageUpdate(MU, INDEX2, WIDTH)
    rule #memory(store _ , INDEX1 , INDEX2 , WIDTH, _::Map MEMINDEX |-> MU, MEMINDEX) => #memoryUsageUpdate(MU, INDEX2, WIDTH)
    rule #memory(store VALUE , INDEX,               _::Map MEMINDEX |-> MU, MEMINDEX) => #memoryUsageUpdate(MU, 0, #sizeWordStack(#asSignedBytes(VALUE)))

    rule #memIndex(_ = load INDEX)          => INDEX
    rule #memIndex(_ = load INDEX , _ , _)  => INDEX
    rule #memIndex(store _ , INDEX)         => INDEX
    rule #memIndex(store _ , INDEX , _ , _) => INDEX
    rule #memIndex(_ = sha3 INDEX)          => INDEX
    rule #memIndex(log INDEX)               => INDEX
    rule #memIndex(log INDEX , _:Ints)      => INDEX

    rule #usesMemory(_:LoadInst)  => true
    rule #usesMemory(_:StoreInst) => true
    rule #usesMemory(_:SHA3Inst)  => true
    rule #usesMemory(_:LogInst)   => true
    rule #usesMemory(...)         => false [owise]

    rule #memory(OP, MU,                     MEMINDEX) => #memory(OP, MU MEMINDEX |-> 0, MEMINDEX) requires notBool MEMINDEX in_keys(MU)
    rule #memory(_,  _::Map MEMINDEX |-> MU, MEMINDEX) => MU [owise]

    syntax Int ::= #memoryUsageUpdate ( Int , Int , Int ) [function]
 // ----------------------------------------------------------------
    rule #memoryUsageUpdate(MU, START, 0)     => MU
    rule #memoryUsageUpdate(MU, START, WIDTH) => maxInt(MU, (chop(START) +Int chop(WIDTH)) up/Int 32) requires WIDTH =/=Int 0
```

Execution Gas
-------------

Each opcode has an intrinsic gas cost of execution as well (appendix H of the yellowpaper).

-   `#gasExec` loads all the relevant surronding state and uses that to compute the intrinsic execution gas of each opcode.

```{.k .uiuck .rvk}
    syntax InternalOp ::= #gasExec ( Schedule , Instruction )
 // ----------------------------------------------------
    rule <k> #gasExec(SCHED, sstore VALUE , INDEX) => Csstore(SCHED, VALUE, #lookup(STORAGE, INDEX)) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account>

    rule <k> #gasExec(SCHED, _ = exp W0 , 0)  => Gexp < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = exp W0 , W1) => Gexp < SCHED > +Int (Gexpbyte < SCHED > *Int (1 +Int (log256Int(W1)))) ... </k> requires W1 =/=K 0


    rule <k> #gasExec(SCHED, log IDX)                                 => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (0 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, log IDX , _:Int)                         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (1 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, log IDX , _:Int , _:Int)                 => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (2 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, log IDX , _:Int , _:Int , _:Int)         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (3 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, log IDX , _:Int , _:Int , _:Int,  _:Int) => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (4 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>

    rule <k> #gasExec(SCHED, _ = call _ at ACCTTO ( _ ) send VALUE , gaslimit GCAP) => Ccall(SCHED, ACCTTO,   ACCTS, GCAP, GAVAIL, VALUE) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, _ = staticcall _ at ACCTTO ( _ ) gaslimit GCAP) => Ccall(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, 0) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, selfdestruct ACCTTO) => Cselfdestruct(SCHED, ACCTTO, ACCTS, BAL) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <id> ACCTFROM </id>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL </balance>
           ...
         </account>

    rule <k> #gasExec(SCHED, _:CreateInst) => Gcreate < SCHED > ... </k>

    rule <k> #gasExec(SCHED, _ = sha3 IDX) => Gsha3   < SCHED > +Int (Gsha3word < SCHED > *Int (#sizeWordStack({LM [ IDX ]}:>WordStack) up/Int 32)) ... </k> <localMem> LM </localMem>

    rule <k> #gasExec(SCHED, _:SLoadInst)  => Gsload  < SCHED > ... </k>

    // Wzero
    rule <k> #gasExec(SCHED, _:RevertInst)  => Gzero < SCHED > ... </k>

    // Wbase
    rule <k> #gasExec(SCHED, _ = call @iele.address ( .Ints ))     => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.origin ( .Ints ))      => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.caller ( .Ints ))      => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.callvalue ( .Ints ))   => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.codesize ( .Ints ))    => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.gasprice ( .Ints ))    => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.beneficiary ( .Ints )) => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.timestamp ( .Ints ))   => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.number ( .Ints ))      => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.difficulty ( .Ints ))  => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.gaslimit ( .Ints ))    => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.msize ( .Ints ))       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.gas ( .Ints ))         => Gbase < SCHED > ... </k>

    // Wverylow
    rule <k> #gasExec(SCHED, _:AddInst)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:SubInst)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:NotInst)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:CmpInst)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:IsZeroInst) => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:AndInst)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:OrInst)     => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:XorInst)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:ByteInst)   => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:LoadInst)   => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:StoreInst)  => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:AssignInst) => Gverylow < SCHED > ... </k>

    // Wlow
    rule <k> #gasExec(SCHED, _:MulInst)  => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:DivInst)  => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:ModInst)  => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:SExtInst) => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:TwosInst) => Glow < SCHED > ... </k>

    // Wmid
    rule <k> #gasExec(SCHED, _:AddModInst)    => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:MulModInst)    => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:ExpModInst)    => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:JumpInst)      => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _:ReturnInst)    => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @ NAME ( _ )) => Gmid < SCHED > ... </k> requires notBool isIeleBuiltin(NAME)

    // Whigh
    rule <k> #gasExec(SCHED, _:CondJumpInst) => Ghigh < SCHED > ... </k>

    rule <k> #gasExec(SCHED, _ = call @iele.extcodesize ( _ )) => Gextcodesize < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.balance ( _ ))     => Gbalance     < SCHED > ... </k>
    rule <k> #gasExec(SCHED, _ = call @iele.blockhash ( _ ))   => Gblockhash   < SCHED > ... </k>

    // Precompiled
    rule <k> #gasExec(_, ECREC)  => 3000 ... </k>
    rule <k> #gasExec(_, SHA256) =>  60 +Int  12 *Int (LEN up/Int 32) ... </k> <callData> LEN , DATA , .Ints </callData>
    rule <k> #gasExec(_, RIP160) => 600 +Int 120 *Int (LEN up/Int 32) ... </k> <callData> LEN , DATA , .Ints </callData>
    rule <k> #gasExec(_, ID)     =>  15 +Int   3 *Int #sizeRegs(DATA) ... </k> <callData> DATA </callData>

    rule #gasExec(_, ECADD) => 500
    rule #gasExec(_, ECMUL) => 40000
    rule <k> #gasExec(_, ECPAIRING) => 100000 +Int (#sizeRegs(DATA) /Int 6) *Int 80000 ... </k> <callData> DATA </callData>
```

There are several helpers for calculating gas (most of them also specified in the yellowpaper).

Note: These are all functions as the operator `#gasExec` has already loaded all the relevant state.

```{.k .uiuck .rvk}
    syntax Int ::= Csstore ( Schedule , Int , Int ) [function]
 // ----------------------------------------------------------
    rule Csstore(SCHED, VALUE, OLD) => #if VALUE =/=Int 0 andBool OLD ==Int 0 #then Gsstoreset < SCHED > #else Gsstorereset < SCHED > #fi

    syntax Int ::= Ccall    ( Schedule , Int , Map , Int , Int , Int ) [function]
                 | Ccallgas ( Schedule , Int , Map , Int , Int , Int ) [function]
                 | Cgascap  ( Schedule , Int , Int , Int )             [function]
                 | Cextra   ( Schedule , Int , Map , Int )             [function]
                 | Cxfer    ( Schedule , Int )                         [function]
                 | Cnew     ( Schedule , Int , Map , Int )             [function]
 // -----------------------------------------------------------------------------
    rule Ccall(SCHED, ACCT, ACCTS, GCAP, GAVAIL, VALUE) => Cextra(SCHED, ACCT, ACCTS, VALUE) +Int Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ACCT, ACCTS, VALUE))

    rule Ccallgas(SCHED, ACCT, ACCTS, GCAP, GAVAIL, 0)     => Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ACCT, ACCTS,     0))
    rule Ccallgas(SCHED, ACCT, ACCTS, GCAP, GAVAIL, VALUE) => Cgascap(SCHED, GCAP, GAVAIL, Cextra(SCHED, ACCT, ACCTS, VALUE)) +Int Gcallstipend < SCHED > requires VALUE =/=K 0

    rule Cgascap(SCHED, GCAP, GAVAIL, GEXTRA) => minInt(#allBut64th(GAVAIL -Int GEXTRA), GCAP) requires GAVAIL >=Int GEXTRA andBool notBool Gstaticcalldepth << SCHED >>
    rule Cgascap(SCHED, GCAP, GAVAIL, GEXTRA) => GCAP                                          requires GAVAIL <Int  GEXTRA orBool Gstaticcalldepth << SCHED >>

    rule Cextra(SCHED, ACCT, ACCTS, VALUE) => Gcall < SCHED > +Int Cnew(SCHED, ACCT, ACCTS, VALUE) +Int Cxfer(SCHED, VALUE)

    rule Cxfer(SCHED, 0) => 0
    rule Cxfer(SCHED, N) => Gcallvalue < SCHED > requires N =/=K 0

    rule Cnew(SCHED, ACCT, ACCTS, VALUE) => Gnewaccount < SCHED >
      requires         #accountNonexistent(SCHED, ACCT, ACCTS) andBool VALUE =/=Int 0
    rule Cnew(SCHED, ACCT, ACCTS, VALUE) => 0
      requires notBool #accountNonexistent(SCHED, ACCT, ACCTS) orBool  VALUE  ==Int 0

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

The IELE semantics is designed to be extensible in future hard forks while still maintainin an accurate semantics of the language prior
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

    syntax ScheduleConst ::= "Gzero"        | "Gbase"          | "Gverylow"      | "Glow"          | "Gmid"        | "Ghigh"
                           | "Gextcodesize" | "Gextcodecopy"   | "Gbalance"      | "Gsload"        | "Gjumpdest"   | "Gsstoreset"
                           | "Gsstorereset" | "Rsstoreclear"   | "Rselfdestruct" | "Gselfdestruct" | "Gcreate"     | "Gcodedeposit"  | "Gcall"
                           | "Gcallvalue"   | "Gcallstipend"   | "Gnewaccount"   | "Gexp"          | "Gexpbyte"    | "Gmemory"       | "Gtxcreate"
                           | "Gtxdatazero"  | "Gtxdatanonzero" | "Gtransaction"  | "Glog"          | "Glogdata"    | "Glogtopic"     | "Gsha3"
                           | "Gsha3word"    | "Gcopy"          | "Gblockhash"    | "Gquadcoeff"    | "maxCodeSize" | "Rb"            | "Gquaddivisor"
 // -------------------------------------------------------------------------------------------------------------------------------------------------
```

### Default Schedule

```{.k .uiuck .rvk}
    syntax Schedule ::= "DEFAULT"
 // -----------------------------
    rule Gzero    < DEFAULT > => 0
    rule Gbase    < DEFAULT > => 2
    rule Gverylow < DEFAULT > => 3
    rule Glow     < DEFAULT > => 5
    rule Gmid     < DEFAULT > => 8
    rule Ghigh    < DEFAULT > => 10

    rule Gexp      < DEFAULT > => 10
    rule Gexpbyte  < DEFAULT > => 50
    rule Gsha3     < DEFAULT > => 30
    rule Gsha3word < DEFAULT > => 6

    rule Gsload       < DEFAULT > => 200
    rule Gsstoreset   < DEFAULT > => 20000
    rule Gsstorereset < DEFAULT > => 5000
    rule Rsstoreclear < DEFAULT > => 15000

    rule Glog      < DEFAULT > => 375
    rule Glogdata  < DEFAULT > => 8
    rule Glogtopic < DEFAULT > => 375

    rule Gcall        < DEFAULT > => 40
    rule Gcallstipend < DEFAULT > => 2300
    rule Gcallvalue   < DEFAULT > => 9000
    rule Gnewaccount  < DEFAULT > => 25000

    rule Gcreate       < DEFAULT > => 32000
    rule Gcodedeposit  < DEFAULT > => 200
    rule Gselfdestruct < DEFAULT > => 0
    rule Rselfdestruct < DEFAULT > => 24000

    rule Gmemory      < DEFAULT > => 3
    rule Gquadcoeff   < DEFAULT > => 512
    rule Gcopy        < DEFAULT > => 3
    rule Gquaddivisor < DEFAULT > => 20

    rule Gtransaction   < DEFAULT > => 21000
    rule Gtxcreate      < DEFAULT > => 53000
    rule Gtxdatazero    < DEFAULT > => 4
    rule Gtxdatanonzero < DEFAULT > => 68

    rule Gjumpdest    < DEFAULT > => 1
    rule Gbalance     < DEFAULT > => 400
    rule Gblockhash   < DEFAULT > => 20
    rule Gextcodesize < DEFAULT > => 700
    rule Gextcodecopy < DEFAULT > => 700

    rule maxCodeSize < DEFAULT > => 2 ^Int 16
    rule Rb          < DEFAULT > => 3 *Int (10 ^Int 18)

    rule Gselfdestructnewaccount << DEFAULT >> => false
    rule Gstaticcalldepth        << DEFAULT >> => true
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


```

IELE Program Representations
============================

The following code processes a `Contract` and loads it into the `<program>` cell.

```{.k .uiuck .rvk}
    syntax ProgramCell ::= #loadCode ( Contract ) [function]
                         | #loadCode ( Contract , Contract ) [klabel(#loadCodeAux), function]
 // -----------------------------------------------------------------------------------------
    rule #loadCode(contract _ ! _ { _ } CONTRACTS, CONTRACT) => #loadCode(CONTRACTS, CONTRACT)
      requires CONTRACTS =/=K .Contract
    rule #loadCode(contract _ ! SIZE { DEFS }, CONTRACT)
      => #loadDeclarations(DEFS,
         <program>
           <functions> .Bag </functions>
           <funcIds> .Set </funcIds>
           <programSize> SIZE </programSize>
           <exported> .Set </exported>
           <contractCode> CONTRACT </contractCode>
         </program>)
      [owise]
     rule #loadCode(CONTRACT) => #loadCode(CONTRACT, CONTRACT)

    syntax ProgramCell ::= #loadDeclarations ( TopLevelDefinitions , ProgramCell ) [function]
                         | #loadFunction  ( TopLevelDefinitions , Blocks , ProgramCell , IeleName , FunctionCell ) [function]
 // -------------------------------------------------------------------------------------------------------------------------
    rule #loadDeclarations(define @ NAME ( NARGS:Int ) { BLOCKS } FUNCS, <program> PROG </program>)
      => #loadFunction(FUNCS, BLOCKS, <program> PROG </program>, NAME, <function> <funcId> NAME </funcId> <nparams> NARGS </nparams> ... </function>)
    rule #loadDeclarations(define public @ NAME ( NARGS:Int ) { BLOCKS } FUNCS, <program> <exported> EXPORTS </exported> REST </program>)
      => #loadFunction(FUNCS, BLOCKS, <program> <exported> SetItem(NAME) EXPORTS </exported> REST </program>, NAME, <function> <funcId> NAME </funcId> <nparams> NARGS </nparams> ... </function>)
    rule #loadDeclarations(external contract _ FUNCS, <program> PROG </program>)
      => #loadDeclarations(FUNCS, <program> PROG </program>)
    rule #loadDeclarations(.TopLevelDefinitions, <program> PROG </program>) => <program> PROG </program>

    rule #loadFunction(FUNCS, BLOCKS, <program> PROG <functions> REST </functions> <funcIds> NAMES </funcIds> </program>, NAME, <function> FUNC <instructions> _ </instructions> <jumpTable> _ </jumpTable> </function>)
      => #loadDeclarations(FUNCS, <program> PROG <funcIds> NAMES SetItem(NAME) </funcIds> <functions> REST <function> FUNC <instructions> BLOCKS </instructions> <jumpTable> #computeJumpTable(BLOCKS) </jumpTable> </function> </functions> </program>)
   
    syntax IeleName ::= #mainContract ( Contract ) [function]
    syntax Int ::= #contractSize ( Contract , IeleName ) [function]
 // ---------------------------------------------------------------
    rule #mainContract(contract NAME ! _ { _ }) => NAME
    rule #mainContract(contract _ ! _ { _ } REST) => #mainContract(REST) [owise]

    rule #contractSize(contract NAME ! SIZE { _ } _, NAME) => SIZE
    rule #contractSize(contract _ ! _ { _ } REST, NAME) => #contractSize(REST, NAME) [owise]

    syntax Map ::= #computeJumpTable ( Blocks )             [function]
                 | #computeJumpTable ( Blocks , Map , Set ) [function, klabel(#computeJumpTableAux)]
 // ------------------------------------------------------------------------------------------------
    rule #computeJumpTable(BLOCKS) => #computeJumpTable(BLOCKS, .Map, .Set)

    rule #computeJumpTable(.LabeledBlocks, JUMPS, _) => JUMPS

    rule #computeJumpTable(LABEL : INSTRS BLOCKS, JUMPS, LABELS) => #computeJumpTable(BLOCKS, JUMPS [ LABEL <- INSTRS BLOCKS ], LABELS SetItem(LABEL)) requires notBool LABEL in LABELS
    rule #computeJumpTable(_::LabeledBlock BLOCKS, JUMPS, LABELS) => #computeJumpTable(BLOCKS, JUMPS, LABELS) [owise]
    rule #computeJumpTable(_::UnlabeledBlock BLOCKS, JUMPS, LABELS) => #computeJumpTable(BLOCKS, JUMPS, LABELS) [owise]
endmodule
```

