IELE Execution
==============

IELE is a register-based abstract machine over some simple opcodes.
Most of the opcodes are "local" to the execution state of the machine, but some of them must interact with the world state.
This file only defines the local execution operations. A separate `cardano.md` will eventually be created to describe the remainder
of the transaction protocol.

```{.k .uiuck .rvk}
requires "data.k"

module IELE
    imports STRING
    imports IELE-DATA
```

Configuration
-------------

The configuration has cells for the current account id, the current position in the program, the current gas, the gas price, the current program, the word stack, and the local memory.
In addition, there are cells for the callstack and execution substate.

We've broken up the configuration into two components; those parts of the state that mutate during execution of a single transaction and those that are static throughout.
In the comments next to each cell, we've marked which component of the yellowpaper state corresponds to each cell.

```{.k .uiuck .rvk}
    configuration <k> $PGM:EthereumSimulation </k>
                  <exit-code exit=""> 1 </exit-code>
                  <mode> $MODE:Mode </mode>
                  <schedule> $SCHEDULE:Schedule </schedule>
                  <analysis> .Map </analysis>

                  // IELE Specific
                  // ============

                  <iele>

                    // Mutable during a single transaction
                    // -----------------------------------

                    <output>        .Regs:Ints </output>                   // H_RETURN
                    <callStack>     .List      </callStack>
                    <interimStates> .List      </interimStates>
                    <substateStack> .List      </substateStack>

                    // A single contract call frame
                    // ----------------------------
                    <callFrame>
                      // The loaded state of a IELE program
                      // ----------------------------------
                      <program>
                        <functions>
                          <function multiplicity="*" type="Map">
                            <funcId>       0     </funcId>
                            <nparams>      0     </nparams>
                            <instructions> .Ops  </instructions>
                            <jumpTable>    .Map  </jumpTable>
                          </function>
                        </functions>
                        <funcIds>     .Set </funcIds>
                        <constants>   .Map </constants>
                        <exported>    .Map </exported>
                        <nregs>       5    </nregs>
                        <programSize> 0    </programSize>
                      </program>
                      <callDepth>    0          </callDepth>
                      <localCalls>   .List      </localCalls>

                      // I_*
                      <id>        0          </id>                         // I_a
                      <caller>    0          </caller>                     // I_s
                      <callData>  .Regs:Ints </callData>                   // I_d
                      <callValue> 0          </callValue>                  // I_v

                      // \mu_*
                      <regs>        .Array  </regs>                        // \mu_s
                      <globalRegs>  .Array  </globalRegs>                  // \mu_s
                      <localMem>    .Memory </localMem>                    // \mu_m
                      <memoryUsed>  .Map    </memoryUsed>                  // \mu_i
                      <fid>         0       </fid>
                      <gas>         0       </gas>                         // \mu_g
                      <previousGas> 0       </previousGas>

                      <static> false </static>
                    </callFrame>

                    // A_* (execution substate)
                    <substate>
                      <selfDestruct> .Set  </selfDestruct>                 // A_s
                      <log>          .List </log>                          // A_l
                      <refund>       0     </refund>                       // A_r
                    </substate>

                    // Immutable during a single transaction
                    // -------------------------------------

                    <gasPrice> 0 </gasPrice>                               // I_p
                    <origin>   0 </origin>                                 // I_o

                    // I_H* (block information)
                    <previousHash>     0          </previousHash>          // I_Hp
                    <ommersHash>       0          </ommersHash>            // I_Ho
                    <coinbase>         0          </coinbase>              // I_Hc
                    <stateRoot>        0          </stateRoot>             // I_Hr
                    <transactionsRoot> 0          </transactionsRoot>      // I_Ht
                    <receiptsRoot>     0          </receiptsRoot>          // I_He
                    <logsBloom>        .WordStack </logsBloom>             // I_Hb
                    <difficulty>       0          </difficulty>            // I_Hd
                    <number>           0          </number>                // I_Hi
                    <gasLimit>         0          </gasLimit>              // I_Hl
                    <gasUsed>          0          </gasUsed>               // I_Hg
                    <timestamp>        0          </timestamp>             // I_Hs
                    <extraData>        .WordStack </extraData>             // I_Hx
                    <mixHash>          0          </mixHash>               // I_Hm
                    <blockNonce>       0          </blockNonce>            // I_Hn

                    <ommerBlockHeaders> [ .JSONList ] </ommerBlockHeaders>
                    <blockhash>         .List         </blockhash>

                  </iele>

                  // Ethereum Network
                  // ================

                  <network>

                    // Accounts Record
                    // ---------------

                    <activeAccounts> .Map </activeAccounts>
                    <accounts>
                      <account multiplicity="*" type="Map">
                        <acctID>   0          </acctID>
                        <balance>  0          </balance>
                        <code>     #emptyCode </code>
                        <codeSize> 0          </codeSize>
                        <storage>  .Map       </storage>
                        <nonce>    0          </nonce>
                      </account>
                    </accounts>

                    // Transactions Record
                    // -------------------

                    <txOrder>   .List </txOrder>
                    <txPending> .List </txPending>

                    <messages>
                      <message multiplicity="*" type="Map">
                        <msgID>      0          </msgID>
                        <txNonce>    0          </txNonce>            // T_n
                        <txGasPrice> 0          </txGasPrice>         // T_p
                        <txGasLimit> 0          </txGasLimit>         // T_g
                        <to>         .Account   </to>                 // T_t
                        <value>      0          </value>              // T_v
                        <v>          0          </v>                  // T_w
                        <r>          .WordStack </r>                  // T_r
                        <s>          .WordStack </s>                  // T_s
                        <data>       .WordStack </data>               // T_i/T_e
                      </message>
                    </messages>

                  </network>

    syntax EthereumSimulation
 // -------------------------
```

Modal Semantics
---------------

Our semantics is modal, with the initial mode being set on the command line via `-cMODE=EXECMODE`.

-   `NORMAL` executes as a client on the network would.
-   `VMTESTS` skips `CALL*` and `CREATE` operations.

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
-   `#revert` is used to indicate the contract terminated with the REVERT instruction (exceptional return with error message, refunding gas).

```{.k .uiuck .rvk}
    syntax KItem ::= Exception
    syntax Exception ::= "#exception" | "#end" | "#revert"
 // ------------------------------------------
    rule <k> EX:Exception ~> (_:Int    => .) ... </k>
    rule <k> EX:Exception ~> (_:Op => .) ... </k>
    rule <k> EX:Exception ~> (_:Ops => .) ... </k>

```

Description of registers.

-   Local registers begin with `%`
-   Global registers begin with `@`
-   Registers are evaluated using heating to the values they contain.
-   `#regRange(N)` generates the registers 0 to N-1.
-   `#sizeRegs(R)` returns the number of regsiters in a list of registers.

```{.k .uiuck .rvk}

    syntax Reg ::= "%" Int | "@" Int | Int
    syntax KResult ::= Int
 // ----------------------------
    rule <k> % REG => REGS [ REG ] ... </k> <regs> REGS </regs>
    rule <k> @ REG => REGS [ REG ] ... </k> <globalRegs> REGS </globalRegs>

    syntax Regs ::= NilRegs
    syntax Ints ::= NilRegs
    syntax NilRegs ::= ".Regs"
    syntax Regs ::= Reg Regs [strict]
    syntax Ints ::= Int Ints
    syntax Regs ::= Ints
    syntax KResult ::= NilRegs | Ints

    syntax Regs ::= #regRange ( Int ) [function]
                  | #regRange ( Int , Int ) [function, klabel(#regRangeAux)]
 // ------------------------------------------------------------------------
    rule #regRange(N) => #regRange(0, N)
    rule #regRange(_, 0) => .Regs
    rule #regRange(N, M) => % N #regRange(N +Int 1, M -Int 1) [owise]

    syntax Int ::= #sizeRegs ( Regs ) [function]
                 | #sizeRegs ( Regs , Int ) [function, klabel(#sizeRegsAux)]
 // ------------------------------------------------------------------------
    rule #sizeRegs(REGS) => #sizeRegs(REGS, 0)
    rule #sizeRegs(REG REGS, N) => #sizeRegs(REGS, N +Int 1)
    rule #sizeRegs(.Regs, N) => N
```

OpCode Execution Cycle
----------------------

`OpCode` is broken into several subsorts for easier handling.
Here all `OpCode`s are subsorted into `KItem` (allowing sequentialization), and the various sorts of opcodes are subsorted into `OpCode`.

```{.k .uiuck .rvk}
    syntax KItem  ::= OpCode
    syntax Op ::= InternalOp | HeaderOp
    syntax OpCode ::= NullOp | NullVoidOp | UnOp | UnVoidOp | BinOp | BinVoidOp | TernOp
                    | TernVoidOp | QuadVoidOp | FiveVoidOp | CallOp | CallSixOp
                    | LocalCallOp | ReturnOp | CreateOp | CopyCreateOp | HeaderOp
 // ---------------------------------------------------------------------------------------
```

-   `#execute` loads the code of the currently-executing function into the `k` cell, where it executes until the function returns.
    It is an exception if we try to execute a function that does not exist.

```{.k .uiuck .rvk}
    syntax KItem ::= "#execute"
 // ---------------------------
    rule <k> #execute => CODE       ... </k> <fid> FUNC </fid> <funcId> FUNC </funcId> <instructions> CODE </instructions>
    rule <k> #execute => #exception ... </k> <fid> FUNC </fid> <funcIds> FUNCS </funcIds> requires notBool FUNC in FUNCS
```

Execution follows a simple cycle where first the state is checked for exceptions, then if no exceptions will be thrown the opcode is run.

-   Regardless of the mode, if we reach the end of the function, we execute an implicit `STOP` instruction.

```{.k .uiuck .rvk}
    rule <k> .Ops => STOP ; .Ops ... </k>
```

-   In `NORMAL` or `VMTESTS` mode, `#next` checks if the opcode is exceptional, runs it if not, then executes the next instruction (assuming
    the instruction doesn't execute a transfer of control flow).

```{.k .uiuck .rvk}
    rule <mode> EXECMODE </mode>
         <k> OP ; OPS
          => #exceptional? [ OP ] ~> #exec [ OP ]
          ~> OPS
         ...
         </k>
      requires EXECMODE in #normalModes

    syntax Set ::= "#normalModes" [function]
 // ----------------------------------------
    rule #normalModes => (SetItem(NORMAL) SetItem(VMTESTS))
```

### Exceptional Ops

Some checks if an opcode will throw an exception are relatively quick and done up front.

-   `#exceptional?` checks if the operator is invalid.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#exceptional?" "[" Op "]"
 // ----------------------------------------------------
    rule <k> #exceptional? [ OP ] => #invalid? [ #code(OP) ] ~> #badJumpDest? [ OP ] ~> #static? [ OP ] ... </k>
```

-   `#invalid?` checks if it's the designated invalid opcode.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#invalid?" "[" OpCode "]"
 // ------------------------------------------------
    rule <k> #invalid? [ INVALID ] => #exception ... </k>
    rule <k> #invalid? [ OP      ] => .          ... </k> requires notBool isInvalidOp(OP)
```

-   `#badJumpDest?` determines if the opcode will result in a bad jump destination.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#badJumpDest?" "[" Op "]"
 // ----------------------------------------------------
    rule <k> #badJumpDest? [ OP                        ] => . ... </k> requires notBool isJumpOp(#code(OP))
    rule <k> #badJumpDest? [ JUMP (LABEL)              ] => . ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires LABEL in_keys(JUMPS)
    rule <k> #badJumpDest? [ JUMPI(LABEL) _            ] => . ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires LABEL in_keys(JUMPS)

    rule <k> #badJumpDest? [ JUMP (LABEL)              ] => #exception ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires notBool LABEL in_keys(JUMPS)
    rule <k> #badJumpDest? [ JUMPI(LABEL) _            ] => #exception ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> JUMPS </jumpTable> </function> requires notBool LABEL in_keys(JUMPS)

    syntax Bool ::= isJumpOp ( OpCode ) [function]
 // ----------------------------------------------
    rule isJumpOp(JUMP(_)) => true
    rule isJumpOp(JUMPI(_)) => true
    rule isJumpOp(...) => false [owise]
```

-   `#static?` determines if the opcode should throw an exception due to the static flag.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#static?" "[" Op "]"
 // -----------------------------------------------
    rule <k> #static? [ OP ] => .          ... </k>                               <static> false </static>
    rule <k> #static? [ OP ] => .          ... </k> <regs> REGS </regs> <static> true  </static> requires notBool #changesState(#code(OP), OP, REGS)
    rule <k> #static? [ OP ] => #exception ... </k> <regs> REGS </regs> <static> true  </static> requires         #changesState(#code(OP), OP, REGS)

    syntax Bool ::= #changesState ( OpCode, Op , Array ) [function]
 // ---------------------------------------------------------------
    rule #changesState(LOG0, _, _) => true
    rule #changesState(LOG1, _, _) => true
    rule #changesState(LOG2, _, _) => true
    rule #changesState(LOG3, _, _) => true
    rule #changesState(LOG4, _, _) => true
    rule #changesState(SSTORE, _, _) => true
    rule #changesState(_, CALL(_,_,_) _ _ _ VALUE _ _, _) => true requires VALUE =/=Int 0
    rule #changesState(CREATE(_,_), _, _) => true
    rule #changesState(COPYCREATE(_), _, _) => true
    rule #changesState(SELFDESTRUCT, _, _) => true
    rule #changesState(...) => false [owise]
```

### Execution Step

-   `#exec` will load the arguments of the opcode and trigger the subsequent operations.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#exec" "[" Op "]"
 // ----------------------------------------
    rule <k> #exec [ OP:LoadedOp ] => #gas [ #addr?(OP) ] ~> #addr?(OP) ... </k>
```

Here we load the correct number of arguments from the `regs` based on the sort of the opcode.
Some of them require an argument to be interpereted as an address (modulo 160 bits), so the `#addr?` function performs that check.

```{.k .uiuck .rvk}
    syntax LoadedOp ::= NullOp Reg                      [klabel(nullOp)]
                      | NullVoidOp

    syntax Op ::= UnOp Reg Reg                           [klabel(unOp)]
                | UnVoidOp Reg                           [klabel(unVoidOp)]
                | BinOp Reg Reg Reg                      [klabel(binOp)]
                | BinVoidOp Reg Reg                      [klabel(binVoidOp)]
                | TernOp Reg Reg Reg Reg                 [klabel(ternOp)]
                | TernVoidOp Reg Reg Reg                 [klabel(ternVoidOp)]
                | QuadVoidOp Reg Reg Reg Reg             [klabel(quadVoidOp)]
                | FiveVoidOp Reg Reg Reg Reg Reg         [klabel(fiveVoidOp)]
                | CallSixOp Reg Reg Reg Regs Regs        [klabel(callSixOp)]
                | CallOp Reg Reg Reg Reg Regs Regs       [klabel(callOp)]
                | CreateOp Reg Reg Regs                  [klabel(createOp)]
                | CopyCreateOp Reg Reg Reg Regs          [klabel(copyCreateOp)]
                | LocalCallOp Regs Regs                  [klabel(localCallOp)]
                | ReturnOp Regs                          [klabel(returnOp)]

    context #exec [ _::UnOp _ HOLE:Reg ]

    context #exec [ _::UnVoidOp HOLE:Reg ]

    context #exec [ _::BinOp _ HOLE:Reg _ ]
    context #exec [ _::BinOp _ _ HOLE:Reg ]

    context #exec [ _::BinVoidOp HOLE:Reg _ ]
    context #exec [ _::BinVoidOp _ HOLE:Reg ]

    context #exec [ _::TernOp _ HOLE:Reg _ _ ]
    context #exec [ _::TernOp _ _ HOLE:Reg _ ]
    context #exec [ _::TernOp _ _ _ HOLE:Reg ]

    context #exec [ _::TernVoidOp HOLE:Reg _ _ ]
    context #exec [ _::TernVoidOp _ HOLE:Reg _ ]
    context #exec [ _::TernVoidOp _ _ HOLE:Reg ]

    context #exec [ _::QuadVoidOp HOLE:Reg _ _ _ ]
    context #exec [ _::QuadVoidOp _ HOLE:Reg _ _ ]
    context #exec [ _::QuadVoidOp _ _ HOLE:Reg _ ]
    context #exec [ _::QuadVoidOp _ _ _ HOLE:Reg ]

    context #exec [ _::FiveVoidOp HOLE:Reg _ _ _ _ ]
    context #exec [ _::FiveVoidOp _ HOLE:Reg _ _ _ ]
    context #exec [ _::FiveVoidOp _ _ HOLE:Reg _ _ ]
    context #exec [ _::FiveVoidOp _ _ _ HOLE:Reg _ ]
    context #exec [ _::FiveVoidOp _ _ _ _ HOLE:Reg ]

    context #exec [ _::CallSixOp _ HOLE:Reg _ _ _  ]
    context #exec [ _::CallSixOp _ _ HOLE:Reg _ _  ]
    context #exec [ _::CallSixOp _ _ _ _ HOLE:Regs ]

    context #exec [ _::CallOp _ HOLE:Reg _ _ _ _  ]
    context #exec [ _::CallOp _ _ HOLE:Reg _ _ _  ]
    context #exec [ _::CallOp _ _ _ HOLE:Reg _ _  ]
    context #exec [ _::CallOp _ _ _ _ _ HOLE:Regs ]

    context #exec [ _::LocalCallOp _ HOLE:Regs ]
    context #exec [ _::ReturnOp HOLE:Regs ]

    context #exec [ _::CreateOp _ HOLE:Reg _  ]
    context #exec [ _::CreateOp _ _ HOLE:Regs ]

    context #exec [ _::CopyCreateOp _ HOLE:Reg _ _  ]
    context #exec [ _::CopyCreateOp _ _ HOLE:Reg _  ]
    context #exec [ _::CopyCreateOp _ _ _ HOLE:Regs ]

    syntax Op ::= LoadedOp

    syntax LoadedOp ::= UnOp Reg Int                           [klabel(unOp)]
                      | UnVoidOp Int                           [klabel(unVoidOp)]
                      | BinOp Reg Int Int                      [klabel(binOp)]
                      | BinVoidOp Int Int                      [klabel(binVoidOp)]
                      | TernOp Reg Int Int Int                 [klabel(ternOp)]
                      | TernVoidOp Int Int Int                 [klabel(ternVoidOp)]
                      | QuadVoidOp Int Int Int Int             [klabel(quadVoidOp)]
                      | FiveVoidOp Int Int Int Int Int         [klabel(fiveVoidOp)]
                      | CallSixOp Reg Int Int Regs Ints        [klabel(callSixOp)]
                      | CallOp Reg Int Int Int Regs Ints       [klabel(callOp)]
                      | CreateOp Reg Int Ints                  [klabel(createOp)]
                      | CopyCreateOp Reg Int Int Ints          [klabel(copyCreateOp)]
                      | LocalCallOp Regs Ints                  [klabel(localCallOp)]
                      | ReturnOp Ints                          [klabel(returnOp)]

    syntax Op ::= "#addr?" "(" Op ")" [function]
 // -------------------------------------------------
    rule #addr?(BALANCE REG W)                       => BALANCE REG #addr(W)
    rule #addr?(EXTCODESIZE REG W)                   => EXTCODESIZE REG #addr(W)
    rule #addr?(O:CopyCreateOp REG W0 W1 REGS1)      => O REG #addr(W0) W1 REGS1
    rule #addr?(SELFDESTRUCT W)                      => SELFDESTRUCT #addr(W)
    rule #addr?(CSO:CallSixOp REG W0 W1 REGS1 REGS2) => CSO REG W0 #addr(W1) REGS1 REGS2
    rule #addr?(CO:CallOp REG W0 W1 W2 REGS1 REGS2)  => CO  REG W0 #addr(W1) W2 REGS1 REGS2
    rule #addr?(OP)                                  => OP [owise]

    syntax OpCode ::= #code ( Op ) [function]
    rule #code(OP::NullOp _)              => OP
    rule #code(OP:NullVoidOp)             => OP
    rule #code(OP::UnOp _ _)              => OP
    rule #code(OP::UnVoidOp _)            => OP
    rule #code(OP::BinOp _ _ _)           => OP
    rule #code(OP::BinVoidOp _ _)         => OP
    rule #code(OP::TernOp _ _ _ _)        => OP
    rule #code(OP::TernVoidOp _ _ _)      => OP
    rule #code(OP::QuadVoidOp _ _ _ _)    => OP
    rule #code(OP::FiveVoidOp _ _ _ _ _)  => OP
    rule #code(OP::CallSixOp _ _ _ _ _)   => OP
    rule #code(OP::CallOp _ _ _ _ _ _)    => OP
    rule #code(OP::LocalCallOp _ _)       => OP
    rule #code(OP::ReturnOp _)            => OP
    rule #code(OP::CreateOp _ _ _)        => OP
    rule #code(OP::CopyCreateOp _ _ _ _)  => OP
```

-   `#gas` calculates how much gas this operation costs, and takes into account the memory consumed.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#gas" "[" Op "]" | "#deductGas" | #deductMemory ( Int )
 // ------------------------------------------------------------------------------
    rule <k> #gas [ OP ] => #memory(OP, MU, #memIndex(OP)) ~> #deductMemory(#memIndex(OP)) ~> #gasExec(SCHED, OP) ~> #deductGas ... </k> <memoryUsed> MU </memoryUsed> <schedule> SCHED </schedule>
      requires #usesMemory(#code(OP))
    rule <k> #gas [ OP ] => #gasExec(SCHED, OP) ~> #deductGas ... </k> <gas> GAVAIL </gas> <previousGas> _ => GAVAIL </previousGas> <schedule> SCHED </schedule>
      requires notBool #usesMemory(#code(OP))

    rule <k> MU':Int ~> #deductMemory(_)        => #exception ... </k> requires MU' >=Int pow256
    rule <k> MU':Int ~> #deductMemory(MEMINDEX) => (Cmem(SCHED, MU [ MEMINDEX <- MU' ]) -Int Cmem(SCHED, MU)) ~> #deductGas ... </k>
         <memoryUsed> MU => MU [ MEMINDEX <- MU' ] </memoryUsed> <schedule> SCHED </schedule>
      requires MU' <Int pow256

    rule <k> G:Int ~> #deductGas => #exception ... </k> <gas> GAVAIL                  </gas> requires GAVAIL <Int G
    rule <k> G:Int ~> #deductGas => .          ... </k> <gas> GAVAIL => GAVAIL -Int G </gas> <previousGas> _ => GAVAIL </previousGas> requires GAVAIL >=Int G

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
         <coinbase> MINER </coinbase>
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
         <coinbase> MINER </coinbase>
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
         <coinbase> ACCT </coinbase>
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

Lists of opcodes form programs.

-   `#revOps` reverses a list of instructions.

```{.k .uiuck .rvk}
    syntax Ops [flatPredicate]
    syntax Ops ::= ".Ops" | Op ";" Ops
                 | #revOps  ( Ops , Ops ) [function]
 // ------------------------------------------------
    rule #revOps ( OP ; OPS , OPS' ) => #revOps(OPS, OP ; OPS')
    rule #revOps ( .Ops , OPS  ) => OPS
```

IELE Ops
--------

Each subsection has a different class of opcodes.
Organization is based roughly on what parts of the execution state are needed to compute the result of each operator.

### Internal Operations

-   `#newAccount_` allows declaring a new empty account with the given address (and assumes the rounding to 160 bits has already occured).
    If the account already exists with non-zero nonce or non-empty code, an exception is thrown.
    Otherwise, if the account already exists, the storage is cleared.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#newAccount" Int
 // ---------------------------------------
    rule <k> #newAccount ACCT => #exception ... </k>
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
               <codeSize> 0          </codeSize>
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

    rule <k> #transferFunds ACCTFROM ACCTTO VALUE => #exception ... </k>
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

We use `INVALID` both for marking the designated invalid operator and for garbage bytes in the input program.
Executing the INVALID instruction results in an exception.

```{.k .uiuck .rvk}
    syntax InvalidOp ::= "INVALID"
    syntax NullVoidOp ::= InvalidOp
 // -------------------------------
```

### Program Header

```{.k .uiuck .rvk}
    syntax HeaderOp ::= REGISTERS ( Int )
                      | CALLDEST ( Int , Int )
                      | EXTCALLDEST ( Int , Int )
                      | ConstantOp
    syntax ConstantOp ::= FUNCTION ( String )
                        | CONTRACT ( Ops , Int )

    syntax Ops ::= "#emptyCode"
 // ---------------------------
    rule #emptyCode => FUNCTION("deposit") ; EXTCALLDEST(1, 0); .Ops [macro]
```

### Register Manipulations

Some operators don't calculate anything, they just manipulate the state of registers.

```{.k .uiuck .rvk}
    syntax NullOp ::= LOADPOS ( Int , Int )
 // ---------------------------------------
    rule <k> LOADPOS(_, W) REG => #load REG W ... </k> 

    syntax NullOp ::= LOADNEG ( Int , Int )
 // ---------------------------------------
    rule <k> LOADNEG(_, W) REG => #load REG (0 -Int W) ... </k> 

    syntax UnOp ::= "MOVE"
 // ----------------------
    rule <k> MOVE REG VAL => #load REG VAL ... </k>

    syntax InternalOp ::= "#load" Reg Int
 // -------------------------------------
    rule <k> #load % REG VALUE => . ... </k>
         <regs> REGS => REGS [ REG <- VALUE ] </regs>

    syntax InternalOp ::= "#load" Regs Ints
 // ---------------------------------------
    rule <k> #load (REG REGS) (VALUE VALUES) => #load REG VALUE ~> #load REGS VALUES ... </k>
    rule <k> #load .Regs      .Regs          => .                                    ... </k>
    rule <k> #load (REG REGS) .Regs          => #exception                           ... </k>
    rule <k> #load .Regs      (VALUE VALUES) => #exception                           ... </k>
```

### Local Memory

These operations are getters/setters of the local execution memory.

```{.k .uiuck .rvk}
    syntax TernOp ::= "MLOADN"
 // --------------------------
    rule <k> MLOADN REG INDEX1 INDEX2 WIDTH => #load REG #asSigned({LM [ INDEX1 ]}:>WordStack [ INDEX2 .. WIDTH ]) ... </k>
         <localMem> LM </localMem>

    syntax UnOp ::= "MLOAD"
 // -----------------------
    rule <k> MLOAD REG INDEX => #load REG #asSigned({LM [ INDEX ]}:>WordStack) ... </k>
         <localMem> LM </localMem>

    syntax QuadVoidOp ::= "MSTOREN"
 // -------------------------------
    rule <k> MSTOREN INDEX1 INDEX2 VALUE WIDTH => . ... </k>
         <localMem> LM => LM [ INDEX1 <- {LM [ INDEX1 ]}:>WordStack [ INDEX2 := #padToWidth(WIDTH, #asUnsignedBytes(VALUE modInt (2 ^Int (WIDTH *Int 8)))) ] ] </localMem>

    syntax BinVoidOp ::= "MSTORE"
 // -----------------------------
    rule <k> MSTORE INDEX VALUE => . ... </k>
         <localMem> LM => LM [ INDEX <- #asSignedBytes(VALUE) ] </localMem>
```

### Expressions

Expression calculations are simple and don't require anything but the arguments from the `regs` to operate.

```{.k .uiuck .rvk}
    syntax UnOp ::= "ISZERO" | "NOT"
 // --------------------------------
    rule <k> ISZERO REG 0 => #load REG 1       ... </k>
    rule <k> ISZERO REG W => #load REG 0       ... </k> requires W =/=K 0
    rule <k> NOT    REG W => #load REG ~Int W ... </k>

    syntax BinOp ::= "ADD" | "MUL" | "SUB" | "DIV" | "EXP" | "MOD"
 // --------------------------------------------------------------
    rule <k> ADD REG W0 W1 => #load REG W0 +Int W1 ... </k>
    rule <k> MUL REG W0 W1 => #load REG W0 *Int W1 ... </k>
    rule <k> SUB REG W0 W1 => #load REG W0 -Int W1 ... </k>
    rule <k> DIV REG W0 W1 => #load REG W0 /Int W1 ... </k> requires W1 =/=Int 0
    rule <k> DIV REG W0  0 => #load REG 0          ... </k>
    rule <k> EXP REG W0 W1 => #load REG W0 ^Int W1 ... </k>
    rule <k> MOD REG W0 W1 => #load REG W0 %Int W1 ... </k> requires W1 =/=Int 0
    rule <k> MOD REG W0  0 => #load REG 0          ... </k>

    syntax TernOp ::= "ADDMOD" | "MULMOD" | "EXPMOD"
 // ------------------------------------------------
    rule <k> ADDMOD REG W0 W1 W2 => #load REG (W0 +Int W1) %Int W2 ... </k> requires W2 =/=Int 0
    rule <k> ADDMOD REG W0 W1  0 => #load REG 0                    ... </k>
    rule <k> MULMOD REG W0 W1 W2 => #load REG (W0 *Int W1) %Int W2 ... </k> requires W2 =/=Int 0
    rule <k> MULMOD REG W0 W1  0 => #load REG 0                    ... </k>
    rule <k> EXPMOD REG W0 W1 W2 => #load REG powmod(W0,W1,W2)     ... </k>

    syntax BinOp ::= "BYTE" | "SIGNEXTEND" | "TWOS"
 // -----------------------------------------------
    rule <k> BYTE REG INDEX W        => #load REG byte(chop(INDEX), W)     ... </k>
    rule <k> SIGNEXTEND REG WIDTH W1 => #load REG signextend(chop(WIDTH), W1) ... </k>
    rule <k> TWOS REG WIDTH W1       => #load REG twos(chop(WIDTH), W1)       ... </k>

    syntax BinOp ::= "AND" | "OR" | "XOR"
 // -------------------------------------
    rule <k> AND REG W0 W1 => #load REG W0 &Int W1   ... </k>
    rule <k> OR  REG W0 W1 => #load REG W0 |Int W1   ... </k>
    rule <k> XOR REG W0 W1 => #load REG W0 xorInt W1 ... </k>

    syntax BinOp ::= "LT" | "GT" | "EQ"
 // -----------------------------------
    rule <k> LT REG W0 W1 => #load REG 1 ... </k>  requires W0 <Int   W1
    rule <k> LT REG W0 W1 => #load REG 0 ... </k>  requires W0 >=Int  W1
    rule <k> GT REG W0 W1 => #load REG 1 ... </k>  requires W0 >Int   W1
    rule <k> GT REG W0 W1 => #load REG 0 ... </k>  requires W0 <=Int  W1
    rule <k> EQ REG W0 W1 => #load REG 1 ... </k>  requires W0 ==Int  W1
    rule <k> EQ REG W0 W1 => #load REG 0 ... </k>  requires W0 =/=Int W1

    syntax UnOp ::= "SHA3"
 // ----------------------
    rule <k> SHA3 REG MEMINDEX => #load REG keccak({LM [ MEMINDEX ]}:>WordStack) ... </k>
         <localMem> LM </localMem>
```

### Local State

These operators make queries about the current execution state.

```{.k .uiuck .rvk}
    syntax NullOp ::= "GAS" | "GASPRICE" | "GASLIMIT"
 // -------------------------------------------------
    rule <k> GAS      REG => #load REG GAVAIL ... </k> <gas> GAVAIL </gas>
    rule <k> GASPRICE REG => #load REG GPRICE ... </k> <gasPrice> GPRICE </gasPrice>
    rule <k> GASLIMIT REG => #load REG GLIMIT ... </k> <gasLimit> GLIMIT </gasLimit>

    syntax NullOp ::= "COINBASE" | "TIMESTAMP" | "NUMBER" | "DIFFICULTY"
 // --------------------------------------------------------------------
    rule <k> COINBASE   REG => #load REG CB   ... </k> <coinbase> CB </coinbase>
    rule <k> TIMESTAMP  REG => #load REG TS   ... </k> <timestamp> TS </timestamp>
    rule <k> NUMBER     REG => #load REG NUMB ... </k> <number> NUMB </number>
    rule <k> DIFFICULTY REG => #load REG DIFF ... </k> <difficulty> DIFF </difficulty>

    syntax NullOp ::= "ADDRESS" | "ORIGIN" | "CALLER" | "CALLVALUE"
 // ---------------------------------------------------------------
    rule <k> ADDRESS   REG => #load REG ACCT ... </k> <id> ACCT </id>
    rule <k> ORIGIN    REG => #load REG ORG  ... </k> <origin> ORG </origin>
    rule <k> CALLER    REG => #load REG CL   ... </k> <caller> CL </caller>
    rule <k> CALLVALUE REG => #load REG CV   ... </k> <callValue> CV </callValue>

    syntax NullOp ::= "MSIZE" | "CODESIZE"
 // --------------------------------------
    rule <k> MSIZE    REG => #load REG 32 *Int #msize(MU) ... </k> <memoryUsed> MU </memoryUsed>
    rule <k> CODESIZE REG => #load REG SIZE               ... </k> <programSize> SIZE </programSize>

    syntax UnOp ::= "BLOCKHASH"
 // ---------------------------
    rule <k> BLOCKHASH REG N => #load REG #if N >=Int HI orBool HI -Int 256 >Int N #then 0 #else #parseHexWord(Keccak256(Int2String(N))) #fi ... </k> <number> HI </number> <mode> VMTESTS </mode>

    rule <k> BLOCKHASH REG N => #load REG #blockhash(HASHES, N, HI -Int 1, 0) ... </k> <number> HI </number> <blockhash> HASHES </blockhash> <mode> NORMAL </mode>

    syntax Int ::= #blockhash ( List , Int , Int , Int ) [function]
 // ---------------------------------------------------------------
    rule #blockhash(_, N, HI, _) => 0 requires N >Int HI
    rule #blockhash(_, _, _, 256) => 0
    rule #blockhash(ListItem(0) _, _, _, _) => 0
    rule #blockhash(ListItem(H) _, N, N, _) => H
    rule #blockhash(ListItem(_) L, N, HI, A) => #blockhash(L, N, HI -Int 1, A +Int 1) [owise]
```

### `JUMP*`

The `JUMP*` family of operations affect the current program counter.

```{.k .uiuck .rvk}
    syntax NullVoidOp ::= JUMPDEST ( Int )
 // --------------------------------------
    rule <k> JUMPDEST(_) => . ... </k>

    syntax NullVoidOp ::= JUMP ( Int )
 // ----------------------------------
    rule <k> JUMP(LABEL) ~> _:Ops => CODE ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> ... LABEL |-> CODE </jumpTable> </function>

    syntax UnVoidOp ::= JUMPI ( Int )
 // ---------------------------------
    rule <k> JUMPI(LABEL) I ~> _:Ops => CODE ... </k> <fid> FUNC </fid> <function>... <funcId> FUNC </funcId> <jumpTable> ... LABEL |-> CODE </jumpTable> </function> requires I =/=K 0
    rule <k> JUMPI(LABEL) 0          => .    ... </k>

    syntax LocalCall ::= "{" Ops "|" Int "|" Regs "|" Array "}"
 // -----------------------------------------------------------

    syntax LocalCallOp ::= LOCALCALL ( Int , Int , Int )
 // ----------------------------------------------------
    rule <k> LOCALCALL(LABEL, NARGS, NRETURNS) RETURNS ARGS ~> OPS:Ops => #load #regRange(NARGS) ARGS ~> #execute ... </k>
         <fid> FUNC => LABEL </fid>
         <regs> REGS => .Array </regs>
         <nregs> NREGS </nregs>
         <localCalls> .List => ListItem({ OPS | FUNC | RETURNS | REGS }) ... </localCalls>
```

### `STOP`, `REVERT`, and `RETURN`

```{.k .uiuck .rvk}
    syntax NullVoidOp ::= "STOP"
 // ----------------------------
    rule <k> STOP => #end ... </k>
         <output> _ => .Regs </output>

    syntax ReturnOp ::= RETURN ( Int )
 // -----------------------------------
    rule <k> RETURN (NRETURNS) VALUES => #end ... </k>
         <output> _ => VALUES </output>
         <localCalls> .List </localCalls>

    rule <k> RETURN(NRETURNS) VALUES ~> _:Ops => #load RETURNS VALUES ~> OPS ... </k>
         <fid> _ => FUNC </fid>
         <regs> _ => REGS </regs>
         <localCalls> ListItem({ OPS | FUNC | RETURNS | REGS }) => .List ... </localCalls>

    syntax ReturnOp ::= REVERT ( Int )
 // ----------------------------------
    rule <k> REVERT(NRETURNS) VALUES => #revert ... </k>
         <output> _ => VALUES </output>
```

### Log Operations

During execution of a transaction some things are recorded in the substate log (Section 6.1 in yellowpaper).
This is a right cons-list of `SubstateLogEntry` (which contains the account ID along with the specified portions of the `wordStack` and `localMem`).

```{.k .uiuck .rvk}
    syntax SubstateLogEntry ::= "{" Int "|" WordStack "|" WordStack "}"
 // -------------------------------------------------------------------
```

```{.k .uiuck .rvk}
    syntax UnVoidOp  ::= "LOG0"
    syntax BinVoidOp ::= "LOG1"
    syntax TernVoidOp ::= "LOG2"
    syntax QuadVoidOp ::= "LOG3"
    syntax FiveVoidOp  ::= "LOG4"
 // -----------------------------

    rule LOG0 MEMINDEX => #log MEMINDEX .WordStack
    rule LOG1 MEMINDEX W0 => #log MEMINDEX W0 : .WordStack
    rule LOG2 MEMINDEX W0 W1 => #log MEMINDEX W0 : W1 : .WordStack
    rule LOG3 MEMINDEX W0 W1 W2 => #log MEMINDEX W0 : W1 : W2 : .WordStack
    rule LOG4 MEMINDEX W0 W1 W2 W3 => #log MEMINDEX W0 : W1 : W2 : W3 : .WordStack

    syntax InternalOp ::= "#log" Int WordStack
 // ----------------------------------------------
    rule <k> #log MEMINDEX WS => . ... </k>
         <id> ACCT </id>
         <localMem> LM </localMem>
         <log> ... (.List => ListItem({ ACCT | WS | {LM [ MEMINDEX ]}:>WordStack })) </log>
```

Network Ops
-----------

Operators that require access to the rest of the Ethereum network world-state can be taken as a first draft of a "blockchain generic" language.

### Account Queries

TODO: It's unclear what to do in the case of an account not existing for these operators.
`BALANCE` is specified to push 0 in this case, but the others are not specified.
For now, I assume that they instantiate an empty account and use the empty data.

```{.k .uiuck .rvk}
    syntax UnOp ::= "BALANCE"
 // -------------------------
    rule <k> BALANCE REG ACCT => #load REG BAL ... </k>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>

    rule <k> BALANCE REG ACCT => #newAccount ACCT ~> #load REG 0 ... </k>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool ACCT in_keys(ACCTS)

    syntax UnOp ::= "EXTCODESIZE"
 // -----------------------------
    rule <k> EXTCODESIZE REG ACCT => #load REG SIZE ... </k>
         <account>
           <acctID> ACCT </acctID>
           <codeSize> SIZE </codeSize>
           ...
         </account>

    rule <k> EXTCODESIZE REG ACCT => #newAccount ACCT ~> #load REG 0 ... </k>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool ACCT in_keys(ACCTS)
```

### Account Storage Operations

These operations interact with the account storage.

```{.k .uiuck .rvk}
    syntax UnOp ::= "SLOAD"
 // -----------------------
    rule <k> SLOAD REG INDEX => #load REG 0 ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account> requires notBool INDEX in_keys(STORAGE)

    rule <k> SLOAD REG INDEX => #load REG VALUE ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> ... INDEX |-> VALUE ... </storage>
           ...
         </account>

    syntax BinVoidOp ::= "SSTORE"
 // -----------------------------
    rule <k> SSTORE INDEX VALUE => . ... </k>
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
         requires INDEX >=Int 0 andBool INDEX <Int pow256

    rule <k> SSTORE INDEX VALUE => . ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE => STORAGE [ INDEX <- VALUE ] </storage>
           ...
         </account>
      requires notBool (INDEX in_keys(STORAGE))
       andBool INDEX >=Int 0 andBool INDEX <Int pow256

    rule <k> SSTORE (INDEX => chop(INDEX)) _ ... </k>
      requires INDEX <Int 0 orBool INDEX >=Int pow256
```

### Call Operations

The various `CALL*` (and other inter-contract control flow) operations will be desugared into these `InternalOp`s.

```{.k .uiuck .rvk}

-   `#call_____` takes the calling account, the account to execute as, the account whose code should execute, the gas limit, the amount to transfer, and the arguments.
-   `#callWithCode______` takes the calling account, the accout to execute as, the code to execute (as a map), the gas limit, the amount to transfer, and the arguments.
-   `#return__` is a placeholder for the calling program, specifying where to place the returned data in memory.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#checkCall" Int Int
                        | "#call" Int Int Int String Int Int Int Ints Bool
                        | "#callWithCode" Int Int ProgramCell String Ops Int Int Int Ints Bool
                        | "#mkCall" Int Int ProgramCell String Ops Int Int Int Ints Bool
 // --------------------------------------------------------------------------------
    rule <k> #checkCall ACCT VALUE ~> #call _ _ _ _ GLIMIT _ _ _ _ => #refund GLIMIT ~> #pushCallStack ~> #pushWorldState ~> #pushSubstate ~> #exception ... </k>
         <callDepth> CD </callDepth>
         <output> _ => .Regs </output>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>
      requires VALUE >Int BAL orBool CD >=Int 1024

     rule <k> #checkCall ACCT VALUE => . ... </k>
         <callDepth> CD </callDepth>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>
      requires notBool (VALUE >Int BAL orBool CD >=Int 1024)

    rule <k> #call ACCTFROM ACCTTO ACCTCODE FUNC GLIMIT VALUE APPVALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO #precompiled FUNC #emptyCode GLIMIT VALUE APPVALUE ARGS STATIC
         ...
         </k>
         <schedule> SCHED </schedule>
      requires ACCTCODE ==Int #precompiledAccount

    rule <k> #call ACCTFROM ACCTTO ACCTCODE FUNC GLIMIT VALUE APPVALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO #loadCode(CODE, SIZE) FUNC CODE GLIMIT VALUE APPVALUE ARGS STATIC
         ...
         </k>
         <schedule> SCHED </schedule>
         <acctID> ACCTCODE </acctID>
         <code> CODE </code>
         <codeSize> SIZE </codeSize>
      requires ACCTCODE =/=Int #precompiledAccount

    rule <k> #call ACCTFROM ACCTTO ACCTCODE FUNC GLIMIT VALUE APPVALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO #loadCode(#emptyCode, 0) FUNC #emptyCode GLIMIT VALUE APPVALUE ARGS STATIC
         ...
         </k>
         <activeAccounts> ACCTS </activeAccounts>
         <schedule> SCHED </schedule>
      requires ACCTCODE =/=Int #precompiledAccount andBool notBool ACCTCODE in_keys(ACCTS)

    rule #callWithCode ACCTFROM ACCTTO CODE FUNC BYTES GLIMIT VALUE APPVALUE ARGS STATIC
      => #pushCallStack ~> #pushWorldState ~> #pushSubstate
      ~> #transferFunds ACCTFROM ACCTTO VALUE
      ~> #mkCall ACCTFROM ACCTTO CODE FUNC BYTES GLIMIT VALUE APPVALUE ARGS STATIC

    rule <k> #mkCall ACCTFROM ACCTTO CODE FUNC BYTES GLIMIT VALUE APPVALUE ARGS STATIC:Bool
          => #initVM(ARGS) ~> #initFun(FUNC, #sizeRegs(ARGS))
         ...
         </k>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => ARGS </callData>
         <callValue> _ => APPVALUE </callValue>
         <id> _ => ACCTTO </id>
         <gas> _ => GLIMIT </gas>
         <caller> _ => ACCTFROM </caller>
         (<program> _ </program> => CODE:ProgramCell)
         <static> OLDSTATIC:Bool => OLDSTATIC orBool STATIC </static>

    syntax KItem ::= #initVM ( Ints )
                   | #initFun ( String , Int ) [klabel(initFunName)]
                   | #initFun ( Int , Int )    [klabel(initFunLabel)]
 // -----------------------------------------------------------------
    rule <k> #initVM(ARGS) => #load #regRange(#sizeRegs(ARGS)) ARGS ... </k>
         <memoryUsed> _ => .Map    </memoryUsed>
         <output>     _ => .Regs   </output>
         <regs>       _ => .Array  </regs>
         <localMem>   _ => .Memory </localMem>
         <localCalls> _ => .List   </localCalls>

    rule <k> #initFun(FUNC::String, NARGS) => #initFun(LABEL::Int, NARGS) ... </k>
         <exported> ... FUNC |-> LABEL </exported>

    rule <k> #initFun(FUNC::String, _) => #exception ... </k>
         <exported> FUNCS </exported>
      requires notBool FUNC in_keys(FUNCS)

    rule <k> #initFun(LABEL::Int, _) => #exception ... </k>
         <funcIds> LABELS </funcIds>
      requires notBool LABEL in LABELS

    rule <k> #initFun(LABEL::Int, NARGS) => #exception ... </k>
         <funcId> LABEL </funcId>
         <nparams> NPARAMS </nparams>
      requires NARGS =/=Int NPARAMS

    rule <k> #initFun(LABEL::Int, NARGS) => #if EXECMODE ==K VMTESTS #then #end #else #execute #fi ... </k>
         <mode> EXECMODE </mode>
         <funcIds> ... SetItem(LABEL) </funcIds>
         <fid> _ => LABEL </fid>
         <funcId> LABEL </funcId>
         <nparams> NARGS </nparams>

    syntax KItem ::= "#return" Regs Reg
 // -----------------------------------
    rule <k> #exception ~> #return _ REG
          => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0
         ...
         </k>
         <output> _ => .Regs </output>

    rule <k> #revert ~> #return REGS REG
           => #popCallStack
           ~> #popWorldState
           ~> #popSubstate
           ~> #load REG 0 ~> #refund GAVAIL ~> #load REGS OUT
          ...
         </k>
         <output> OUT </output>
         <gas> GAVAIL </gas>

    rule <mode> EXECMODE </mode>
         <k> #end ~> #return REGS REG
          => #popCallStack
          ~> #if EXECMODE ==K VMTESTS #then #popWorldState #else #dropWorldState #fi
          ~> #dropSubstate
          ~> #load REG 1 ~> #refund GAVAIL ~> #if EXECMODE ==K VMTESTS #then .K #else #load REGS OUT #fi
         ...
         </k>
         <output> OUT </output>
         <gas> GAVAIL </gas>

    syntax InternalOp ::= "#refund" Int
 // ------------------------------------------------------
    rule <k> #refund G => . ... </k> <gas> GAVAIL => GAVAIL +Int G </gas>
```

For each `CALL*` operation, we make a corresponding call to `#call` and a state-change to setup the custom parts of the calling environment.

```{.k .uiuck .rvk}
    syntax CallOp ::= CallOpCode "(" Int "," Int "," Int ")"
    syntax CallSixOp ::= CallSixOpCode "(" Int "," Int "," Int ")"
 // ------------------------------------------------------

    syntax CallOpCode ::= "CALL"
 // ----------------------------
    rule <k> CALL(LABEL,_,_) REG GCAP ACCTTO VALUE REGS ARGS
          => #checkCall ACCTFROM VALUE
          ~> #call ACCTFROM ACCTTO ACCTTO FUNC Ccallgas(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, VALUE) VALUE VALUE ARGS false
          ~> #return REGS REG
         ...
         </k>
         <constants> ... LABEL |-> FUNCTION(FUNC) </constants>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    syntax CallOpCode ::= "CALLCODE"
 // --------------------------------
    rule <k> CALLCODE(LABEL,_,_) REG GCAP ACCTTO VALUE REGS ARGS
          => #checkCall ACCTFROM VALUE
          ~> #call ACCTFROM ACCTFROM ACCTTO FUNC Ccallgas(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, VALUE) VALUE VALUE ARGS false
          ~> #return REGS REG
         ...
         </k>
         <constants> ... LABEL |-> FUNCTION(FUNC) </constants>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    syntax CallSixOpCode ::= "DELEGATECALL"
 // ---------------------------------------
    rule <k> DELEGATECALL(LABEL,_,_) REG GCAP ACCTTO REGS ARGS
          => #checkCall ACCTFROM 0
          ~> #call ACCTAPPFROM ACCTFROM ACCTTO FUNC Ccallgas(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, 0) 0 VALUE ARGS false
          ~> #return REGS REG
         ...
         </k>
         <constants> ... LABEL |-> FUNCTION(FUNC) </constants>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <caller> ACCTAPPFROM </caller>
         <callValue> VALUE </callValue>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    syntax CallSixOpCode ::= "STATICCALL"
 // -------------------------------------
    rule <k> STATICCALL(LABEL,_,_) REG GCAP ACCTTO REGS ARGS
          => #checkCall ACCTFROM 0
          ~> #call ACCTFROM ACCTTO ACCTTO FUNC Ccallgas(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, 0) 0 0 ARGS true
          ~> #return REGS REG
         ...
         </k>
         <constants> ... LABEL |-> FUNCTION(FUNC) </constants>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>
```

### Account Creation/Deletion

-   `#create____` transfers the endowment to the new account and triggers the execution of the initialization code.
-   `#codeDeposit_` checks the result of initialization code and whether the code deposit can be paid, indicating an error if not.

```{.k .uiuck .rvk}
    syntax InternalOp ::= "#create" Int Int Int Int Ops Int Ints
                        | "#mkCreate" Int Int Ops Int Int Int Ints
                        | "#checkCreate" Int Int
 // --------------------------------------------
    rule <k> #checkCreate ACCT VALUE ~> #create _ _ GAVAIL _ _ _ _ => #refund GAVAIL ~> #pushCallStack ~> #pushWorldState ~> #pushSubstate ~> #exception ... </k>
         <callDepth> CD </callDepth>
         <output> _ => .Regs </output>
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
      requires notBool (VALUE >Int BAL orBool CD >=Int 1024)

    rule #create ACCTFROM ACCTTO GAVAIL VALUE CODE SIZE ARGS
      => #pushCallStack ~> #pushWorldState ~> #pushSubstate
      ~> #newAccount ACCTTO
      ~> #transferFunds ACCTFROM ACCTTO VALUE
      ~> #mkCreate ACCTFROM ACCTTO CODE SIZE GAVAIL VALUE ARGS

    rule <mode> EXECMODE </mode>
         <k> #mkCreate ACCTFROM ACCTTO CODE LEN GAVAIL VALUE ARGS
          => #initVM(ARGS) ~> #initFun(0, #sizeRegs(ARGS))
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT => ACCTTO </id>
         <gas> OLDGAVAIL => GAVAIL </gas>
         (<program> _ </program> => #loadCode(CODE, LEN))
         <caller> _ => ACCTFROM </caller>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => .Regs </callData>
         <callValue> _ => VALUE </callValue>
         <account>
           <acctID> ACCTTO </acctID>
           <nonce> NONCE => NONCE +Int 1 </nonce>
           ...
         </account>
         <activeAccounts> ... ACCTTO |-> (EMPTY => false) ... </activeAccounts>

    syntax KItem ::= "#codeDeposit" Int Int Ops Reg
                   | "#mkCodeDeposit" Int Int Ops Reg
                   | "#finishCodeDeposit" Int Ops Reg
 // -------------------------------------------------------
    rule <k> #exception ~> #codeDeposit _ _ _ REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0 ... </k> <output> _ => .Regs </output>
    rule <k> #revert ~> #codeDeposit _ _ _ REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ~> #load REG 0 ... </k>
         <gas> GAVAIL </gas>

    rule <mode> EXECMODE </mode>
         <k> #end ~> #codeDeposit ACCT LEN CODE REG => #mkCodeDeposit ACCT LEN CODE REG ... </k>

    rule <k> #mkCodeDeposit ACCT LEN CODE REG
          => #if EXECMODE ==K VMTESTS #then . #else Gcodedeposit < SCHED > *Int LEN ~> #deductGas #fi
          ~> #finishCodeDeposit ACCT CODE REG
         ...
         </k>
         <mode> EXECMODE </mode>
         <schedule> SCHED </schedule>
         <output> .Regs </output>
      requires LEN <=Int maxCodeSize < SCHED >

    rule <k> #mkCodeDeposit ACCT LEN _ REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0 ... </k>
         <schedule> SCHED </schedule>
         <output> .Regs </output>
      requires LEN >Int maxCodeSize < SCHED >

    rule <k> #finishCodeDeposit ACCT CODE REG
          => #popCallStack ~> #if EXECMODE ==K VMTESTS #then #popWorldState #else #dropWorldState #fi ~> #dropSubstate
          ~> #refund GAVAIL ~> #load REG ACCT
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

    rule <k> #exception ~> #finishCodeDeposit _ _ REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0 ... </k>
```

`CREATE` will attempt to `#create` the account using the initialization code and cleans up the result with `#codeDeposit`.

```{.k .uiuck .rvk}
    syntax CreateOp ::= CREATE ( Int , Int )
    syntax Contract ::= "{" Ops "|" Int "}"
 // ---------------------------------------
    rule <k> CREATE(LABEL,_) REG VALUE ARGS
          => #checkCreate ACCT VALUE
          ~> #create ACCT #newAddr(ACCT, NONCE) #if Gstaticcalldepth << SCHED >> #then GAVAIL #else #allBut64th(GAVAIL) #fi VALUE CODE LEN ARGS
          ~> #codeDeposit #newAddr(ACCT, NONCE) LEN CODE REG
         ...
         </k>
         <constants> ... LABEL |-> CONTRACT(CODE, LEN) </constants>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <gas> GAVAIL => #if Gstaticcalldepth << SCHED >> #then 0 #else GAVAIL /Int 64 #fi </gas>
         <account>
           <acctID> ACCT </acctID>
           <nonce> NONCE </nonce>
           ...
         </account>

    syntax CopyCreateOp ::= COPYCREATE ( Int )
 // ------------------------------------------
    rule <k> COPYCREATE(_) REG ACCTCODE VALUE ARGS
          => #checkCreate ACCT VALUE
          ~> #create ACCT #newAddr(ACCT, NONCE) #if Gstaticcalldepth << SCHED >> #then GAVAIL #else #allBut64th(GAVAIL) #fi VALUE CODE LEN ARGS
          ~> #codeDeposit #newAddr(ACCT, NONCE) LEN CODE REG
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
           <codeSize> LEN </codeSize>
           ...
         </account>
         requires ACCT =/=Int ACCTCODE

    rule <k> COPYCREATE(_) REG ACCT VALUE ARGS
          => #checkCreate ACCT VALUE
          ~> #create ACCT #newAddr(ACCT, NONCE) #if Gstaticcalldepth << SCHED >> #then GAVAIL #else #allBut64th(GAVAIL) #fi VALUE CODE LEN ARGS
          ~> #codeDeposit #newAddr(ACCT, NONCE) LEN CODE REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <gas> GAVAIL => #if Gstaticcalldepth << SCHED >> #then 0 #else GAVAIL /Int 64 #fi </gas>
         <account>
           <acctID> ACCT </acctID>
           <nonce> NONCE </nonce>
           <code> CODE </code>
           <codeSize> LEN </codeSize>
           ...
         </account>
```

`SELFDESTRUCT` marks the current account for deletion and transfers funds out of the current account.
Self destructing to yourself, unlike a regular transfer, destroys the balance in the account, irreparably losing it.

```{.k .uiuck .rvk}
    syntax UnVoidOp ::= "SELFDESTRUCT"
 // ----------------------------------
    rule <k> SELFDESTRUCT ACCTTO => #transferFunds ACCT ACCTTO BALFROM ~> #end ... </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <selfDestruct> SDS (.Set => SetItem(ACCT)) </selfDestruct>
         <refund> RF => #if ACCT in SDS #then RF #else RF +Int Rselfdestruct < SCHED > #fi </refund>
         <account>
           <acctID> ACCT </acctID>
           <balance> BALFROM </balance>
           ...
         </account>
         <output> _ => .Regs </output>
      requires ACCT =/=Int ACCTTO

    rule <k> SELFDESTRUCT ACCT => #end ... </k>
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
         <output> _ => .Regs </output>

```

Precompiled Contracts
=====================

-   `#precompiled` is a placeholder for the 4 pre-compiled contracts at addresses 1 through 4.

```{.k .uiuck .rvk}
    syntax NullVoidOp   ::= PrecompiledOp
    syntax Int ::= "#precompiledAccount" [function]
 // -----------------------------------------------
    rule #precompiledAccount => 1

    syntax ProgramCell ::= "#precompiled" [function]
 // ------------------------------------------------
    rule #precompiled =>
         <program>
           <functions>
             <function> <funcId> 1 </funcId> <instructions> ECREC;     .Ops </instructions> <nparams> 4 </nparams> ... </function>
             <function> <funcId> 2 </funcId> <instructions> SHA256;    .Ops </instructions> <nparams> 2 </nparams> ... </function>
             <function> <funcId> 3 </funcId> <instructions> RIP160;    .Ops </instructions> <nparams> 2 </nparams> ... </function>
             <function> <funcId> 4 </funcId> <instructions> ID;        .Ops </instructions> <nparams> 1 </nparams> ... </function>
             <function> <funcId> 5 </funcId> <instructions> ECADD;     .Ops </instructions> <nparams> 4 </nparams> ... </function>
             <function> <funcId> 6 </funcId> <instructions> ECMUL;     .Ops </instructions> <nparams> 3 </nparams> ... </function>
             <function> <funcId> 7 </funcId> <instructions> ECPAIRING; .Ops </instructions> <nparams> 6 </nparams> ... </function>
           </functions>
           <funcIds> SetItem(1) SetItem(2) SetItem(3) SetItem(4) SetItem(5) SetItem(6) SetItem(7) </funcIds>
           <constants> 
             1 |-> FUNCTION("ECREC")
             2 |-> FUNCTION("SHA256")
             3 |-> FUNCTION("RIP160")
             4 |-> FUNCTION("ID")
             5 |-> FUNCTION("ECADD")
             6 |-> FUNCTION("ECMUL")
             7 |-> FUNCTION("ECPAIRING")
           </constants>
           <exported>
             "ECREC"     |-> 1
             "SHA256"    |-> 2
             "RIP160"    |-> 3
             "ID"        |-> 4
             "ECADD"     |-> 5
             "ECMUL"     |-> 6
             "ECPAIRING" |-> 7
           </exported>
           ...
         </program>
```

-   `ECREC` performs ECDSA public key recovery.
-   `SHA256` performs the SHA2-257 hash function.
-   `RIP160` performs the RIPEMD-160 hash function.
-   `ID` is the identity function (copies input to output).

```{.k .uiuck .rvk}
    syntax PrecompiledOp ::= "ECREC"
 // --------------------------------
    rule <k> ECREC => #end ... </k>
         <callData> HASH V R S .Regs </callData>
         <output> _ => #ecrec(#sender(#unparseByteStack(#padToWidth(32, #asUnsignedBytes(HASH))), V, #unparseByteStack(#padToWidth(32, #asUnsignedBytes(R))), #unparseByteStack(#padToWidth(32, #asUnsignedBytes(S))))) </output>

    syntax Ints ::= #ecrec ( Account ) [function]
 // ---------------------------------------------
    rule #ecrec(.Account) => -1 .Regs
    rule #ecrec(N:Int)    => N .Regs

    syntax PrecompiledOp ::= "SHA256"
 // ---------------------------------
    rule <k> SHA256 => #end ... </k>
         <callData> LEN DATA .Regs </callData>
         <output> _ => #parseHexWord(Sha256(#unparseByteStack(#padToWidth(LEN, #asUnsignedBytes(DATA))))) .Regs </output>

    syntax PrecompiledOp ::= "RIP160"
 // ---------------------------------
    rule <k> RIP160 => #end ... </k>
         <callData> LEN DATA .Regs </callData>
         <output> _ => #parseHexWord(RipEmd160(#unparseByteStack(#padToWidth(LEN, #asUnsignedBytes(DATA))))) .Regs </output>

    syntax PrecompiledOp ::= "ID"
 // -----------------------------
    rule <k> ID => #end ... </k>
         <callData> DATA </callData>
         <output> _ => DATA </output>

    syntax PrecompiledOp ::= "ECADD"
 // --------------------------------
    rule <k> ECADD => #ecadd((X1, Y1), (X2, Y2)) ... </k>
         <callData> X1 Y1 X2 Y2 .Regs </callData>

    syntax InternalOp ::= #ecadd(G1Point, G1Point)
 // ----------------------------------------------
    rule #ecadd(P1, P2) => #exception
      requires notBool isValidPoint(P1) orBool notBool isValidPoint(P2)
    rule <k> #ecadd(P1, P2) => #end ... </k> <output> _ => #point(BN128Add(P1, P2)) </output>
      requires isValidPoint(P1) andBool isValidPoint(P2)

    syntax PrecompiledOp ::= "ECMUL"
 // --------------------------------
    rule <k> ECMUL => #ecmul((X, Y), S) ... </k>
         <callData> X Y S .Regs </callData>

    syntax InternalOp ::= #ecmul(G1Point, Int)
 // ------------------------------------------
    rule #ecmul(P, S) => #exception
      requires notBool isValidPoint(P)
    rule <k> #ecmul(P, S) => #end ... </k> <output> _ => #point(BN128Mul(P, S)) </output>
      requires isValidPoint(P)

    syntax Ints ::= #point(G1Point) [function]
 // ------------------------------------------
    rule #point((X, Y)) => X Y .Regs

    syntax PrecompiledOp ::= "ECPAIRING"
 // ------------------------------------
    rule <k> ECPAIRING => #ecpairing(.List, .List, DATA) ... </k>
         <callData> DATA </callData>
      requires #sizeRegs(DATA) %Int 6 ==Int 0
    rule <k> ECPAIRING => #exception ... </k>
         <callData> DATA </callData>
      requires #sizeRegs(DATA) %Int 6 =/=Int 0

    syntax InternalOp ::= #ecpairing(List, List, Ints)
 // --------------------------------------------------
    rule (.K => #checkPoint) ~> #ecpairing((.List => ListItem((X, Y))) _, (.List => ListItem((A x B , C x D))) _, X Y A B C D REGS => REGS)
    rule <k> #ecpairing(A, B, .Regs) => #end ... </k>
         <output> _ => bool2Word(BN128AtePairing(A, B)) .Regs </output>

    syntax InternalOp ::= "#checkPoint"
 // -----------------------------------
    rule (#checkPoint => .) ~> #ecpairing(ListItem(AK::G1Point) _, ListItem(BK::G2Point) _, _)
      requires isValidPoint(AK) andBool isValidPoint(BK)
    rule #checkPoint ~> #ecpairing(ListItem(AK::G1Point) _, ListItem(BK::G2Point) _, _) => #exception
      requires notBool isValidPoint(AK) orBool notBool isValidPoint(BK)
```
    

Ethereum Gas Calculation
========================

The gas calculation is designed to mirror the style of the yellowpaper.
Gas is consumed either by increasing the amount of memory being used, or by executing opcodes.

Memory Consumption
------------------

Memory consumed is tracked to determine the appropriate amount of gas to charge for each operation.
In the yellowpaper, each opcode is defined to consume zero gas unless specified otherwise next to the semantics of the opcode (appendix H).

-   `#memory` computes the new memory size given the old size and next operator (with its arguments).
-   `#memoryUsageUpdate` is the function `M` in appendix H of the yellowpaper which helps track the memory used.

```{.k .uiuck .rvk}
    syntax Int ::= #memory ( Op , Map , Int ) [function]
                 | #memIndex ( Op )           [function]
    syntax Bool ::= #usesMemory ( OpCode )    [function]
 // ----------------------------------------------------
    rule #memory(MLOADN  _ INDEX1 INDEX2   WIDTH, _::Map MEMINDEX |-> MU, MEMINDEX) => #memoryUsageUpdate(MU, INDEX2, WIDTH)
    rule #memory(MSTOREN   INDEX1 INDEX2 _ WIDTH, _::Map MEMINDEX |-> MU, MEMINDEX) => #memoryUsageUpdate(MU, INDEX2, WIDTH)
    rule #memory(MSTORE INDEX VALUE,              _::Map MEMINDEX |-> MU, MEMINDEX) => #memoryUsageUpdate(MU, 0, #sizeWordStack(#asSignedBytes(VALUE)))

    rule #memIndex(MLOADN _ INDEX _ _)  => INDEX
    rule #memIndex(MLOAD _ INDEX)       => INDEX
    rule #memIndex(MSTOREN INDEX _ _ _) => INDEX
    rule #memIndex(MSTORE INDEX _)      => INDEX
    rule #memIndex(SHA3 _ INDEX)        => INDEX
    rule #memIndex(LOG0 INDEX)          => INDEX
    rule #memIndex(LOG1 INDEX _)        => INDEX
    rule #memIndex(LOG2 INDEX _ _)      => INDEX
    rule #memIndex(LOG3 INDEX _ _ _)    => INDEX
    rule #memIndex(LOG4 INDEX _ _ _ _)  => INDEX

    rule #usesMemory(MLOADN)  => true
    rule #usesMemory(MLOAD)   => true
    rule #usesMemory(MSTOREN) => true
    rule #usesMemory(MSTORE)  => true
    rule #usesMemory(SHA3)    => true
    rule #usesMemory(LOG0)    => true
    rule #usesMemory(LOG1)    => true
    rule #usesMemory(LOG2)    => true
    rule #usesMemory(LOG3)    => true
    rule #usesMemory(LOG4)    => true
    rule #usesMemory(...)     => false [owise]

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
    syntax InternalOp ::= #gasExec ( Schedule , Op )
 // ----------------------------------------------------
    rule <k> #gasExec(SCHED, SSTORE INDEX VALUE) => Csstore(SCHED, VALUE, #lookup(STORAGE, INDEX)) ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE </storage>
           ...
         </account>

    rule <k> #gasExec(SCHED, EXP _ W0 0)  => Gexp < SCHED > ... </k>
    rule <k> #gasExec(SCHED, EXP _ W0 W1) => Gexp < SCHED > +Int (Gexpbyte < SCHED > *Int (1 +Int (log256Int(W1)))) ... </k> requires W1 =/=K 0


    rule <k> #gasExec(SCHED, LOG0 IDX)         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (0 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, LOG1 IDX _)       => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (1 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, LOG2 IDX _ _)     => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (2 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, LOG3 IDX _ _ _)   => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (3 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>
    rule <k> #gasExec(SCHED, LOG4 IDX _ _ _ _) => (Glog < SCHED > +Int (Glogdata < SCHED > *Int #sizeWordStack({LM [ IDX ]}:>WordStack)) +Int (4 *Int Glogtopic < SCHED >)) ... </k> <localMem> LM </localMem>

    rule <k> #gasExec(SCHED, CALL(_,_,_) _ GCAP ACCTTO VALUE _ _) => Ccall(SCHED, ACCTTO,   ACCTS, GCAP, GAVAIL, VALUE) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, CALLCODE(_,_,_) _ GCAP _ VALUE _ _) => Ccall(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, VALUE) ... </k>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, DELEGATECALL(_,_,_) _ GCAP _ _ _) => Ccall(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, 0) ... </k>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, STATICCALL(_,_,_) _ GCAP ACCTTO _ _) => Ccall(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, 0) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, SELFDESTRUCT ACCTTO) => Cselfdestruct(SCHED, ACCTTO, ACCTS, BAL) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <id> ACCTFROM </id>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL </balance>
           ...
         </account>

    rule <k> #gasExec(SCHED, CREATE(_,_) _ _ _) => Gcreate < SCHED > ... </k>

    rule <k> #gasExec(SCHED, SHA3 _ IDX) => Gsha3 < SCHED > +Int (Gsha3word < SCHED > *Int (#sizeWordStack({LM [ IDX ]}:>WordStack) up/Int 32)) ... </k> <localMem> LM </localMem>

    rule <k> #gasExec(SCHED, JUMPDEST(_)) => Gjumpdest < SCHED > ... </k>
    rule <k> #gasExec(SCHED, SLOAD _ _)     => Gsload    < SCHED > ... </k>

    // Wzero
    rule <k> #gasExec(SCHED, STOP)         => Gzero < SCHED > ... </k>
    rule <k> #gasExec(SCHED, REVERT(_) _)  => Gzero < SCHED > ... </k>

    // Wbase
    rule <k> #gasExec(SCHED, ADDRESS _)        => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, ORIGIN _)         => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CALLER _)         => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CALLVALUE _)      => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CODESIZE _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, GASPRICE _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, COINBASE _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, TIMESTAMP _)      => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, NUMBER _)         => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, DIFFICULTY _)     => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, GASLIMIT _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MSIZE _)          => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, GAS _)            => Gbase < SCHED > ... </k>

    // Wverylow
    rule <k> #gasExec(SCHED, ADD _ _ _)          => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, SUB _ _ _)          => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, NOT _ _)            => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, LT _ _ _)           => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, GT _ _ _)           => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, EQ _ _ _)           => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, ISZERO _ _)         => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, AND _ _ _)          => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, OR _ _ _)           => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, XOR _ _ _)          => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, BYTE _ _ _)         => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MLOADN _ _ _ _)     => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MSTOREN _ _ _ _)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MLOAD _ _)          => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MSTORE _ _)         => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, LOADPOS(_, _) _)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, LOADNEG(_, _) _)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MOVE _ _)           => Gverylow < SCHED > ... </k>

    // Wlow
    rule <k> #gasExec(SCHED, MUL _ _ _)        => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, DIV _ _ _)        => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MOD _ _ _)        => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, SIGNEXTEND _ _ _) => Glow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, TWOS _ _ _)       => Glow < SCHED > ... </k>

    // Wmid
    rule <k> #gasExec(SCHED, ADDMOD _ _ _ _) => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MULMOD _ _ _ _) => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, EXPMOD _ _ _ _) => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, JUMP(_))        => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, LOCALCALL(_,_,_) _ _) => Gmid < SCHED > ... </k>
    rule <k> #gasExec(SCHED, RETURN(_) _)    => Gmid < SCHED > ... </k>

    // Whigh
    rule <k> #gasExec(SCHED, JUMPI(_) _)        => Ghigh < SCHED > ... </k>

    rule <k> #gasExec(SCHED, EXTCODESIZE _ _) => Gextcodesize < SCHED > ... </k>
    rule <k> #gasExec(SCHED, BALANCE _ _)     => Gbalance     < SCHED > ... </k>
    rule <k> #gasExec(SCHED, BLOCKHASH _ _)   => Gblockhash   < SCHED > ... </k>

    // Precompiled
    rule <k> #gasExec(_, ECREC)  => 3000 ... </k>
    rule <k> #gasExec(_, SHA256) =>  60 +Int  12 *Int (LEN up/Int 32) ... </k> <callData> LEN DATA .Regs </callData>
    rule <k> #gasExec(_, RIP160) => 600 +Int 120 *Int (LEN up/Int 32) ... </k> <callData> LEN DATA .Regs </callData>
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

    syntax Int ::= G0 ( Schedule , WordStack , Bool ) [function]
 // ------------------------------------------------------------
    rule G0(SCHED, .WordStack, true)  => Gtxcreate    < SCHED >
    rule G0(SCHED, .WordStack, false) => Gtransaction < SCHED >

    rule G0(SCHED, 0 : REST, ISCREATE) => Gtxdatazero    < SCHED > +Int G0(SCHED, REST, ISCREATE)
    rule G0(SCHED, N : REST, ISCREATE) => Gtxdatanonzero < SCHED > +Int G0(SCHED, REST, ISCREATE) requires N =/=Int 0

    syntax Int ::= "G*" "(" Int "," Int "," Int ")" [function]
 // ----------------------------------------------------------
    rule G*(GAVAIL, GLIMIT, REFUND) => GAVAIL +Int minInt((GLIMIT -Int GAVAIL)/Int 2, REFUND)
```

Fee Schedule from C++ Implementation
------------------------------------

The [C++ Implementation of EVM](https://github.com/ethereum/cpp-ethereum) specifies several different "profiles" for how the VM works.
Here we provide each protocol from the C++ implementation, as the yellowpaper does not contain all the different profiles.
Specify which profile by passing in the argument `-cSCHEDULE=<FEE_SCHEDULE>` when calling `krun` (the available `<FEE_SCHEDULE>` are supplied here).

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

### Defualt Schedule

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

```c++
struct EVMSchedule
{
    EVMSchedule(): tierStepGas(std::array<unsigned, 8>{{0, 2, 3, 5, 8, 10, 20, 0}}) {}
    EVMSchedule(bool _efcd, bool _hdc, unsigned const& _txCreateGas): exceptionalFailedCodeDeposit(_efcd), haveDelegateCall(_hdc), tierStepGas(std::array<unsigned, 8>{{0, 2, 3, 5, 8, 10, 20, 0}}), txCreateGas(_txCreateGas) {}
    bool exceptionalFailedCodeDeposit = true;
    bool haveDelegateCall = true;
    bool eip150Mode = false;
    bool eip158Mode = false;
    bool haveRevert = false;
    bool haveReturnData = false;
    bool haveStaticCall = false;
    bool haveCreate2 = false;
    std::array<unsigned, 8> tierStepGas;

    unsigned expGas = 10;
    unsigned expByteGas = 10;
    unsigned sha3Gas = 30;
    unsigned sha3WordGas = 6;

    unsigned sloadGas = 50;
    unsigned sstoreSetGas = 20000;
    unsigned sstoreResetGas = 5000;
    unsigned sstoreRefundGas = 15000;

    unsigned logGas = 375;
    unsigned logDataGas = 8;
    unsigned logTopicGas = 375;

    unsigned callGas = 40;
    unsigned callStipend = 2300;
    unsigned callValueTransferGas = 9000;
    unsigned callNewAccountGas = 25000;

    unsigned createGas = 32000;
    unsigned createDataGas = 200;
    unsigned suicideGas = 0;
    unsigned suicideRefundGas = 24000;

    unsigned memoryGas = 3;
    unsigned quadCoeffDiv = 512;
    unsigned copyGas = 3;

    unsigned txGas = 21000;
    unsigned txCreateGas = 53000;
    unsigned txDataZeroGas = 4;
    unsigned txDataNonZeroGas = 68;

    unsigned jumpdestGas = 1;
    unsigned balanceGas = 20;
    unsigned blockhashGas = 20;
    unsigned extcodesizeGas = 20;
    unsigned extcodecopyGas = 20;

    unsigned maxCodeSize = unsigned(-1);

    bool staticCallDepthLimit() const { return !eip150Mode; }
    bool suicideNewAccountGas() const { return !eip150Mode; }
    bool suicideChargesNewAccountGas() const { return eip150Mode; }
    bool emptinessIsNonexistence() const { return eip158Mode; }
    bool zeroValueTransferChargesNewAccountGas() const { return !eip158Mode; }
};
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

```c++
static const EVMSchedule HomesteadSchedule = EVMSchedule(true, true, 53000);

static const EVMSchedule EIP150Schedule = []
{
    EVMSchedule schedule = HomesteadSchedule;
    schedule.eip150Mode = true;
    schedule.extcodesizeGas = 700;
    schedule.extcodecopyGas = 700;
    schedule.balanceGas = 400;
    schedule.sloadGas = 200;
    schedule.callGas = 700;
    schedule.suicideGas = 5000;
    return schedule;
}();

static const EVMSchedule EIP158Schedule = []
{
    EVMSchedule schedule = EIP150Schedule;
    schedule.expByteGas = 50;
    schedule.eip158Mode = true;
    schedule.maxCodeSize = 0x6000;
    return schedule;
}();

static const EVMSchedule ByzantiumSchedule = []
{
    EVMSchedule schedule = EIP158Schedule;
    schedule.haveRevert = true;
    schedule.haveReturnData = true;
    schedule.haveStaticCall = true;
    schedule.blockRewardOverwrite = {3 * ether};
    return schedule;
}();
```

IELE Program Representations
============================

```{.k .uiuck .rvk}
    syntax ProgramCell ::= #loadCode ( Ops , Int ) [function]
                         | #loadCode ( Int , Int , Map , Int , Ops ) [function, klabel(#loadCodeAux)]
 // -------------------------------------------------------------------------------------------------
    rule #loadCode(REGISTERS(N) ; OPS, SIZE) => #loadCode(N, 1, .Map, SIZE, OPS)
    rule #loadCode(OPS, SIZE) => #loadCode(5, 1, .Map, SIZE, OPS) [owise]

    rule #loadCode(NREGS, I, CONSTANTS, SIZE, O:ConstantOp ; OPS) => #loadCode(NREGS, I +Int 1, CONSTANTS I |-> O, SIZE, OPS)
    rule #loadCode(NREGS, _, CONSTANTS, SIZE, OPS)
      => #loadFunctions(OPS, CONSTANTS, 
         <program>
           <functions> .Bag </functions>
           <funcIds> .Set </funcIds>
           <constants> CONSTANTS </constants>
           <nregs> NREGS </nregs>
           <programSize> SIZE </programSize>
           <exported> .Map </exported>
         </program>) [owise]

    syntax ProgramCell ::= #loadFunctions ( Ops , Map , ProgramCell ) [function]
                         | #loadFunction  ( Ops , Map , ProgramCell , Int , FunctionCell ) [function]
 // ----------------------------------------------------------------------------------------------------
    rule #loadFunctions(CALLDEST(LABEL, ARGS) ; OPS, CONSTANTS, <program> PROG </program>)
      => #loadFunction(OPS, CONSTANTS, <program> PROG </program>, LABEL, <function> <funcId> LABEL </funcId> <nparams> ARGS </nparams> ... </function>)
    rule #loadFunctions(EXTCALLDEST(LABEL, ARGS) ; OPS, CONSTANTS LABEL |-> FUNCTION(FUNC), <program> PROG <exported> EXPORTED </exported> </program>)
      => #loadFunction(OPS, CONSTANTS, <program> PROG <exported> EXPORTED FUNC |-> LABEL </exported> </program>, LABEL, <function> <funcId> LABEL </funcId> <nparams> ARGS </nparams> ... </function>)
    rule #loadFunctions(.Ops, _, <program> PROG </program>) => <program> PROG </program>

    rule #loadFunction(OP ; OPS => OPS, CONSTANTS, _, _, <function> FUNC <instructions> REST => OP ; REST </instructions> </function>)
      requires notBool isHeaderOp(OP)
    rule #loadFunction(OP:HeaderOp ; OPS, CONSTANTS, <program> PROG <functions> FUNCS </functions> <funcIds> NAMES </funcIds> </program>, NAME, <function> FUNC <instructions> INSTRUCTIONS </instructions> <jumpTable> _ </jumpTable> </function>)
      => #loadFunctions(OP ; OPS, CONSTANTS, <program> PROG <funcIds> NAMES SetItem(NAME) </funcIds> <functions> FUNCS <function> FUNC <instructions> #revOps(INSTRUCTIONS, .Ops) </instructions> <jumpTable> #computeJumpTable(#revOps(INSTRUCTIONS, .Ops)) </jumpTable> </function> </functions> </program>)
    rule #loadFunction(.Ops, CONSTANTS, <program> PROG <functions> FUNCS </functions> <funcIds> NAMES </funcIds> </program>, NAME, <function> FUNC <instructions> INSTRUCTIONS </instructions> <jumpTable> _ </jumpTable> </function>)
      => #loadFunctions(.Ops, CONSTANTS, <program> PROG <funcIds> NAMES SetItem(NAME) </funcIds> <functions> FUNCS <function> FUNC <instructions> #revOps(INSTRUCTIONS, .Ops) </instructions> <jumpTable> #computeJumpTable(#revOps(INSTRUCTIONS, .Ops)) </jumpTable> </function> </functions> </program>)
    
    syntax Map ::= #computeJumpTable ( Ops )             [function]
                 | #computeJumpTable ( Ops , Map , Set ) [function, klabel(#computeJumpTableAux)]
 // ---------------------------------------------------------------------------------------------
    rule #computeJumpTable(OPS) => #computeJumpTable(OPS, .Map, .Set)

    rule #computeJumpTable(.Ops, JUMPS, _) => JUMPS

    rule #computeJumpTable(JUMPDEST(LABEL); OPS, JUMPS, LABELS) => #computeJumpTable(OPS, JUMPS [ LABEL <- JUMPDEST(LABEL); OPS  ], LABELS SetItem(LABEL)) requires notBool LABEL in LABELS
    rule #computeJumpTable(JUMPDEST(LABEL); OPS, JUMPS, LABELS) => #computeJumpTable(OPS, JUMPS [ LABEL <- undef ],                 LABELS)                requires         LABEL in LABELS
    rule #computeJumpTable(_ ; OPS, JUMPS, LABELS) => #computeJumpTable(OPS, JUMPS, LABELS) [owise]
endmodule
```
