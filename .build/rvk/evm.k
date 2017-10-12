// EVM Execution
// =============

// The EVM is a stack machine over some simple opcodes. Most of the opcodes are
// "local" to the execution state of the machine, but some of them must interact
// with the world state. This file only defines the local execution operations, the
// file `ethereum.md` will define the interactions with the world state.


requires "data.k"

module EVM
    imports STRING
    imports EVM-DATA


// Configuration
// -------------

// The configuration has cells for the current account id, the current opcode, the
// program counter, the current gas, the gas price, the current program, the word
// stack, and the local memory. In addition, there are cells for the callstack and
// execution substate.

// We've broken up the configuration into two components; those parts of the state
// that mutate during execution of a single transaction and those that are static
// throughout. In the comments next to each cell, we've marked which component of
// the yellowpaper state corresponds to each cell.


    configuration <k> $PGM:EthereumSimulation </k>
                  <exit-code exit=""> 1 </exit-code>
                  <mode> $MODE:Mode </mode>
                  <schedule> $SCHEDULE:Schedule </schedule>
                  <analysis> .Map </analysis>

                  <ethereum>

                    // EVM Specific
                    // ============

                    <evm>

                      // Mutable during a single transaction
                      // -----------------------------------

                      <output>        .WordStack </output>                   // H_RETURN
                      <callStack>     .List      </callStack>
                      <interimStates> .List      </interimStates>
                      <substateStack> .List      </substateStack>
                      <callLog>       .Set       </callLog>

                      <callFrame>
                        <program>      .Map       </program>                 // I_b
                        <programBytes> .WordStack </programBytes>
                        <callDepth>    0          </callDepth>
                        <jumpTable>    .Map       </jumpTable>

                        // I_*
                        <id>        0          </id>                         // I_a
                        <caller>    0          </caller>                     // I_s
                        <callData>  .WordStack </callData>                   // I_d
                        <callValue> 0          </callValue>                  // I_v

                        // \mu_*
                        <regs>        .Array </regs>                         // \mu_s
                        <nregs>       5      </nregs>
                        <localMem>    .Array </localMem>                     // \mu_m
                        <memoryUsed>  0      </memoryUsed>                   // \mu_i
                        <pc>          0      </pc>                           // \mu_pc
                        <gas>         0      </gas>                          // \mu_g
                        <previousGas> 0      </previousGas>

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

                    </evm>

                    // Ethereum Network
                    // ================

                    <network>

                      // Accounts Record
                      // ---------------

                      <activeAccounts> .Map </activeAccounts>
                      <accounts>


// -   UIUC-K and RV-K have slight differences of opinion here.


                        <account multiplicity="*" type="Map">



                          <acctID>  0          </acctID>
                          <balance> 0          </balance>
                          <code>    .WordStack </code>
                          <storage> .Map       </storage>
                          <nonce>   0          </nonce>
                        </account>
                      </accounts>

                      // Transactions Record
                      // -------------------

                      <txOrder>   .List </txOrder>
                      <txPending> .List </txPending>

                      <messages>


// -   UIUC-K and RV-K have slight differences of opinion here.


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

                  </ethereum>

    syntax EthereumSimulation
 // -------------------------


// Modal Semantics
// ---------------

// Our semantics is modal, with the initial mode being set on the command line via
// `-cMODE=EXECMODE`.

// -   `NORMAL` executes as a client on the network would.
// -   `VMTESTS` skips `CALL*` and `CREATE` operations.


    syntax Mode ::= "NORMAL" | "VMTESTS"


// -   `#setMode_` sets the mode to the supplied one.


    syntax Mode ::= "#setMode" Mode
 // -------------------------------
    rule <k> #setMode EXECMODE => . ... </k> <mode> _ => EXECMODE </mode>
    rule <k> EX:Exception ~> (#setMode _ => .) ... </k>


// Hardware
// --------

// The `callStack` cell stores a list of previous VM execution states.

// -   `#pushCallStack` saves a copy of VM execution state on the `callStack`.
// -   `#popCallStack` restores the top element of the `callStack`.
// -   `#dropCallStack` removes the top element of the `callStack`.


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


// The `interimStates` cell stores a list of previous world states.

// -   `#pushWorldState` stores a copy of the current accounts at the top of the
//     `interimStates` cell.
// -   `#popWorldState` restores the top element of the `interimStates`.
// -   `#dropWorldState` removes the top element of the `interimStates`.


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


// The `substateStack` cell stores a list of previous substate logs.

// -   `#pushSubstate` stores a copy of the current substate at the top of the
//     `substateStack` cell.
// -   `#popSubstate` restores the top element of the `substateStack`.
// -   `#dropSubstate` removes the top element of the `substateStack`.


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


// Simple commands controlling exceptions provide control-flow.

// -   `#end` is used to indicate the (non-exceptional) end of execution.
// -   `#exception` is used to indicate exceptional states (it consumes any
//     operations to be performed after it).
// -   `#?_:_?#` allows for branching control-flow; if it reaches the front of the
//     `op` cell it takes the first branch, if an exception runs into it it takes
//     the second branch.


    syntax KItem ::= Exception
    syntax Exception ::= "#exception" | "#end" | "#revert"
 // ------------------------------------------
    rule <k> EX:Exception ~> (_:Int    => .) ... </k>
    rule <k> EX:Exception ~> (_:Op => .) ... </k>

    syntax KItem ::= "#?" K ":" K "?#"
 // ----------------------------------
    rule <k>                #? K : _ ?#  => K               ... </k>
    rule <k> #exception ~>  #? _ : K ?#  => K ~> #exception ... </k>
    rule <k> #revert    ~>  #? K : _ ?#  => K ~> #revert    ... </k>
    rule <k> #end       ~> (#? K : _ ?#) => K ~> #end       ... </k>

    syntax Reg ::= "%" Int | Int
    syntax KResult ::= Int
 // ----------------------------
    rule <k> % REG => REGS [ REG ] ... </k> <regs> REGS </regs>


// OpCode Execution Cycle
// ----------------------

// `OpCode` is broken into several subsorts for easier handling. Here all `OpCode`s
// are subsorted into `KItem` (allowing sequentialization), and the various sorts
// of opcodes are subsorted into `OpCode`.


    syntax KItem  ::= OpCode
    syntax Op ::= InternalOp
    syntax OpCode ::= NullOp | NullVoidOp | UnOp | UnVoidOp | BinOp | BinVoidOp | TernOp
                    | TernVoidOp | QuadVoidOp | FiveVoidOp | SixVoidOp | CallOp | CallSixOp
 // ---------------------------------------------------------------------------------------


// -   `#execute` calls `#next` repeatedly until it recieves an `#end` or
//     `#exception`.
// -   `#execTo` executes until the next opcode is one of the specified ones.


    syntax KItem ::= "#execute"
 // ---------------------------
    rule <k> (. => #next) ~> #execute ... </k>
    rule <k> EX:Exception ~> (#execute => .)  ... </k>

    syntax InternalOp ::= "#execTo" K
 // -----------------------------------
    rule <k> (. => #next) ~> #execTo #klabel(LBL) ... </k>
         <pc> PCOUNT </pc>
         <program> ... PCOUNT |-> OP ... </program>
      requires notBool LBL(OP)

    rule <k> #execTo #klabel(LBL) => . ... </k>
         <pc> PCOUNT </pc>
         <program> ... PCOUNT |-> OP ... </program>
      requires LBL(OP)

    rule <k> #execTo #klabel(LBL) => #end ... </k>
         <pc> PCOUNT </pc>
         <program> PGM </program>
      requires notBool PCOUNT in keys(PGM)


// Execution follows a simple cycle where first the state is checked for
// exceptions, then if no exceptions will be thrown the opcode is run.

// -   Regardless of the mode, `#next` will throw `#end` or `#exception` if the
//     current program counter is not pointing at an OpCode.

// TODO: I think on `#next` we are supposed to pretend it's `STOP` if it's in the
// middle of the program somewhere but is invalid? I suppose the semantics
// currently loads `INVALID` where `N` is the position in the bytecode array.


    syntax InternalOp ::= "#next"
 // -----------------------------
    rule <k> #next => #end ... </k>
         <pc> PCOUNT </pc>
         <program> PGM </program>
         <output> _ => .WordStack </output>
      requires notBool (PCOUNT in_keys(PGM))


// -   In `NORMAL` or `VMTESTS` mode, `#next` checks if the opcode is exceptional,
//     runs it if not, then increments the program counter.


    rule <mode> EXECMODE </mode>
         <k> #next
          => #pushCallStack
          ~> #exceptional? [ OP ] ~> #exec [ OP ] ~> #pc [ #code(OP) ]
          ~> #? #dropCallStack : #popCallStack ?#
         ...
         </k>
         <pc> PCOUNT </pc>
         <program> ... PCOUNT |-> OP ... </program>
      requires EXECMODE in (SetItem(NORMAL) SetItem(VMTESTS))


// ### Exceptional Ops

// Some checks if an opcode will throw an exception are relatively quick and done
// up front.

// -   `#exceptional?` checks if the operator is invalid (this implements the
//     function `Z` in the yellowpaper, section 9.4.2).


    syntax InternalOp ::= "#exceptional?" "[" Op "]"
 // ----------------------------------------------------
    rule <k> #exceptional? [ OP ] => #invalid? [ #code(OP) ] ~> #badJumpDest? [ OP ] ~> #static? [ OP ] ... </k>


// -   `#invalid?` checks if it's the designated invalid opcode.


    syntax InternalOp ::= "#invalid?" "[" OpCode "]"
 // ------------------------------------------------
    rule <k> #invalid? [ INVALID ] => #exception ... </k>
    rule <k> #invalid? [ OP      ] => .          ... </k> requires notBool isInvalidOp(OP)


// -   `#badJumpDest?` determines if the opcode will result in a bad
//     jump destination.


    syntax InternalOp ::= "#badJumpDest?" "[" Op "]"
 // ----------------------------------------------------
    rule <k> #badJumpDest? [ OP               ] => . ... </k> requires notBool isJumpOp(#code(OP))
    rule <k> #badJumpDest? [ JUMP (LABEL)     ] => . ... </k> <jumpTable> JUMPS </jumpTable> requires LABEL in_keys(JUMPS)
    rule <k> #badJumpDest? [ JUMPI(LABEL) _   ] => . ... </k> <jumpTable> JUMPS </jumpTable> requires LABEL in_keys(JUMPS)
    rule <k> #badJumpDest? [ JUMPI(_)     % R ] => . ... </k> <regs> REGS </regs> requires REGS [ R ] ==K 0

    rule <k> #badJumpDest? [ JUMP (LABEL)     ] => #exception ... </k> <jumpTable> JUMPS </jumpTable> requires notBool LABEL in_keys(JUMPS)
    rule <k> #badJumpDest? [ JUMPI(LABEL) % R ] => #exception ... </k> <jumpTable> JUMPS </jumpTable> <regs> REGS </regs> requires notBool LABEL in_keys(JUMPS) andBool REGS [ R ] =/=K 0


// -   `#static?` determines if the opcode should throw an exception due to the
//     static flag.


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
    rule #changesState(_, CALL _ _ _ VALUE _ _ _ _, _) => true requires VALUE =/=Int 0
    rule #changesState(CREATE, _, _) => true
    rule #changesState(SELFDESTRUCT, _, _) => true
    rule #changesState(...) => false [owise]


// ### Execution Step

// -   `#exec` will load the arguments of the opcode and trigger the
//     subsequent operations.


    syntax InternalOp ::= "#exec" "[" Op "]"
 // ----------------------------------------
    rule <k> #exec [ OP:LoadedOp ] => #gas [ #addr?(OP) ] ~> #addr?(OP) ... </k>


// Here we load the correct number of arguments from the `regs` based on the sort
// of the opcode. Some of them require an argument to be interpereted as an address
// (modulo 160 bits), so the `#addr?` function performs that check.


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
                | SixVoidOp Reg Reg Reg Reg Reg Reg      [klabel(sixVoidOp)]
                | CallSixOp Reg Reg Reg Reg Reg Reg Reg  [klabel(callSixOp)]
                | CallOp Reg Reg Reg Reg Reg Reg Reg Reg [klabel(callOp)]

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

    context #exec [ _::SixVoidOp HOLE:Reg _ _ _ _ _ ]
    context #exec [ _::SixVoidOp _ HOLE:Reg _ _ _ _ ]
    context #exec [ _::SixVoidOp _ _ HOLE:Reg _ _ _ ]
    context #exec [ _::SixVoidOp _ _ _ HOLE:Reg _ _ ]
    context #exec [ _::SixVoidOp _ _ _ _ HOLE:Reg _ ]
    context #exec [ _::SixVoidOp _ _ _ _ _ HOLE:Reg ]

    context #exec [ _::CallSixOp _ HOLE:Reg _ _ _ _ _ ]
    context #exec [ _::CallSixOp _ _ HOLE:Reg _ _ _ _ ]
    context #exec [ _::CallSixOp _ _ _ HOLE:Reg _ _ _ ]
    context #exec [ _::CallSixOp _ _ _ _ HOLE:Reg _ _ ]
    context #exec [ _::CallSixOp _ _ _ _ _ HOLE:Reg _ ]
    context #exec [ _::CallSixOp _ _ _ _ _ _ HOLE:Reg ]

    context #exec [ _::CallOp _ HOLE:Reg _ _ _ _ _ _ ]
    context #exec [ _::CallOp _ _ HOLE:Reg _ _ _ _ _ ]
    context #exec [ _::CallOp _ _ _ HOLE:Reg _ _ _ _ ]
    context #exec [ _::CallOp _ _ _ _ HOLE:Reg _ _ _ ]
    context #exec [ _::CallOp _ _ _ _ _ HOLE:Reg _ _ ]
    context #exec [ _::CallOp _ _ _ _ _ _ HOLE:Reg _ ]
    context #exec [ _::CallOp _ _ _ _ _ _ _ HOLE:Reg ]

    syntax Op ::= LoadedOp

    syntax LoadedOp ::= UnOp Reg Int                           [klabel(unOp)]
                      | UnVoidOp Int                           [klabel(unVoidOp)]
                      | BinOp Reg Int Int                      [klabel(binOp)]
                      | BinVoidOp Int Int                      [klabel(binVoidOp)]
                      | TernOp Reg Int Int Int                 [klabel(ternOp)]
                      | TernVoidOp Int Int Int                 [klabel(ternVoidOp)]
                      | QuadVoidOp Int Int Int Int             [klabel(quadVoidOp)]
                      | FiveVoidOp Int Int Int Int Int         [klabel(fiveVoidOp)]
                      | SixVoidOp Int Int Int Int Int Int      [klabel(sixVoidOp)]
                      | CallSixOp Reg Int Int Int Int Int Int  [klabel(callSixOp)]
                      | CallOp Reg Int Int Int Int Int Int Int [klabel(callOp)]

    syntax Op ::= "#addr?" "(" Op ")" [function]
 // -------------------------------------------------
    rule #addr?(BALANCE REG W)                       => BALANCE REG #addr(W)
    rule #addr?(EXTCODESIZE REG W)                   => EXTCODESIZE REG #addr(W)
    rule #addr?(EXTCODECOPY W0 W1 W2 W3)             => EXTCODECOPY #addr(W0) W1 W2 W3
    rule #addr?(SELFDESTRUCT W)                      => SELFDESTRUCT #addr(W)
    rule #addr?(CSO:CallSixOp REG W0 W1 W2 W3 W4 W5) => CSO REG W0 #addr(W1) W2 W3 W4 W5
    rule #addr?(CO:CallOp REG W0 W1 W2 W3 W4 W5 W6)  => CO  REG W0 #addr(W1) W2 W3 W4 W5 W6
    rule #addr?(OP)                                  => OP [owise]

    syntax OpCode ::= #code ( Op ) [function]
    rule #code(OP::NullOp _)                => OP
    rule #code(OP:NullVoidOp)               => OP
    rule #code(OP::UnOp _ _)                => OP
    rule #code(OP::UnVoidOp _)              => OP
    rule #code(OP::BinOp _ _ _)             => OP
    rule #code(OP::BinVoidOp _ _)           => OP
    rule #code(OP::TernOp _ _ _ _)          => OP
    rule #code(OP::TernVoidOp _ _ _)        => OP
    rule #code(OP::QuadVoidOp _ _ _ _)      => OP
    rule #code(OP::FiveVoidOp _ _ _ _ _)    => OP
    rule #code(OP::SixVoidOp _ _ _ _ _ _)   => OP
    rule #code(OP::CallSixOp _ _ _ _ _ _ _) => OP
    rule #code(OP::CallOp _ _ _ _ _ _ _ _)  => OP


// -   `#gas` calculates how much gas this operation costs, and takes into account
//     the memory consumed.


    syntax InternalOp ::= "#gas" "[" Op "]" | "#deductGas" | "#deductMemory"
 // ----------------------------------------------------------------------------
    rule <k> #gas [ OP ] => #memory(OP, MU) ~> #deductMemory ~> #gasExec(SCHED, OP) ~> #deductGas ... </k> <memoryUsed> MU </memoryUsed> <schedule> SCHED </schedule>

    rule <k> MU':Int ~> #deductMemory => #exception ... </k> requires MU' >=Int pow256
    rule <k> MU':Int ~> #deductMemory => (Cmem(SCHED, MU') -Int Cmem(SCHED, MU)) ~> #deductGas ... </k>
         <memoryUsed> MU => MU' </memoryUsed> <schedule> SCHED </schedule>
      requires MU' <Int pow256

    rule <k> G:Int ~> #deductGas => #exception ... </k> <gas> GAVAIL                  </gas> requires GAVAIL <Int G
    rule <k> G:Int ~> #deductGas => .          ... </k> <gas> GAVAIL => GAVAIL -Int G </gas> <previousGas> _ => GAVAIL </previousGas> requires GAVAIL >=Int G

    syntax Int ::= Cmem ( Schedule , Int ) [function, memo]
 // -------------------------------------------------------
    rule Cmem(SCHED, N) => (N *Int Gmemory < SCHED >) +Int ((N *Int N) /Int Gquadcoeff < SCHED >)


// ### Program Counter

// -   `#pc` calculates the next program counter of the given operator.


    syntax InternalOp ::= "#pc" "[" OpCode "]"
 // ------------------------------------------
    rule <k> #pc [ OP ] => . ... </k> requires isJumpOp(OP)
    rule <k> #pc [ OP ] => . ... </k> <pc> PCOUNT => PCOUNT +Int #opWidth(OP, NREGS) </pc> <nregs> NREGS </nregs> requires notBool isJumpOp(OP)

    syntax Bool ::= isJumpOp ( OpCode ) [function]
 // ----------------------------------------------
    rule isJumpOp(JUMP(_)) => true
    rule isJumpOp(JUMPI(_)) => true
    rule isJumpOp(...) => false [owise]
    syntax Bool ::= isJumpAddrOp ( OpCode ) [function]
    rule isJumpAddrOp(JUMPDEST(_)) => true
    rule isJumpAddrOp(OP) => true requires isJumpOp(OP)
    rule isJumpAddrOp(...) => false [owise]


// ### Substate Log

// After executing a transaction, it's necessary to have the effect of the substate
// log recorded.

// -   `#finalizeTx` makes the substate log actually have an effect on the state.


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


// EVM Programs
// ============

// Lists of opcodes form programs. Deciding if an opcode is in a list will be
// useful for modeling gas, and converting a program into a map of program-counter
// to opcode is useful for execution.

// Note that `_in_` ignores the arguments to operators that are parametric.


    syntax Ops ::= ".Ops" | Op ";" Ops
 // --------------------------------------------------

    syntax Map ::= #asMapOps ( Ops )                   [function]
                 | #asMapOps ( Int , Ops , Map , Int ) [function, klabel(#asMapOpsAux)]
 // -----------------------------------------------------------------------------------------
    rule #asMapOps( REGISTERS(N) ; OPS::Ops ) => #asMapOps(2, OPS, 0 |-> REGISTERS(N), N)
    rule #asMapOps( OPS::Ops ) => #asMapOps(0, OPS, .Map, 32) [owise]

    rule #asMapOps( _ , .Ops        , MAP, _ )     => MAP
    rule #asMapOps( N , OP:Op ; OCS , MAP, NREGS ) => #asMapOps(N +Int #opWidth(#code(OP), NREGS), OCS, MAP (N |-> OP), NREGS)


// EVM Ops
// -------

// Each subsection has a different class of opcodes. Organization is based roughly
// on what parts of the execution state are needed to compute the result of each
// operator. This sometimes corresponds to the organization in the yellowpaper.

// ### Internal Operations

// -   `#newAccount_` allows declaring a new empty account with the given address
//     (and assumes the rounding to 160 bits has already occured). If the account
//     already exists with non-zero nonce or non-empty code, an exception is
//     thrown. Otherwise, if the account already exists, the storage is cleared.


    syntax InternalOp ::= "#newAccount" Int
 // ---------------------------------------
    rule <k> #newAccount ACCT => #exception ... </k>
         <account>
           <acctID> ACCT  </acctID>
           <code>   CODE  </code>
           <nonce>  NONCE </nonce>
           ...
         </account>
      requires CODE =/=K .WordStack orBool NONCE =/=K 0

    rule <k> #newAccount ACCT => . ... </k>
         <account>
           <acctID>  ACCT       </acctID>
           <code>    .WordStack </code>
           <nonce>   0          </nonce>
           <storage> _ => .Map  </storage>
           ...
         </account>

    rule <k> #newAccount ACCT => . ... </k>
         <activeAccounts> ACCTS (.Map => ACCT |-> true) </activeAccounts>
         <accounts>
           ( .Bag
          => <account>
               <acctID>  ACCT       </acctID>
               <balance> 0          </balance>
               <code>    .WordStack </code>
               <storage> .Map       </storage>
               <nonce>   0          </nonce>
             </account>
           )
           ...
         </accounts>
      requires notBool ACCT in_keys(ACCTS)


// -   `#transferFunds` moves money from one account into another, creating the
//     destination account if it doesn't exist.


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
         <activeAccounts> ... ACCTTO |-> (EMPTY => #if VALUE >Int 0 #then false #else EMPTY #fi) ACCTFROM |-> (_ => ORIGFROM ==Int VALUE andBool NONCE ==Int 0 andBool CODE ==K .WordStack) ... </activeAccounts>
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
      requires ACCTFROM =/=K ACCTTO andBool notBool ACCTTO in_keys(ACCTS)

    rule <k> #transferFunds ACCT ACCT VALUE => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <balance> ORIGFROM </balance>
           ...
         </account>
      requires VALUE <=Int ORIGFROM


// ### Invalid Operator

// We use `INVALID` both for marking the designated invalid operator and for
// garbage bytes in the input program.


    syntax InvalidOp ::= "INVALID"
    syntax NullVoidOp ::= InvalidOp
 // -------------------------------


// ### Program Header


    syntax NullVoidOp ::= REGISTERS(Int)
 // ------------------------------------
    rule <k> REGISTERS(NREGS) => .          ... </k> <pc>      0 </pc> <nregs> _ => NREGS </nregs>
    rule <k> REGISTERS(NREGS) => #exception ... </k> <pc> PCOUNT </pc> requires PCOUNT =/=K 0


// ### Stack Manipulations

// Some operators don't calculate anything, they just push the stack around a bit.


    syntax NullOp ::= LOADPOS ( Int , Int )
 // ---------------------------------------
    rule <k> LOADPOS(_, W) REG => #load REG W ... </k> 

    syntax NullOp ::= LOADNEG ( Int , Int )
 // ---------------------------------------
    rule <k> LOADNEG(_, W) REG => #load REG (0 -Int W) ... </k> 

    syntax InternalOp ::= "#load" Reg Int
 // -------------------------------------
    rule <k> #load % REG VALUE => . ... </k>
         <regs> REGS => REGS [ REG <- VALUE ] </regs>


// ### Local Memory

// These operations are getters/setters of the local execution memory.


    syntax UnOp ::= "MLOAD8" | "MLOAD256" | "MLOAD"
 // -----------------------------------------------
    rule <k> MLOAD256 REG INDEX => #load REG #asSigned(#range(LM, INDEX, 32)) ... </k>
         <localMem> LM </localMem>

    rule <k> MLOAD8 REG INDEX => #load REG #asSigned(#range(LM, INDEX, 1)) ... </k>
         <localMem> LM </localMem>

    syntax BinVoidOp ::= "MSTORE8" | "MSTORE256" | "MSTORE"
 // -------------------------------------------------------
    rule <k> MSTORE256 INDEX VALUE => . ... </k>
         <localMem> LM => LM [ INDEX := #padToWidth(32, #asUnsignedBytes(chop(VALUE))) ] </localMem>

    rule <k> MSTORE8 INDEX VALUE => . ... </k>
         <localMem> LM => LM [ INDEX <- (VALUE modInt 256) ]    </localMem>


// ### Expressions

// Expression calculations are simple and don't require anything but the arguments
// from the `regs` to operate.

// NOTE: We have to call the opcode `OR` by `EVMOR` instead, because K has trouble
// parsing it/compiling the definition otherwise.


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
    rule <k> BYTE REG INDEX W     => #load REG byte(chop(INDEX), W)     ... </k>
    rule <k> SIGNEXTEND REG W0 W1 => #load REG signextend(chop(W0), W1) ... </k>
    rule <k> TWOS REG W0 W1       => #load REG twos(chop(W0), W1)       ... </k>

    syntax BinOp ::= "AND" | "EVMOR" | "XOR"
 // ----------------------------------------
    rule <k> AND   REG W0 W1 => #load REG W0 &Int W1   ... </k>
    rule <k> EVMOR REG W0 W1 => #load REG W0 |Int W1   ... </k>
    rule <k> XOR   REG W0 W1 => #load REG W0 xorInt W1 ... </k>

    syntax BinOp ::= "LT" | "GT" | "EQ"
 // -----------------------------------
    rule <k> LT REG W0 W1 => #load REG 1 ... </k>  requires W0 <Int   W1
    rule <k> LT REG W0 W1 => #load REG 0 ... </k>  requires W0 >=Int  W1
    rule <k> GT REG W0 W1 => #load REG 1 ... </k>  requires W0 >Int   W1
    rule <k> GT REG W0 W1 => #load REG 0 ... </k>  requires W0 <=Int  W1
    rule <k> EQ REG W0 W1 => #load REG 1 ... </k>  requires W0 ==Int  W1
    rule <k> EQ REG W0 W1 => #load REG 0 ... </k>  requires W0 =/=Int W1

    syntax BinOp ::= "SHA3"
 // -----------------------
    rule <k> SHA3 REG MEMSTART MEMWIDTH => #load REG keccak(#range(LM, MEMSTART, MEMWIDTH)) ... </k>
         <localMem> LM </localMem>


// ### Local State

// These operators make queries about the current execution state.


    syntax NullOp ::= "PC" | "GAS" | "GASPRICE" | "GASLIMIT"
 // --------------------------------------------------------
    rule <k> PC       REG => #load REG PCOUNT ... </k> <pc> PCOUNT </pc>
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
    rule <k> MSIZE    REG => #load REG 32 *Int MU         ... </k> <memoryUsed> MU </memoryUsed>
    rule <k> CODESIZE REG => #load REG #sizeWordStack(PGM) ... </k> <programBytes> PGM </programBytes>

    syntax TernVoidOp ::= "CODECOPY"
 // --------------------------------
    rule <k> CODECOPY MEMSTART PGMSTART WIDTH => . ... </k>
         <programBytes> PGM </programBytes>
         <localMem> LM => LM [ MEMSTART := PGM [ PGMSTART .. WIDTH ] ] </localMem>

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


// ### `JUMP*`

// The `JUMP*` family of operations affect the current program counter.


    syntax NullVoidOp ::= JUMPDEST ( Int )
 // --------------------------------------
    rule <k> JUMPDEST(_) => . ... </k>

    syntax NullVoidOp ::= JUMP ( Int )
 // ----------------------------------
    rule <k> JUMP(LABEL) => . ... </k> <jumpTable> ... LABEL |-> DEST </jumpTable> <pc> _ => DEST </pc>

    syntax UnVoidOp ::= JUMPI ( Int )
 // ---------------------------------
    rule <k> JUMPI(LABEL) I => . ... </k> <pc> _      => DEST          </pc> <jumpTable> ... LABEL |-> DEST </jumpTable> requires I =/=K 0
    rule <k> JUMPI(LABEL) 0 => . ... </k> <pc> PCOUNT => PCOUNT +Int #opWidth(JUMPI(LABEL), NREGS) </pc> <nregs> NREGS </nregs>


// ### `STOP`, `REVERT`, and `RETURN`


    syntax NullVoidOp ::= "STOP"
 // ----------------------------
    rule <k> STOP => #end ... </k>
         <output> _ => .WordStack </output>

    syntax BinVoidOp ::= "RETURN"
 // -----------------------------
    rule <k> RETURN RETSTART RETWIDTH => #end ... </k>
         <output> _ => #range(LM, RETSTART, RETWIDTH) </output>
         <localMem> LM </localMem>

    syntax BinVoidOp ::= "REVERT"
 // -----------------------------
    rule <k> REVERT RETSTART RETWIDTH => #revert ... </k>
         <output> _ => #range(LM, RETSTART, RETWIDTH) </output>
         <localMem> LM </localMem>


// ### Call Data

// These operators query about the current `CALL*` state.


    syntax NullOp ::= "CALLDATASIZE"
 // --------------------------------
    rule <k> CALLDATASIZE REG => #load REG #sizeWordStack(CD) ... </k>
         <callData> CD </callData>

    syntax BinOp ::= "CALLDATALOAD"
 // -------------------------------
    rule <k> CALLDATALOAD REG DATASTART DATAWIDTH => #load REG #asSigned(CD [ DATASTART .. DATAWIDTH ]) ... </k>
         <callData> CD </callData>

    syntax TernVoidOp ::= "CALLDATACOPY"
 // ------------------------------------
    rule <k> CALLDATACOPY MEMSTART DATASTART DATAWIDTH => . ... </k>
         <localMem> LM => LM [ MEMSTART := CD [ DATASTART .. DATAWIDTH ] ] </localMem>
         <callData> CD </callData>


// ### Return Data

// These operators query about the current return data buffer.


    syntax NullOp ::= "RETURNDATASIZE"
 // ----------------------------------
    rule <k> RETURNDATASIZE REG => #load REG #sizeWordStack(RD) ... </k>
         <output> RD </output>

    syntax TernVoidOp ::= "RETURNDATACOPY"
 // --------------------------------------
    rule <k> RETURNDATACOPY MEMSTART DATASTART DATAWIDTH => . ... </k>
         <localMem> LM => LM [ MEMSTART := RD [ DATASTART .. DATAWIDTH ] ] </localMem>
         <output> RD </output>
      requires chop(DATASTART) +Int chop(DATAWIDTH) <=Int #sizeWordStack(RD)

    rule <k> RETURNDATACOPY MEMSTART DATASTART DATAWIDTH => #exception ... </k>
         <output> RD </output>
      requires chop(DATASTART) +Int chop(DATAWIDTH) >Int #sizeWordStack(RD)


// ### Log Operations

// During execution of a transaction some things are recorded in the substate log
// (Section 6.1 in yellowpaper). This is a right cons-list of `SubstateLogEntry`
// (which contains the account ID along with the specified portions of the
// `wordStack` and `localMem`).


    syntax SubstateLogEntry ::= "{" Int "|" WordStack "|" WordStack "}"
 // -------------------------------------------------------------------



    syntax BinVoidOp  ::= "LOG0"
    syntax TernVoidOp ::= "LOG1"
    syntax QuadVoidOp ::= "LOG2"
    syntax FiveVoidOp ::= "LOG3"
    syntax SixVoidOp  ::= "LOG4"
 // ----------------------------

    rule LOG0 MEMSTART MEMWIDTH => #log MEMSTART MEMWIDTH .WordStack
    rule LOG1 MEMSTART MEMWIDTH W0 => #log MEMSTART MEMWIDTH W0 : .WordStack
    rule LOG2 MEMSTART MEMWIDTH W0 W1 => #log MEMSTART MEMWIDTH W0 : W1 : .WordStack
    rule LOG3 MEMSTART MEMWIDTH W0 W1 W2 => #log MEMSTART MEMWIDTH W0 : W1 : W2 : .WordStack
    rule LOG4 MEMSTART MEMWIDTH W0 W1 W2 W3 => #log MEMSTART MEMWIDTH W0 : W1 : W2 : W3 : .WordStack

    syntax InternalOp ::= "#log" Int Int WordStack
 // ----------------------------------------------
    rule <k> #log MEMSTART MEMWIDTH WS => . ... </k>
         <id> ACCT </id>
         <localMem> LM </localMem>
         <log> ... (.List => ListItem({ ACCT | WS | #range(LM, MEMSTART, MEMWIDTH) })) </log>


// Ethereum Network Ops
// --------------------

// Operators that require access to the rest of the Ethereum network world-state
// can be taken as a first draft of a "blockchain generic" language.

// ### Account Queries

// TODO: It's unclear what to do in the case of an account not existing for these
// operators. `BALANCE` is specified to push 0 in this case, but the others are not
// specified. For now, I assume that they instantiate an empty account and use the
// empty data.


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
    rule <k> EXTCODESIZE REG ACCT => #load REG #sizeWordStack(CODE) ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> CODE </code>
           ...
         </account>

    rule <k> EXTCODESIZE REG ACCT => #newAccount ACCT ~> #load REG 0 ... </k>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool ACCT in_keys(ACCTS)


// TODO: What should happen in the case that the account doesn't exist with
// `EXTCODECOPY`? Should we pad zeros (for the copied "program")?


    syntax QuadVoidOp ::= "EXTCODECOPY"
 // -----------------------------------
    rule <k> EXTCODECOPY ACCT MEMSTART PGMSTART WIDTH => . ... </k>
         <localMem> LM => LM [ MEMSTART := PGM [ PGMSTART .. WIDTH ] ] </localMem>
         <account>
           <acctID> ACCT </acctID>
           <code> PGM </code>
           ...
         </account>

    rule <k> EXTCODECOPY ACCT MEMSTART PGMSTART WIDTH => #newAccount ACCT ... </k>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool ACCT in_keys(ACCTS)


// ### Account Storage Operations

// These operations interact with the account storage.


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
         <refund> R => #ifInt OLD =/=Int 0 andBool VALUE ==Int 0
                        #then R +Int Rsstoreclear < SCHED >
                        #else R
                       #fi
         </refund>
         <schedule> SCHED </schedule>

    rule <k> SSTORE INDEX VALUE => . ... </k>
         <id> ACCT </id>
         <account>
           <acctID> ACCT </acctID>
           <storage> STORAGE => STORAGE [ INDEX <- VALUE ] </storage>
           ...
         </account>
      requires notBool (INDEX in_keys(STORAGE))


// ### Call Operations

// The various `CALL*` (and other inter-contract control flow) operations will be
// desugared into these `InternalOp`s.

// -   The `callLog` is used to store the `CALL*`/`CREATE` operations so that we
//     can compare them against the test-set.


    syntax Call ::= "{" Int "|" Int "|" Int "|" WordStack "}"
 // ---------------------------------------------------------


// -   `#call_____` takes the calling account, the account to execute as, the
//     account whose code should execute, the gas limit, the amount to transfer,
//     and the arguments.
// -   `#callWithCode______` takes the calling account, the accout to execute as,
//     the code to execute (as a map), the gas limit, the amount to transfer, and
//     the arguments.
// -   `#return__` is a placeholder for the calling program, specifying where to
//     place the returned data in memory.


    syntax InternalOp ::= "#checkCall" Int Int
                        | "#call" Int Int Int Int Int Int WordStack Bool
                        | "#callWithCode" Int Int Map WordStack Int Int Int WordStack Bool
                        | "#mkCall" Int Int Map WordStack Int Int Int WordStack Bool
 // --------------------------------------------------------------------------------
    rule <k> #checkCall ACCT VALUE ~> #call _ _ _ GLIMIT _ _ _ _ => #refund GLIMIT ~> #pushCallStack ~> #pushWorldState ~> #pushSubstate ~> #exception ... </k>
         <callDepth> CD </callDepth>
         <output> _ => .WordStack </output>
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

    rule <k> #call ACCTFROM ACCTTO ACCTCODE GLIMIT VALUE APPVALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO (0 |-> #precompiled(ACCTCODE)) .WordStack GLIMIT VALUE APPVALUE ARGS STATIC
         ...
         </k>
         <schedule> SCHED </schedule>
      requires ACCTCODE in #precompiledAccounts(SCHED)

    rule <k> #call ACCTFROM ACCTTO ACCTCODE GLIMIT VALUE APPVALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO #asMapOps(#dasmOps(CODE, SCHED)) CODE GLIMIT VALUE APPVALUE ARGS STATIC
         ...
         </k>
         <schedule> SCHED </schedule>
         <acctID> ACCTCODE </acctID>
         <code> CODE </code>
      requires notBool ACCTCODE in #precompiledAccounts(SCHED)

    rule <k> #call ACCTFROM ACCTTO ACCTCODE GLIMIT VALUE APPVALUE ARGS STATIC
          => #callWithCode ACCTFROM ACCTTO .Map .WordStack GLIMIT VALUE APPVALUE ARGS STATIC
         ...
         </k>
         <activeAccounts> ACCTS </activeAccounts>
         <schedule> SCHED </schedule>
      requires notBool ACCTCODE in #precompiledAccounts(SCHED) andBool notBool ACCTCODE in_keys(ACCTS)

    rule #callWithCode ACCTFROM ACCTTO CODE BYTES GLIMIT VALUE APPVALUE ARGS STATIC
      => #pushCallStack ~> #pushWorldState ~> #pushSubstate
      ~> #transferFunds ACCTFROM ACCTTO VALUE
      ~> #mkCall ACCTFROM ACCTTO CODE BYTES GLIMIT VALUE APPVALUE ARGS STATIC

    rule <mode> EXECMODE </mode>
         <k> #mkCall ACCTFROM ACCTTO CODE BYTES GLIMIT VALUE APPVALUE ARGS STATIC:Bool
          => #initVM ~> #if EXECMODE ==K VMTESTS #then #end #else #execute #fi
         ...
         </k>
         <callLog> ... (.Set => #ifSet EXECMODE ==K VMTESTS #then SetItem({ ACCTTO | GLIMIT | VALUE | ARGS }) #else .Set #fi) </callLog>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => ARGS </callData>
         <callValue> _ => APPVALUE </callValue>
         <id> _ => ACCTTO </id>
         <gas> _ => GLIMIT </gas>
         <caller> _ => ACCTFROM </caller>
         <program> _ => CODE </program>
         <programBytes> _ => BYTES </programBytes>
         <static> OLDSTATIC:Bool => OLDSTATIC orBool STATIC </static>
         <jumpTable> _ => #computeJumpTable(CODE) </jumpTable>

    syntax KItem ::= "#initVM"
 // --------------------------
    rule <k> #initVM    => . ...       </k>
         <pc>         _ => 0           </pc>
         <memoryUsed> _ => 0           </memoryUsed>
         <output>     _ => .WordStack  </output>
         <regs>       _ => .Array      </regs>
         <nregs>      _ => 5           </nregs>
         <localMem>   _ => .Array      </localMem>

    syntax KItem ::= "#return" Int Int Reg
 // --------------------------------------
    rule <k> #exception ~> #return _ _ REG
          => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0
         ...
         </k>
         <output> _ => .WordStack </output>

    rule <k> #revert ~> #return RETSTART RETWIDTH REG
           => #popCallStack
           ~> #popWorldState
           ~> #popSubstate
           ~> #load REG 0 ~> #refund GAVAIL ~> #setLocalMem RETSTART RETWIDTH OUT
          ...
         </k>
         <output> OUT </output>
         <gas> GAVAIL </gas>

    rule <mode> EXECMODE </mode>
         <k> #end ~> #return RETSTART RETWIDTH REG
          => #popCallStack
          ~> #if EXECMODE ==K VMTESTS #then #popWorldState #else #dropWorldState #fi
          ~> #dropSubstate
          ~> #load REG 1 ~> #refund GAVAIL ~> #setLocalMem RETSTART RETWIDTH OUT
         ...
         </k>
         <output> OUT </output>
         <gas> GAVAIL </gas>

    syntax InternalOp ::= "#refund" Int
                        | "#setLocalMem" Int Int WordStack
 // ------------------------------------------------------
    rule <k> #refund G => . ... </k> <gas> GAVAIL => GAVAIL +Int G </gas>

    rule <k> #setLocalMem START WIDTH WS => . ... </k>
         <localMem> LM => LM [ START := #take(minInt(chop(WIDTH), #sizeWordStack(WS)), WS) ] </localMem>


// For each `CALL*` operation, we make a corresponding call to `#call` and a
// state-change to setup the custom parts of the calling environment.


    syntax CallOp ::= "CALL"
 // ------------------------
    rule <k> CALL REG GCAP ACCTTO VALUE ARGSTART ARGWIDTH RETSTART RETWIDTH
          => #checkCall ACCTFROM VALUE
          ~> #call ACCTFROM ACCTTO ACCTTO Ccallgas(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, VALUE) VALUE VALUE #range(LM, ARGSTART, ARGWIDTH) false
          ~> #return RETSTART RETWIDTH REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <localMem> LM </localMem>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    syntax CallOp ::= "CALLCODE"
 // ----------------------------
    rule <k> CALLCODE REG GCAP ACCTTO VALUE ARGSTART ARGWIDTH RETSTART RETWIDTH
          => #checkCall ACCTFROM VALUE
          ~> #call ACCTFROM ACCTFROM ACCTTO Ccallgas(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, VALUE) VALUE VALUE #range(LM, ARGSTART, ARGWIDTH) false
          ~> #return RETSTART RETWIDTH REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <localMem> LM </localMem>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    syntax CallSixOp ::= "DELEGATECALL"
 // -----------------------------------
    rule <k> DELEGATECALL REG GCAP ACCTTO ARGSTART ARGWIDTH RETSTART RETWIDTH
          => #checkCall ACCTFROM 0
          ~> #call ACCTAPPFROM ACCTFROM ACCTTO Ccallgas(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, 0) 0 VALUE #range(LM, ARGSTART, ARGWIDTH) false
          ~> #return RETSTART RETWIDTH REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <caller> ACCTAPPFROM </caller>
         <callValue> VALUE </callValue>
         <localMem> LM </localMem>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>

    syntax CallSixOp ::= "STATICCALL"
 // ---------------------------------
    rule <k> STATICCALL REG GCAP ACCTTO ARGSTART ARGWIDTH RETSTART RETWIDTH
          => #checkCall ACCTFROM 0
          ~> #call ACCTFROM ACCTTO ACCTTO Ccallgas(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, 0) 0 0 #range(LM, ARGSTART, ARGWIDTH) true
          ~> #return RETSTART RETWIDTH REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCTFROM </id>
         <localMem> LM </localMem>
         <activeAccounts> ACCTS </activeAccounts>
         <previousGas> GAVAIL </previousGas>


// ### Account Creation/Deletion

// -   `#create____` transfers the endowment to the new account and triggers the
//     execution of the initialization code.
// -   `#codeDeposit_` checks the result of initialization code and whether the
//     code deposit can be paid, indicating an error if not.


    syntax InternalOp ::= "#create" Int Int Int Int WordStack
                        | "#mkCreate" Int Int WordStack Int Int
                        | "#checkCreate" Int Int
 // --------------------------------------------
    rule <k> #checkCreate ACCT VALUE ~> #create _ _ GAVAIL _ _ => #refund GAVAIL ~> #pushCallStack ~> #pushWorldState ~> #pushSubstate ~> #exception ... </k>
         <callDepth> CD </callDepth>
         <output> _ => .WordStack </output>
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
           <nonce> NONCE => #ifInt EXECMODE ==K VMTESTS #then NONCE #else NONCE +Int 1 #fi </nonce>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if EXECMODE ==K VMTESTS #then EMPTY #else false #fi) ... </activeAccounts>
      requires notBool (VALUE >Int BAL orBool CD >=Int 1024)

    rule #create ACCTFROM ACCTTO GAVAIL VALUE INITCODE
      => #pushCallStack ~> #pushWorldState ~> #pushSubstate
      ~> #newAccount ACCTTO
      ~> #transferFunds ACCTFROM ACCTTO VALUE
      ~> #mkCreate ACCTFROM ACCTTO INITCODE GAVAIL VALUE

    rule <mode> EXECMODE </mode>
         <k> #mkCreate ACCTFROM ACCTTO INITCODE GAVAIL VALUE
          => #initVM ~> #if EXECMODE ==K VMTESTS #then #end #else #execute #fi
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT => ACCTTO </id>
         <gas> OLDGAVAIL => GAVAIL </gas>
         <program> _ => #asMapOps(#dasmOps(INITCODE, SCHED)) </program>
         <programBytes> _ => INITCODE </programBytes>
         <jumpTable> _ => #computeJumpTable(#asMapOps(#dasmOps(INITCODE, SCHED))) </jumpTable>
         <caller> _ => ACCTFROM </caller>
         <callLog> ... (.Set => #ifSet EXECMODE ==K VMTESTS #then SetItem({ 0 | OLDGAVAIL +Int GAVAIL | VALUE | INITCODE }) #else .Set #fi) </callLog>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => .WordStack </callData>
         <callValue> _ => VALUE </callValue>
         <account>
           <acctID> ACCTTO </acctID>
           <nonce> NONCE => #ifInt Gemptyisnonexistent << SCHED >> #then NONCE +Int 1 #else NONCE #fi </nonce>
           ...
         </account>
         <activeAccounts> ... ACCTTO |-> (EMPTY => #if Gemptyisnonexistent << SCHED >> #then false #else EMPTY #fi) ... </activeAccounts>

    syntax KItem ::= "#codeDeposit" Int Reg
                   | "#mkCodeDeposit" Int Reg
                   | "#finishCodeDeposit" Int WordStack Reg
 // -------------------------------------------------------
    rule <k> #exception ~> #codeDeposit _ REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0 ... </k> <output> _ => .WordStack </output>
    rule <k> #revert ~> #codeDeposit _ REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ~> #load REG 0 ... </k>
         <gas> GAVAIL </gas>

    rule <mode> EXECMODE </mode>
         <k> #end ~> #codeDeposit ACCT REG => #mkCodeDeposit ACCT REG ... </k>

    rule <k> #mkCodeDeposit ACCT REG
          => #if EXECMODE ==K VMTESTS #then . #else Gcodedeposit < SCHED > *Int #sizeWordStack(OUT) ~> #deductGas #fi
          ~> #finishCodeDeposit ACCT OUT REG
         ...
         </k>
         <mode> EXECMODE </mode>
         <schedule> SCHED </schedule>
         <output> OUT => .WordStack </output>
      requires #sizeWordStack(OUT) <=Int maxCodeSize < SCHED >

    rule <k> #mkCodeDeposit ACCT REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0 ... </k>
         <schedule> SCHED </schedule>
         <output> OUT => .WordStack </output>
      requires #sizeWordStack(OUT) >Int maxCodeSize < SCHED >

    rule <k> #finishCodeDeposit ACCT OUT REG
          => #popCallStack ~> #if EXECMODE ==K VMTESTS #then #popWorldState #else #dropWorldState #fi ~> #dropSubstate
          ~> #refund GAVAIL ~> #load REG ACCT
         ...
         </k>
         <mode> EXECMODE </mode>
         <gas> GAVAIL </gas>
         <account>
           <acctID> ACCT </acctID>
           <code> _ => OUT </code>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if OUT =/=K .WordStack #then false #else EMPTY #fi) ... </activeAccounts>

    rule <k> #exception ~> #finishCodeDeposit ACCT _ REG
          => #popCallStack ~> #if EXECMODE ==K VMTESTS #then #popWorldState #else #dropWorldState #fi ~> #dropSubstate
          ~> #refund GAVAIL ~> #load REG ACCT
         ...
         </k>
         <mode> EXECMODE </mode>
         <gas> GAVAIL </gas>
         <schedule> FRONTIER </schedule>

    rule <k> #exception ~> #finishCodeDeposit _ _ REG => #popCallStack ~> #popWorldState ~> #popSubstate ~> #load REG 0 ... </k>
         <schedule> SCHED </schedule>
      requires SCHED =/=K FRONTIER


// `CREATE` will attempt to `#create` the account using the initialization code and
// cleans up the result with `#codeDeposit`.


    syntax TernOp ::= "CREATE"
 // --------------------------
    rule <k> CREATE REG VALUE MEMSTART MEMWIDTH
          => #checkCreate ACCT VALUE
          ~> #create ACCT #newAddr(ACCT, NONCE) #ifInt Gstaticcalldepth << SCHED >> #then GAVAIL #else #allBut64th(GAVAIL) #fi VALUE #range(LM, MEMSTART, MEMWIDTH)
          ~> #codeDeposit #newAddr(ACCT, NONCE) REG
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <gas> GAVAIL => #ifInt Gstaticcalldepth << SCHED >> #then 0 #else GAVAIL /Int 64 #fi </gas>
         <localMem> LM </localMem>
         <account>
           <acctID> ACCT </acctID>
           <nonce> NONCE </nonce>
           ...
         </account>


// `SELFDESTRUCT` marks the current account for deletion and transfers funds out of
// the current account. Self destructing to yourself, unlike a regular transfer,
// destroys the balance in the account, irreparably losing it.


    syntax UnVoidOp ::= "SELFDESTRUCT"
 // ----------------------------------
    rule <k> SELFDESTRUCT ACCTTO => #transferFunds ACCT ACCTTO BALFROM ~> #end ... </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <selfDestruct> SDS (.Set => SetItem(ACCT)) </selfDestruct>
         <refund> RF => #ifInt ACCT in SDS #then RF #else RF +Int Rselfdestruct < SCHED > #fi </refund>
         <account>
           <acctID> ACCT </acctID>
           <balance> BALFROM </balance>
           ...
         </account>
         <output> _ => .WordStack </output>
      requires ACCT =/=Int ACCTTO

    rule <k> SELFDESTRUCT ACCT => #end ... </k>
         <schedule> SCHED </schedule>
         <id> ACCT </id>
         <selfDestruct> SDS (.Set => SetItem(ACCT)) </selfDestruct>
         <refund> RF => #ifInt ACCT in SDS #then RF #else RF +Int Rselfdestruct < SCHED > #fi </refund>
         <account>
           <acctID> ACCT </acctID>
           <balance> BALFROM => 0 </balance>
           <nonce> NONCE </nonce>
           <code> CODE </code>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (_ => NONCE ==Int 0 andBool CODE ==K .WordStack) ... </activeAccounts>
         <output> _ => .WordStack </output>


// Precompiled Contracts
// =====================

// -   `#precompiled` is a placeholder for the 4 pre-compiled contracts at
//     addresses 1 through 4.


    syntax NullVoidOp   ::= PrecompiledOp
    syntax PrecompiledOp ::= #precompiled ( Int ) [function]
 // --------------------------------------------------------
    rule #precompiled(1) => ECREC
    rule #precompiled(2) => SHA256
    rule #precompiled(3) => RIP160
    rule #precompiled(4) => ID
    rule #precompiled(5) => MODEXP
    rule #precompiled(6) => ECADD
    rule #precompiled(7) => ECMUL
    rule #precompiled(8) => ECPAIRING

    syntax Set ::= #precompiledAccounts ( Schedule ) [function]
 // -----------------------------------------------------------
    rule #precompiledAccounts(DEFAULT) => SetItem(1) SetItem(2) SetItem(3) SetItem(4)
    rule #precompiledAccounts(FRONTIER) => #precompiledAccounts(DEFAULT)
    rule #precompiledAccounts(HOMESTEAD) => #precompiledAccounts(FRONTIER)
    rule #precompiledAccounts(EIP150) => #precompiledAccounts(HOMESTEAD)
    rule #precompiledAccounts(EIP158) => #precompiledAccounts(EIP150)
    rule #precompiledAccounts(BYZANTIUM) => #precompiledAccounts(EIP158) SetItem(5) SetItem(6) SetItem(7) SetItem(8)
    rule #precompiledAccounts(CONSTANTINOPLE) => #precompiledAccounts(BYZANTIUM)


// -   `ECREC` performs ECDSA public key recovery.
// -   `SHA256` performs the SHA2-257 hash function.
// -   `RIP160` performs the RIPEMD-160 hash function.
// -   `ID` is the identity function (copies input to output).


    syntax PrecompiledOp ::= "ECREC"
 // --------------------------------
    rule <k> ECREC => #end ... </k>
         <callData> DATA </callData>
         <output> _ => #ecrec(#sender(#unparseByteStack(DATA [ 0 .. 32 ]), #asUnsigned(DATA [ 32 .. 32 ]), #unparseByteStack(DATA [ 64 .. 32 ]), #unparseByteStack(DATA [ 96 .. 32 ]))) </output>

    syntax WordStack ::= #ecrec ( Account ) [function]
 // --------------------------------------------------
    rule #ecrec(.Account) => .WordStack
    rule #ecrec(N:Int)    => #padToWidth(32, #asUnsignedBytes(N))

    syntax PrecompiledOp ::= "SHA256"
 // ---------------------------------
    rule <k> SHA256 => #end ... </k>
         <callData> DATA </callData>
         <output> _ => #parseHexBytes(Sha256(#unparseByteStack(DATA))) </output>

    syntax PrecompiledOp ::= "RIP160"
 // ---------------------------------
    rule <k> RIP160 => #end ... </k>
         <callData> DATA </callData>
         <output> _ => #padToWidth(32, #parseHexBytes(RipEmd160(#unparseByteStack(DATA)))) </output>

    syntax PrecompiledOp ::= "ID"
 // -----------------------------
    rule <k> ID => #end ... </k>
         <callData> DATA </callData>
         <output> _ => DATA </output>

    syntax PrecompiledOp ::= "MODEXP"
 // ---------------------------------
    rule <k> MODEXP => #end ... </k>
         <callData> DATA </callData>
         <output> _ => #modexp1(#asWord(DATA [ 0 .. 32 ]), #asWord(DATA [ 32 .. 32 ]), #asWord(DATA [ 64 .. 32 ]), #drop(96,DATA)) </output>

    syntax WordStack ::= #modexp1 ( Int , Int , Int , WordStack ) [function]
                       | #modexp2 ( Int , Int , Int , WordStack ) [function]
                       | #modexp3 ( Int , Int , Int , WordStack ) [function]
                       | #modexp4 ( Int , Int , Int )             [function]
 // -----------------------------------------------------------------------
    rule #modexp1(BASELEN, EXPLEN,   MODLEN, DATA) => #modexp2(#asInteger(DATA [ 0 .. BASELEN ]), EXPLEN, MODLEN, #drop(BASELEN, DATA)) requires MODLEN =/=Int 0
    rule #modexp1(_,       _,        0,      _)    => .WordStack
    rule #modexp2(BASE,    EXPLEN,   MODLEN, DATA) => #modexp3(BASE, #asInteger(DATA [ 0 .. EXPLEN ]), MODLEN, #drop(EXPLEN, DATA))
    rule #modexp3(BASE,    EXPONENT, MODLEN, DATA) => #padToWidth(MODLEN, #modexp4(BASE, EXPONENT, #asInteger(DATA [ 0 .. MODLEN ])))
    rule #modexp4(BASE,    EXPONENT, MODULUS)      => #asByteStack(powmod(BASE, EXPONENT, MODULUS))

    syntax PrecompiledOp ::= "ECADD"
 // --------------------------------
    rule <k> ECADD => #ecadd((#asUnsigned(DATA [ 0 .. 32 ]), #asUnsigned(DATA [ 32 .. 32 ])), (#asUnsigned(DATA [ 64 .. 32 ]), #asUnsigned(DATA [ 96 .. 32 ]))) ... </k>
         <callData> DATA </callData>

    syntax InternalOp ::= #ecadd(G1Point, G1Point)
 // ----------------------------------------------
    rule #ecadd(P1, P2) => #exception
      requires notBool isValidPoint(P1) orBool notBool isValidPoint(P2)
    rule <k> #ecadd(P1, P2) => #end ... </k> <output> _ => #point(BN128Add(P1, P2)) </output>
      requires isValidPoint(P1) andBool isValidPoint(P2)

    syntax PrecompiledOp ::= "ECMUL"
 // --------------------------------
    rule <k> ECMUL => #ecmul((#asUnsigned(DATA [ 0 .. 32 ]), #asUnsigned(DATA [ 32 .. 32 ])), #asUnsigned(DATA [ 64 .. 32 ])) ... </k>
         <callData> DATA </callData>

    syntax InternalOp ::= #ecmul(G1Point, Int)
 // ------------------------------------------
    rule #ecmul(P, S) => #exception
      requires notBool isValidPoint(P)
    rule <k> #ecmul(P, S) => #end ... </k> <output> _ => #point(BN128Mul(P, S)) </output>
      requires isValidPoint(P)

    syntax WordStack ::= #point(G1Point) [function]
 // -----------------------------------------------
    rule #point((X, Y)) => #padToWidth(32, #asUnsignedBytes(X)) ++ #padToWidth(32, #asUnsignedBytes(Y))

    syntax PrecompiledOp ::= "ECPAIRING"
 // ------------------------------------
    rule <k> ECPAIRING => #ecpairing(.List, .List, 0, DATA, #sizeWordStack(DATA)) ... </k>
         <callData> DATA </callData>
      requires #sizeWordStack(DATA) %Int 192 ==Int 0
    rule <k> ECPAIRING => #exception ... </k>
         <callData> DATA </callData>
      requires #sizeWordStack(DATA) %Int 192 =/=Int 0

    syntax InternalOp ::= #ecpairing(List, List, Int, WordStack, Int)
 // -----------------------------------------------------------------
    rule (.K => #checkPoint) ~> #ecpairing((.List => ListItem((#asUnsigned(DATA [ I .. 32 ]), #asUnsigned(DATA [ I +Int 32 .. 32 ])))) _, (.List => ListItem((#asUnsigned(DATA [ I +Int 96 .. 32 ]) x #asUnsigned(DATA [ I +Int 64 .. 32 ]) , #asUnsigned(DATA [ I +Int 160 .. 32 ]) x #asUnsigned(DATA [ I +Int 128 .. 32 ])))) _, I => I +Int 192, DATA, LEN)
    rule <k> #ecpairing(A, B, LEN, _, LEN) => #end ... </k>
         <output> _ => #padToWidth(32, #asUnsignedBytes(bool2Word(BN128AtePairing(A, B)))) </output>

    syntax InternalOp ::= "#checkPoint"
 // -----------------------------------
    rule (#checkPoint => .) ~> #ecpairing(ListItem(AK::G1Point) _, ListItem(BK::G2Point) _, _, _, _)
      requires isValidPoint(AK) andBool isValidPoint(BK)
    rule #checkPoint ~> #ecpairing(ListItem(AK::G1Point) _, ListItem(BK::G2Point) _, _, _, _) => #exception
      requires notBool isValidPoint(AK) orBool notBool isValidPoint(BK)


// Ethereum Gas Calculation
// ========================

// The gas calculation is designed to mirror the style of the yellowpaper. Gas is
// consumed either by increasing the amount of memory being used, or by executing
// opcodes.

// Memory Consumption
// ------------------

// Memory consumed is tracked to determine the appropriate amount of gas to charge
// for each operation. In the yellowpaper, each opcode is defined to consume zero
// gas unless specified otherwise next to the semantics of the opcode (appendix H).

// -   `#memory` computes the new memory size given the old size and next operator
//     (with its arguments).
// -   `#memoryUsageUpdate` is the function `M` in appendix H of the yellowpaper
//     which helps track the memory used.


    syntax Int ::= #memory ( Op , Int ) [function]
 // --------------------------------------------------
    rule #memory ( MLOAD256 _ INDEX   , MU ) => #memoryUsageUpdate(MU, INDEX, 32)
    rule #memory ( MSTORE256 INDEX _  , MU ) => #memoryUsageUpdate(MU, INDEX, 32)
    rule #memory ( MSTORE8 INDEX _ , MU ) => #memoryUsageUpdate(MU, INDEX, 1)

    rule #memory ( SHA3 _ START WIDTH       , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( LOG0 START WIDTH         , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( LOG1 START WIDTH _       , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( LOG2 START WIDTH _ _     , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( LOG3 START WIDTH _ _ _   , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( LOG4 START WIDTH _ _ _ _ , MU ) => #memoryUsageUpdate(MU, START, WIDTH)

    rule #memory ( CODECOPY START _ WIDTH       , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( EXTCODECOPY _ START _ WIDTH  , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( CALLDATACOPY START _ WIDTH   , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( RETURNDATACOPY START _ WIDTH , MU ) => #memoryUsageUpdate(MU, START, WIDTH)

    rule #memory ( CREATE _ _ START WIDTH , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( RETURN START WIDTH     , MU ) => #memoryUsageUpdate(MU, START, WIDTH)
    rule #memory ( REVERT START WIDTH     , MU ) => #memoryUsageUpdate(MU, START, WIDTH)

    rule #memory ( COP:CallOp     _ _ _ _ ARGSTART ARGWIDTH RETSTART RETWIDTH , MU ) => #memoryUsageUpdate(#memoryUsageUpdate(MU, ARGSTART, ARGWIDTH), RETSTART, RETWIDTH)
    rule #memory ( CSOP:CallSixOp _ _ _   ARGSTART ARGWIDTH RETSTART RETWIDTH , MU ) => #memoryUsageUpdate(#memoryUsageUpdate(MU, ARGSTART, ARGWIDTH), RETSTART, RETWIDTH)

    rule #memory(_, MU) => MU [owise]

    syntax Int ::= #memoryUsageUpdate ( Int , Int , Int ) [function]
 // ----------------------------------------------------------------
    rule #memoryUsageUpdate(MU, START, 0)     => MU
    rule #memoryUsageUpdate(MU, START, WIDTH) => maxInt(MU, (chop(START) +Int chop(WIDTH)) up/Int 32) requires WIDTH =/=Int 0


// Execution Gas
// -------------

// Each opcode has an intrinsic gas cost of execution as well (appendix H of the
// yellowpaper).

// -   `#gasExec` loads all the relevant surronding state and uses that to compute
//     the intrinsic execution gas of each opcode.


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

    rule <k> #gasExec(SCHED, CALLDATACOPY    _ _ WIDTH) => Gverylow     < SCHED > +Int (Gcopy < SCHED > *Int (chop(WIDTH) up/Int 32)) ... </k>
    rule <k> #gasExec(SCHED, RETURNDATACOPY  _ _ WIDTH) => Gverylow     < SCHED > +Int (Gcopy < SCHED > *Int (chop(WIDTH) up/Int 32)) ... </k>
    rule <k> #gasExec(SCHED, CODECOPY        _ _ WIDTH) => Gverylow     < SCHED > +Int (Gcopy < SCHED > *Int (chop(WIDTH) up/Int 32)) ... </k>
    rule <k> #gasExec(SCHED, EXTCODECOPY   _ _ _ WIDTH) => Gextcodecopy < SCHED > +Int (Gcopy < SCHED > *Int (chop(WIDTH) up/Int 32)) ... </k>

    rule <k> #gasExec(SCHED, LOG0 _ WIDTH)         => (Glog < SCHED > +Int (Glogdata < SCHED > *Int chop(WIDTH)) +Int (0 *Int Glogtopic < SCHED >)) ... </k>
    rule <k> #gasExec(SCHED, LOG1 _ WIDTH _)       => (Glog < SCHED > +Int (Glogdata < SCHED > *Int chop(WIDTH)) +Int (1 *Int Glogtopic < SCHED >)) ... </k>
    rule <k> #gasExec(SCHED, LOG2 _ WIDTH _ _)     => (Glog < SCHED > +Int (Glogdata < SCHED > *Int chop(WIDTH)) +Int (2 *Int Glogtopic < SCHED >)) ... </k>
    rule <k> #gasExec(SCHED, LOG3 _ WIDTH _ _ _)   => (Glog < SCHED > +Int (Glogdata < SCHED > *Int chop(WIDTH)) +Int (3 *Int Glogtopic < SCHED >)) ... </k>
    rule <k> #gasExec(SCHED, LOG4 _ WIDTH _ _ _ _) => (Glog < SCHED > +Int (Glogdata < SCHED > *Int chop(WIDTH)) +Int (4 *Int Glogtopic < SCHED >)) ... </k>

    rule <k> #gasExec(SCHED, CALL _         GCAP ACCTTO VALUE _ _ _ _) => Ccall(SCHED, ACCTTO,   ACCTS, GCAP, GAVAIL, VALUE) ... </k>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, CALLCODE _     GCAP _      VALUE _ _ _ _) => Ccall(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, VALUE) ... </k>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, DELEGATECALL _ GCAP _ _ _ _ _) => Ccall(SCHED, ACCTFROM, ACCTS, GCAP, GAVAIL, 0) ... </k>
         <id> ACCTFROM </id>
         <activeAccounts> ACCTS </activeAccounts>
         <gas> GAVAIL </gas>

    rule <k> #gasExec(SCHED, STATICCALL _ GCAP ACCTTO _ _ _ _) => Ccall(SCHED, ACCTTO, ACCTS, GCAP, GAVAIL, 0) ... </k>
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

    rule <k> #gasExec(SCHED, CREATE _ _ _ _) => Gcreate < SCHED > ... </k>

    rule <k> #gasExec(SCHED, SHA3 _ _ WIDTH) => Gsha3 < SCHED > +Int (Gsha3word < SCHED > *Int (chop(WIDTH) up/Int 32)) ... </k>

    rule <k> #gasExec(SCHED, JUMPDEST(_)) => Gjumpdest < SCHED > ... </k>
    rule <k> #gasExec(SCHED, SLOAD _ _)     => Gsload    < SCHED > ... </k>

    // Wzero
    rule <k> #gasExec(SCHED, STOP)         => Gzero < SCHED > ... </k>
    rule <k> #gasExec(SCHED, RETURN _ _)   => Gzero < SCHED > ... </k>
    rule <k> #gasExec(SCHED, REVERT _ _)   => Gzero < SCHED > ... </k>
    rule <k> #gasExec(SCHED, REGISTERS(_)) => Gzero < SCHED > ... </k>

    // Wbase
    rule <k> #gasExec(SCHED, ADDRESS _)        => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, ORIGIN _)         => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CALLER _)         => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CALLVALUE _)      => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CALLDATASIZE _)   => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, RETURNDATASIZE _) => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CODESIZE _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, GASPRICE _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, COINBASE _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, TIMESTAMP _)      => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, NUMBER _)         => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, DIFFICULTY _)     => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, GASLIMIT _)       => Gbase < SCHED > ... </k>
    rule <k> #gasExec(SCHED, PC _)             => Gbase < SCHED > ... </k>
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
    rule <k> #gasExec(SCHED, EVMOR _ _ _)        => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, XOR _ _ _)          => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, BYTE _ _ _)         => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, CALLDATALOAD _ _ _) => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MLOAD256 _ _)       => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MSTORE256 _ _)      => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, MSTORE8 _ _)        => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, LOADPOS(_, _) _)    => Gverylow < SCHED > ... </k>
    rule <k> #gasExec(SCHED, LOADNEG(_, _) _)    => Gverylow < SCHED > ... </k>

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
    rule <k> #gasExec(SCHED, JUMP(_)) => Gmid < SCHED > ... </k>

    // Whigh
    rule <k> #gasExec(SCHED, JUMPI(_) _) => Ghigh < SCHED > ... </k>

    rule <k> #gasExec(SCHED, EXTCODESIZE _ _) => Gextcodesize < SCHED > ... </k>
    rule <k> #gasExec(SCHED, BALANCE _ _)     => Gbalance     < SCHED > ... </k>
    rule <k> #gasExec(SCHED, BLOCKHASH _ _)   => Gblockhash   < SCHED > ... </k>

    // Precompiled
    rule <k> #gasExec(_, ECREC)  => 3000 ... </k>
    rule <k> #gasExec(_, SHA256) =>  60 +Int  12 *Int (#sizeWordStack(DATA) up/Int 32) ... </k> <callData> DATA </callData>
    rule <k> #gasExec(_, RIP160) => 600 +Int 120 *Int (#sizeWordStack(DATA) up/Int 32) ... </k> <callData> DATA </callData>
    rule <k> #gasExec(_, ID)     =>  15 +Int   3 *Int (#sizeWordStack(DATA) up/Int 32) ... </k> <callData> DATA </callData>
    rule <k> #gasExec(_, MODEXP) 
           => #multComplexity(maxInt(#asWord(DATA [ 0 .. 32 ]), #asWord(DATA [ 64 .. 32 ]))) *Int maxInt(#adjustedExpLength(#asWord(DATA [ 0 .. 32 ]), #asWord(DATA [ 32 .. 32 ]), DATA), 1) /Int Gquaddivisor < SCHED >
          ...
         </k>
         <schedule> SCHED </schedule>
         <callData> DATA </callData>

    rule #gasExec(_, ECADD) => 500
    rule #gasExec(_, ECMUL) => 40000
    rule <k> #gasExec(_, ECPAIRING) => 100000 +Int (#sizeWordStack(DATA) /Int 192) *Int 80000 ... </k> <callData> DATA </callData>


// There are several helpers for calculating gas (most of them also specified in
// the yellowpaper).

// Note: These are all functions as the operator `#gasExec` has already loaded all
// the relevant state.


    syntax Int ::= Csstore ( Schedule , Int , Int ) [function]
 // ----------------------------------------------------------
    rule Csstore(SCHED, VALUE, OLD) => #ifInt VALUE =/=Int 0 andBool OLD ==Int 0 #then Gsstoreset < SCHED > #else Gsstorereset < SCHED > #fi

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
      requires         #accountNonexistent(SCHED, ACCT, ACCTS) andBool (VALUE =/=Int 0 orBool          Gzerovaluenewaccountgas << SCHED >>)
    rule Cnew(SCHED, ACCT, ACCTS, VALUE) => 0
      requires notBool #accountNonexistent(SCHED, ACCT, ACCTS) orBool  (VALUE  ==Int 0 andBool notBool Gzerovaluenewaccountgas << SCHED >>)

    syntax Int ::= Cselfdestruct ( Schedule , Int , Map , Int ) [function]
 // ----------------------------------------------------------------------
    rule Cselfdestruct(SCHED, ACCT, ACCTS, BAL) => Gselfdestruct < SCHED > +Int Gnewaccount < SCHED >
      requires (#accountNonexistent(SCHED, ACCT, ACCTS)) andBool (        Gselfdestructnewaccount << SCHED >>) andBool (BAL =/=Int 0 orBool          Gzerovaluenewaccountgas << SCHED >>)
    rule Cselfdestruct(SCHED, ACCT, ACCTS, BAL) => Gselfdestruct < SCHED >
      requires (#accountNonexistent(SCHED, ACCT, ACCTS)) andBool (notBool Gselfdestructnewaccount << SCHED >>  orBool  (BAL  ==Int 0 andBool notBool Gzerovaluenewaccountgas << SCHED >>))
    rule Cselfdestruct(SCHED, ACCT, ACCTS, BAL) => Gselfdestruct < SCHED >
      requires notBool #accountNonexistent(SCHED, ACCT, ACCTS)

    syntax Bool ::= #accountNonexistent ( Schedule , Int , Map ) [function]
                  | #accountEmpty ( Int , Map )                  [function]
 // -----------------------------------------------------------------------
    rule #accountNonexistent(SCHED, ACCT, ACCTS) => notBool ACCT in_keys(ACCTS) orBool (#accountEmpty(ACCT, ACCTS) andBool Gemptyisnonexistent << SCHED >>)
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

    syntax Int ::= #multComplexity(Int) [function]
 // ----------------------------------------------
    rule #multComplexity(X) => X *Int X requires X <=Int 64
    rule #multComplexity(X) => X *Int X /Int 4 +Int 96 *Int X -Int 3072 requires X >Int 64 andBool X <=Int 1024
    rule #multComplexity(X) => X *Int X /Int 16 +Int 480 *Int X -Int 199680 requires X >Int 1024

    syntax Int ::= #adjustedExpLength(Int, Int, WordStack) [function]
                 | #adjustedExpLength(Int) [function, klabel(#adjustedExpLengthAux)]
 // ------------------------------------------------------------------
    rule #adjustedExpLength(BASELEN, EXPLEN, DATA) => #ifInt EXPLEN <=Int 32 #then 0 #else 8 *Int (EXPLEN -Int 32) #fi +Int #adjustedExpLength(#asInteger(DATA [ 96 +Int BASELEN .. minInt(EXPLEN, 32) ]))

    rule #adjustedExpLength(0) => 0
    rule #adjustedExpLength(1) => 0
    rule #adjustedExpLength(N) => 1 +Int #adjustedExpLength(N /Int 2) requires N >Int 1


// Fee Schedule from C++ Implementation
// ------------------------------------

// The [C++ Implementation of EVM](https://github.com/ethereum/cpp-ethereum)
// specifies several different "profiles" for how the VM works. Here we provide
// each protocol from the C++ implementation, as the yellowpaper does not contain
// all the different profiles. Specify which profile by passing in the argument
// `-cSCHEDULE=<FEE_SCHEDULE>` when calling `krun` (the available `<FEE_SCHEDULE>`
// are supplied here).

// A `ScheduleFlag` is a boolean determined by the fee schedule; applying a
// `ScheduleFlag` to a `Schedule` yields whether the flag is set or not.


    syntax Bool ::= ScheduleFlag "<<" Schedule ">>" [function]
 // ----------------------------------------------------------

    syntax ScheduleFlag ::= "Gselfdestructnewaccount" | "Gstaticcalldepth" | "Gemptyisnonexistent" | "Gzerovaluenewaccountgas"
                          | "hasRevert" | "hasReturnData" | "hasStaticCall"
 // --------------------------------------------------------------------------------------------------------------------------


// A `ScheduleConst` is a constant determined by the fee schedule; applying a
// `ScheduleConst` to a `Schedule` yields the correct constant for that schedule.


    syntax Int ::= ScheduleConst "<" Schedule ">" [function]
 // --------------------------------------------------------

    syntax ScheduleConst ::= "Gzero"        | "Gbase"          | "Gverylow"      | "Glow"          | "Gmid"        | "Ghigh"
                           | "Gextcodesize" | "Gextcodecopy"   | "Gbalance"      | "Gsload"        | "Gjumpdest"   | "Gsstoreset"
                           | "Gsstorereset" | "Rsstoreclear"   | "Rselfdestruct" | "Gselfdestruct" | "Gcreate"     | "Gcodedeposit"  | "Gcall"
                           | "Gcallvalue"   | "Gcallstipend"   | "Gnewaccount"   | "Gexp"          | "Gexpbyte"    | "Gmemory"       | "Gtxcreate"
                           | "Gtxdatazero"  | "Gtxdatanonzero" | "Gtransaction"  | "Glog"          | "Glogdata"    | "Glogtopic"     | "Gsha3"
                           | "Gsha3word"    | "Gcopy"          | "Gblockhash"    | "Gquadcoeff"    | "maxCodeSize" | "Rb"            | "Gquaddivisor"
 // -------------------------------------------------------------------------------------------------------------------------------------------------


// ### Defualt Schedule


    syntax Schedule ::= "DEFAULT"
 // -----------------------------
    rule Gzero    < DEFAULT > => 0
    rule Gbase    < DEFAULT > => 2
    rule Gverylow < DEFAULT > => 3
    rule Glow     < DEFAULT > => 5
    rule Gmid     < DEFAULT > => 8
    rule Ghigh    < DEFAULT > => 10

    rule Gexp      < DEFAULT > => 10
    rule Gexpbyte  < DEFAULT > => 10
    rule Gsha3     < DEFAULT > => 30
    rule Gsha3word < DEFAULT > => 6

    rule Gsload       < DEFAULT > => 50
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
    rule Gbalance     < DEFAULT > => 20
    rule Gblockhash   < DEFAULT > => 20
    rule Gextcodesize < DEFAULT > => 20
    rule Gextcodecopy < DEFAULT > => 20

    rule maxCodeSize < DEFAULT > => 2 ^Int 32 -Int 1
    rule Rb          < DEFAULT > => 5 *Int (10 ^Int 18)

    rule Gselfdestructnewaccount << DEFAULT >> => false
    rule Gstaticcalldepth        << DEFAULT >> => true
    rule Gemptyisnonexistent     << DEFAULT >> => false
    rule Gzerovaluenewaccountgas << DEFAULT >> => true
    rule hasRevert               << DEFAULT >> => false
    rule hasReturnData           << DEFAULT >> => false
    rule hasStaticCall           << DEFAULT >> => false


// ### Frontier Schedule


    syntax Schedule ::= "FRONTIER"
 // ------------------------------
    rule Gtxcreate  < FRONTIER > => 21000
    rule SCHEDCONST < FRONTIER > => SCHEDCONST < DEFAULT > requires SCHEDCONST =/=K Gtxcreate

    rule SCHEDFLAG << FRONTIER >> => SCHEDFLAG << DEFAULT >>


// ### Homestead Schedule


    syntax Schedule ::= "HOMESTEAD"
 // -------------------------------
    rule SCHEDCONST < HOMESTEAD > => SCHEDCONST < DEFAULT >

    rule SCHEDFLAG << HOMESTEAD >> => SCHEDFLAG << DEFAULT >>


// ### EIP150 Schedule


    syntax Schedule ::= "EIP150"
 // ----------------------------
    rule Gbalance      < EIP150 > => 400
    rule Gsload        < EIP150 > => 200
    rule Gcall         < EIP150 > => 700
    rule Gselfdestruct < EIP150 > => 5000
    rule Gextcodesize  < EIP150 > => 700
    rule Gextcodecopy  < EIP150 > => 700

    rule SCHEDCONST    < EIP150 > => SCHEDCONST < HOMESTEAD >
      requires notBool      ( SCHEDCONST ==K Gbalance      orBool SCHEDCONST ==K Gsload       orBool SCHEDCONST ==K Gcall
                       orBool SCHEDCONST ==K Gselfdestruct orBool SCHEDCONST ==K Gextcodesize orBool SCHEDCONST ==K Gextcodecopy
                            )

    rule Gselfdestructnewaccount << EIP150 >> => true
    rule Gstaticcalldepth        << EIP150 >> => false
    rule SCHEDCONST              << EIP150 >> => SCHEDCONST << HOMESTEAD >>
      requires notBool      ( SCHEDCONST ==K Gselfdestructnewaccount orBool SCHEDCONST ==K Gstaticcalldepth )


// ### EIP158 Schedule


    syntax Schedule ::= "EIP158"
 // ----------------------------
    rule Gexpbyte    < EIP158 > => 50
    rule maxCodeSize < EIP158 > => 24576

    rule SCHEDCONST  < EIP158 > => SCHEDCONST < EIP150 > requires SCHEDCONST =/=K Gexpbyte andBool SCHEDCONST =/=K maxCodeSize

    rule Gemptyisnonexistent     << EIP158 >> => true
    rule Gzerovaluenewaccountgas << EIP158 >> => false
    rule SCHEDCONST              << EIP158 >> => SCHEDCONST << EIP150 >>
      requires notBool      ( SCHEDCONST ==K Gemptyisnonexistent orBool SCHEDCONST ==K Gzerovaluenewaccountgas )


// ### Byzantium Schedule


    syntax Schedule ::= "BYZANTIUM"
 // -------------------------------
    rule Rb         < BYZANTIUM > => 3 *Int (10 ^Int 18)
    rule SCHEDCONST < BYZANTIUM > => SCHEDCONST < EIP158 >
      requires notBool ( SCHEDCONST ==K Rb )

    rule hasRevert     << BYZANTIUM >> => true
    rule hasReturnData << BYZANTIUM >> => true
    rule hasStaticCall << BYZANTIUM >> => true
    rule SCHEDFLAG     << BYZANTIUM >> => SCHEDFLAG << EIP158 >>
      requires notBool ( SCHEDFLAG ==K hasRevert orBool SCHEDFLAG ==K hasReturnData orBool SCHEDFLAG ==K hasStaticCall )


// ### Constantinople Schedule


    syntax Schedule ::= "CONSTANTINOPLE"
 // ------------------------------------
    rule Gblockhash < CONSTANTINOPLE > => 800
    rule SCHEDCONST < CONSTANTINOPLE > => SCHEDCONST < BYZANTIUM >
      requires SCHEDCONST =/=K Gblockhash

    rule SCHEDFLAG << CONSTANTINOPLE >> => SCHEDFLAG << BYZANTIUM >>


// EVM Program Representations
// ===========================

// EVM programs are represented algebraically in K, but programs can load and
// manipulate program data directly. The opcodes `CODECOPY` and `EXTCODECOPY` rely
// on the assembled form of the programs being present. The opcode `CREATE` relies
// on being able to interperet EVM data as a program.

// This is a program representation dependence, which we might want to avoid.
// Perhaps the only program representation dependence we should have is the hash of
// the program; doing so achieves:

// -   Program representation independence (different analysis tools on the
//     language don't have to ensure they have a common representation of programs,
//     just a common interperetation of the data-files holding programs).
// -   Programming language independence (we wouldn't even have to commit to a
//     particular language or interperetation of the data-file).
// -   Only depending on the hash allows us to know that we have *exactly* the
//     correct data-file (program), and nothing more.

// Disassembler
// ------------

// After interpreting the strings representing programs as a `WordStack`, it should
// be changed into an `Ops` for use by the EVM semantics.

// -   `#dasmOps` interperets `WordStack` as an `Ops`.
// -   `#dasmOpCode` interperets a `Int` as an `OpCode`.
// -   `#computeJumpTable` fills in the jump table from a `Map` of `OpCode`.


    syntax Ops ::= #dasmOps ( WordStack , Schedule )       [function]
                 | #dasmOps ( Ops , WordStack , K , Schedule , Int ) [function, klabel(#dasmOpsAux)]
                 | #revOps  ( Ops , Ops )                        [function]
 // -----------------------------------------------------------------------------
    rule #dasmOps( 101 : NBITS : WS, SCHED ) => #revOps(#dasmOps(REGISTERS (NBITS) ; .Ops, WS, .K, SCHED, NBITS), .Ops)
    rule #dasmOps( WS, SCHED ) => #revOps(#dasmOps(.Ops, WS, .K, SCHED, 32), .Ops) [owise]

    rule #dasmOps( OPS, .WordStack, .K, _, _ ) => OPS

    rule #dasmOps( OPS, W : WS, .K, SCHED, NREGS ) => #dasmOps(OPS, WS, #dasmOpCode(W, SCHED), SCHED, NREGS)
      requires (W >=Int 0   andBool W <=Int 95)
        orBool (W >=Int 102 andBool W <=Int 255)

    rule #dasmOps( OPS, W : WS, .K,             SCHED, NREGS ) => #dasmOps(OPS, W : WS, #str(#pushLen(#drop(NREGS up/Int 8, WS)), #pushOffset(#drop(NREGS up/Int 8, WS))), SCHED, NREGS)
      requires W >=Int 96 andBool W <Int 98
    rule #dasmOps( OPS, 96 : WS, #str(LEN, POS), SCHED, NREGS ) => #dasmOps(OPS, WS, LOADPOS(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])), SCHED, NREGS)
    rule #dasmOps( OPS, 97 : WS, #str(LEN, POS), SCHED, NREGS ) => #dasmOps(OPS, WS, LOADNEG(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])), SCHED, NREGS)

    rule #dasmOps( OPS,  98 : W1 : W2 : WS, .K, SCHED, NREGS ) => #dasmOps(OPS, WS, JUMP(W1 *Int 256 +Int W2),     SCHED, NREGS)
    rule #dasmOps( OPS,  99 : W1 : W2 : WS, .K, SCHED, NREGS ) => #dasmOps(OPS, WS, JUMPI(W1 *Int 256 +Int W2),    SCHED, NREGS)
    rule #dasmOps( OPS, 100 : W1 : W2 : WS, .K, SCHED, NREGS ) => #dasmOps(OPS, WS, JUMPDEST(W1 *Int 256 +Int W2), SCHED, NREGS)
    rule #dasmOps( OPS, 101 : W1      : WS, .K, SCHED, NREGS ) => #dasmOps(OPS, WS, REGISTERS(W1),                 SCHED, NREGS)

    rule #dasmOps( OPS, WS, OP:OpCode, SCHED, NREGS) => #dasmOps(#dasmRegs(OP, WS, NREGS) ; OPS, #drop(#opWidth(OP, NREGS) -Int #opCodeWidth(OP), WS), .K, SCHED, NREGS)

    rule #revOps ( OP ; OPS , OPS' ) => #revOps(OPS, OP ; OPS')
    rule #revOps ( .Ops , OPS  ) => OPS

    syntax Op ::= #dasmRegs ( OpCode , WordStack , Int ) [function]
                | #dasmRegs ( OpCode , Int , Int , Int ) [function, klabel(#dasmRegsAux)]
 // -------------------------------------------------------------------------------------
    rule #dasmRegs ( LOADPOS(N, W), WS, NREGS ) => #dasmRegs(LOADPOS(N, W), #asUnsigned(#take(NREGS up/Int 8, WS)),                            NREGS, (1 <<Int NREGS) -Int 1)
    rule #dasmRegs ( LOADNEG(N, W), WS, NREGS ) => #dasmRegs(LOADNEG(N, W), #asUnsigned(#take(NREGS up/Int 8, WS)),                            NREGS, (1 <<Int NREGS) -Int 1)
    rule #dasmRegs ( OP,            WS, NREGS ) => #dasmRegs(OP,            #asUnsigned(#take(#opWidth(OP, NREGS) -Int #opCodeWidth(OP), WS)), NREGS, (1 <<Int NREGS) -Int 1) [owise]

    rule #dasmRegs ( OP:NullOp,     R, W, M ) => OP %(R, W, M, 0)
    rule #dasmRegs ( OP:NullVoidOp, R, W, M ) => OP
    rule #dasmRegs ( OP:UnOp,       R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1)
    rule #dasmRegs ( OP:UnVoidOp,   R, W, M ) => OP %(R, W, M, 0)
    rule #dasmRegs ( OP:BinOp,      R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2)
    rule #dasmRegs ( OP:BinVoidOp,  R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1)
    rule #dasmRegs ( OP:TernOp,     R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3)
    rule #dasmRegs ( OP:TernVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2)
    rule #dasmRegs ( OP:QuadVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3)
    rule #dasmRegs ( OP:FiveVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3) %(R, W, M, 4)
    rule #dasmRegs ( OP:SixVoidOp,  R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3) %(R, W, M, 4) %(R, W, M, 5)
    rule #dasmRegs ( OP:CallSixOp,  R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3) %(R, W, M, 4) %(R, W, M, 5) %(R, W, M, 6)
    rule #dasmRegs ( OP:CallOp,     R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3) %(R, W, M, 4) %(R, W, M, 5) %(R, W, M, 6) %(R, W, M, 7)

    syntax Reg ::= "%" "(" Int "," Int "," Int "," Int ")" [function]
 // -----------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX) => % ((REGS >>Int (IDX *Int WIDTH)) &Int MASK)

    syntax Int ::= #opWidth ( OpCode , Int ) [function]
 // ---------------------------------------------------
    rule #opWidth ( LOADPOS(N, _), NREGS ) => 1 +Int N +Int (NREGS up/Int 8)
    rule #opWidth ( LOADNEG(N, _), NREGS ) => 1 +Int N +Int (NREGS up/Int 8)
    rule #opWidth ( OP, NREGS ) => #opCodeWidth(OP) +Int ((NREGS *Int #numArgs(OP)) up/Int 8) [owise]

    syntax Int ::= #opCodeWidth ( OpCode ) [function]
 // -------------------------------------------------
    rule #opCodeWidth( JUMPDEST(_) )   => 3
    rule #opCodeWidth( JUMP(_) )       => 3
    rule #opCodeWidth( JUMPI(_) )      => 3
    rule #opCodeWidth( REGISTERS(_) )  => 2
    rule #opCodeWidth( OP )            => 1 [owise]

    syntax Int ::= #numArgs ( OpCode ) [function]
 // ---------------------------------------------
    rule #numArgs ( _:NullOp )     => 1
    rule #numArgs ( _:NullVoidOp ) => 0
    rule #numArgs ( _:UnOp )       => 2
    rule #numArgs ( _:UnVoidOp )   => 1
    rule #numArgs ( _:BinOp )      => 3
    rule #numArgs ( _:BinVoidOp )  => 2
    rule #numArgs ( _:TernOp )     => 4
    rule #numArgs ( _:TernVoidOp ) => 3
    rule #numArgs ( _:QuadVoidOp ) => 4
    rule #numArgs ( _:FiveVoidOp ) => 5
    rule #numArgs ( _:SixVoidOp )  => 6
    rule #numArgs ( _:CallSixOp )  => 7
    rule #numArgs ( _:CallOp )     => 8

    syntax OpCode ::= #dasmOpCode ( Int , Schedule ) [function]
 // -----------------------------------------------------------
    rule #dasmOpCode(   0,     _ ) => STOP
    rule #dasmOpCode(   1,     _ ) => ADD
    rule #dasmOpCode(   2,     _ ) => MUL
    rule #dasmOpCode(   3,     _ ) => SUB
    rule #dasmOpCode(   4,     _ ) => DIV
    rule #dasmOpCode(   6,     _ ) => MOD
    rule #dasmOpCode(   7,     _ ) => EXP
    rule #dasmOpCode(   8,     _ ) => ADDMOD
    rule #dasmOpCode(   9,     _ ) => MULMOD
    rule #dasmOpCode(  10,     _ ) => EXPMOD
    rule #dasmOpCode(  11,     _ ) => SIGNEXTEND
    rule #dasmOpCode(  12,     _ ) => TWOS
    rule #dasmOpCode(  16,     _ ) => LT
    rule #dasmOpCode(  17,     _ ) => GT
    rule #dasmOpCode(  20,     _ ) => EQ
    rule #dasmOpCode(  21,     _ ) => ISZERO
    rule #dasmOpCode(  22,     _ ) => AND
    rule #dasmOpCode(  23,     _ ) => EVMOR
    rule #dasmOpCode(  24,     _ ) => XOR
    rule #dasmOpCode(  25,     _ ) => NOT
    rule #dasmOpCode(  26,     _ ) => BYTE
    rule #dasmOpCode(  32,     _ ) => SHA3
    rule #dasmOpCode(  48,     _ ) => ADDRESS
    rule #dasmOpCode(  49,     _ ) => BALANCE
    rule #dasmOpCode(  50,     _ ) => ORIGIN
    rule #dasmOpCode(  51,     _ ) => CALLER
    rule #dasmOpCode(  52,     _ ) => CALLVALUE
    rule #dasmOpCode(  53,     _ ) => CALLDATALOAD
    rule #dasmOpCode(  54,     _ ) => CALLDATASIZE
    rule #dasmOpCode(  55,     _ ) => CALLDATACOPY
    rule #dasmOpCode(  56,     _ ) => CODESIZE
    rule #dasmOpCode(  57,     _ ) => CODECOPY
    rule #dasmOpCode(  58,     _ ) => GASPRICE
    rule #dasmOpCode(  59,     _ ) => EXTCODESIZE
    rule #dasmOpCode(  60,     _ ) => EXTCODECOPY
    rule #dasmOpCode(  61, SCHED ) => RETURNDATASIZE requires hasReturnData << SCHED >>
    rule #dasmOpCode(  62, SCHED ) => RETURNDATACOPY requires hasReturnData << SCHED >>
    rule #dasmOpCode(  64,     _ ) => BLOCKHASH
    rule #dasmOpCode(  65,     _ ) => COINBASE
    rule #dasmOpCode(  66,     _ ) => TIMESTAMP
    rule #dasmOpCode(  67,     _ ) => NUMBER
    rule #dasmOpCode(  68,     _ ) => DIFFICULTY
    rule #dasmOpCode(  69,     _ ) => GASLIMIT
    rule #dasmOpCode(  80,     _ ) => MLOAD8
    rule #dasmOpCode(  81,     _ ) => MLOAD256
    rule #dasmOpCode(  82,     _ ) => MLOAD
    rule #dasmOpCode(  83,     _ ) => MSTORE8
    rule #dasmOpCode(  84,     _ ) => MSTORE256
    rule #dasmOpCode(  85,     _ ) => MSTORE
    rule #dasmOpCode(  86,     _ ) => SLOAD
    rule #dasmOpCode(  87,     _ ) => SSTORE
    rule #dasmOpCode(  88,     _ ) => PC
    rule #dasmOpCode(  89,     _ ) => MSIZE
    rule #dasmOpCode(  90,     _ ) => GAS
    rule #dasmOpCode( 160,     _ ) => LOG0
    rule #dasmOpCode( 161,     _ ) => LOG1
    rule #dasmOpCode( 162,     _ ) => LOG2
    rule #dasmOpCode( 163,     _ ) => LOG3
    rule #dasmOpCode( 164,     _ ) => LOG4
    rule #dasmOpCode( 240,     _ ) => CREATE
    rule #dasmOpCode( 241,     _ ) => CALL
    rule #dasmOpCode( 242,     _ ) => CALLCODE
    rule #dasmOpCode( 243,     _ ) => RETURN
    rule #dasmOpCode( 244, SCHED ) => DELEGATECALL requires SCHED =/=K FRONTIER
    rule #dasmOpCode( 250, SCHED ) => STATICCALL requires hasStaticCall << SCHED >>
    rule #dasmOpCode( 253, SCHED ) => REVERT requires hasRevert << SCHED >>
    rule #dasmOpCode( 255,     _ ) => SELFDESTRUCT
    rule #dasmOpCode(   W,     _ ) => INVALID [owise]

    syntax Map ::= #computeJumpTable ( Map )       [function]
                 | #computeJumpTable ( Map , Map , Set ) [function, klabel(#computeJumpTableAux)]
 // ---------------------------------------------------------------------------------------------
    rule #computeJumpTable(OPS) => #computeJumpTable(OPS, .Map, .Set)

    rule #computeJumpTable(.Map, JUMPS, _) => JUMPS

    rule #computeJumpTable(DEST |-> JUMPDEST(LABEL) OPS, JUMPS, LABELS) => #computeJumpTable(OPS, JUMPS [ LABEL <- DEST  ], LABELS SetItem(LABEL)) requires notBool LABEL in LABELS
    rule #computeJumpTable(_    |-> JUMPDEST(LABEL) OPS, JUMPS, LABELS) => #computeJumpTable(OPS, JUMPS [ LABEL <- undef ], LABELS)                requires         LABEL in LABELS
    rule #computeJumpTable(_    |-> _               OPS, JUMPS, LABELS) => #computeJumpTable(OPS, JUMPS, LABELS)                                   [owise]
endmodule

