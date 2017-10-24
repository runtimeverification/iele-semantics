Ethereum Simulations
====================

Ethereum is using the EVM to drive updates over the world state.
Actual execution of the EVM is defined in [the EVM file](evm.md).

```{.k .uiuck}
requires "verification.k"
```

```{.k .uiuck .rvk}
requires "evm.k"
requires "analysis.k"

module ETHEREUM-SIMULATION
    imports EVM
    imports EVM-ANALYSIS
```

```{.k .uiuck}
    imports VERIFICATION
```

```{.k .rvk}
    imports K-REFLECTION
```

An Ethereum simulation is a list of Ethereum commands.
Some Ethereum commands take an Ethereum specification (eg. for an account or transaction).

```{.k .uiuck .rvk}
    syntax EthereumSimulation ::= ".EthereumSimulation"
                                | EthereumCommand EthereumSimulation
 // ----------------------------------------------------------------
    rule .EthereumSimulation => .
    rule ETC:EthereumCommand ETS:EthereumSimulation => ETC ~> ETS

    syntax EthereumSimulation ::= JSON
 // ----------------------------------
    rule <k> JSONINPUT:JSON => run JSONINPUT success .EthereumSimulation </k>
```

For verification purposes, it's much easier to specify a program in terms of its op-codes and not the hex-encoding that the tests use.
To do so, we'll extend sort `JSON` with some EVM specific syntax, and provide a "pretti-fication" to the nicer input form.

```{.k .uiuck .rvk}
    syntax JSON ::= Int | WordStack | Ops | Map | Call | SubstateLogEntry | Account
 // -----------------------------------------------------------------------------------

    syntax JSONList ::= #sortJSONList ( JSONList )            [function]
                      | #sortJSONList ( JSONList , JSONList ) [function, klabel(#sortJSONListAux)]
 // ----------------------------------------------------------------------------------------------
    rule #sortJSONList(JS) => #sortJSONList(JS, .JSONList)
    rule #sortJSONList(.JSONList, LS)            => LS
    rule #sortJSONList(((KEY : VAL) , REST), LS) => #insertJSONKey((KEY : VAL), #sortJSONList(REST, LS))

    syntax JSONList ::= #insertJSONKey ( JSON , JSONList ) [function]
 // -----------------------------------------------------------------
    rule #insertJSONKey( JS , .JSONList ) => JS , .JSONList
    rule #insertJSONKey( (KEY : VAL) , ((KEY' : VAL') , REST) ) => (KEY : VAL)   , (KEY' : VAL')              , REST  requires KEY <String KEY'
    rule #insertJSONKey( (KEY : VAL) , ((KEY' : VAL') , REST) ) => (KEY' : VAL') , #insertJSONKey((KEY : VAL) , REST) requires KEY >=String KEY'

    syntax Bool ::= #isSorted ( JSONList ) [function]
 // -------------------------------------------------
    rule #isSorted( .JSONList ) => true
    rule #isSorted( KEY : _ )   => true
    rule #isSorted( (KEY : _) , (KEY' : VAL) , REST ) => KEY <=String KEY' andThenBool #isSorted((KEY' : VAL) , REST)
```

### Driving Execution

-   `start` places `#next` on the `<k>` cell so that execution of the loaded state begin.
-   `flush` places `#finalize` on the `<k>` cell once it sees `#end` in the `<k>` cell, clearing any exceptions it finds.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "start"
 // ----------------------------------
    rule <mode> NORMAL     </mode> <k> start => #execute    ... </k>
    rule <mode> VMTESTS    </mode> <k> start => #execute    ... </k>
    rule <mode> GASANALYZE </mode> <k> start => #gasAnalyze ... </k>

    syntax EthereumCommand ::= "flush"
 // ----------------------------------
    rule <k> #end       ~> flush => #finalizeTx(false)               ... </k>
    rule <k> #exception ~> flush => #finalizeTx(false) ~> #exception ... </k>
```

-   `startTx` computes the sender of the transaction, and places loadTx on the `k` cell.
-   `loadTx(_)` loads the next transaction to be executed into the current state.
-   `finishTx` is a place-holder for performing necessary cleanup after a transaction.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "startTx"
 // ------------------------------------
    rule <k> startTx => #finalizeBlock ... </k>
         <txPending> .List </txPending>

    rule <k> startTx => loadTx(#sender(TN, TP, TG, TT, TV, #unparseByteStack(DATA), TW, TR, TS)) ... </k>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID>      TXID </msgID>
           <txNonce>    TN   </txNonce>
           <txGasPrice> TP   </txGasPrice>
           <txGasLimit> TG   </txGasLimit>
           <to>         TT   </to>
           <value>      TV   </value>
           <v>          TW   </v>
           <r>          TR   </r>
           <s>          TS   </s>
           <data>       DATA </data>
         </message>

    syntax EthereumCommand ::= loadTx ( Int )
 // -----------------------------------------
    rule <k> loadTx(ACCTFROM)
          => #create ACCTFROM #newAddr(ACCTFROM, NONCE) (GLIMIT -Int G0(SCHED, CODE, true)) VALUE CODE
          ~> #execute ~> #finishTx ~> #finalizeTx(false) ~> startTx
         ...
         </k>
         <schedule> SCHED </schedule>
         <gasPrice> _ => GPRICE </gasPrice>
         <origin> _ => ACCTFROM </origin>
         <callDepth> _ => -1 </callDepth>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID>      TXID     </msgID>
           <txGasPrice> GPRICE   </txGasPrice>
           <txGasLimit> GLIMIT   </txGasLimit>
           <to>         .Account </to>
           <value>      VALUE    </value>
           <data>       CODE     </data>
           ...
         </message>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL => BAL -Int (GLIMIT *Int GPRICE) </balance>
           <nonce> NONCE => NONCE +Int 1 </nonce>
           ...
         </account>
         <activeAccounts> ... ACCTFROM |-> (_ => false) ... </activeAccounts>

    rule <k> loadTx(ACCTFROM)
          => #call ACCTFROM ACCTTO ACCTTO (GLIMIT -Int G0(SCHED, DATA, false)) VALUE VALUE DATA false
          ~> #execute ~> #finishTx ~> #finalizeTx(false) ~> startTx
         ...
         </k>
         <schedule> SCHED </schedule>
         <gasPrice> _ => GPRICE </gasPrice>
         <origin> _ => ACCTFROM </origin>
         <callDepth> _ => -1 </callDepth>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID>      TXID   </msgID>
           <txGasPrice> GPRICE </txGasPrice>
           <txGasLimit> GLIMIT </txGasLimit>
           <to>         ACCTTO </to>
           <value>      VALUE  </value>
           <data>       DATA   </data>
           ...
         </message>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL => BAL -Int (GLIMIT *Int GPRICE) </balance>
           <nonce> NONCE => NONCE +Int 1 </nonce>
           ...
         </account>
         <activeAccounts> ... ACCTFROM |-> (_ => false) ... </activeAccounts>
      requires ACCTTO =/=K .Account

    syntax EthereumCommand ::= "#finishTx"
 // --------------------------------------
    rule <k> #exception ~> #finishTx => #popCallStack ~> #popWorldState ~> #popSubstate ... </k>
    rule <k> #revert    ~> #finishTx => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ... </k> <gas> GAVAIL </gas>       

    rule <k> #end ~> #finishTx => #mkCodeDeposit ACCT %0 ... </k>
         <id> ACCT </id>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID> TXID     </msgID>
           <to>    .Account </to>
           ...
         </message>

    rule <k> #end ~> #finishTx => #popCallStack ~> #dropWorldState ~> #dropSubstate ~> #refund GAVAIL ... </k>
         <id> ACCT </id>
         <gas> GAVAIL </gas>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID> TXID </msgID>
           <to>    TT   </to>
           ...
         </message>
      requires TT =/=K .Account
```

-   `#finalizeBlock` is used to signal that block finalization procedures should take place (after transactions have executed).
-   `#rewardOmmers(_)` pays out the reward to uncle blocks so that blocks are orphaned less often in Ethereum.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "#finalizeBlock" | #rewardOmmers ( JSONList )
 // ------------------------------------------------------------------------
    rule <k> #finalizeBlock => #rewardOmmers(OMMERS) ... </k>
         <schedule> SCHED </schedule>
         <ommerBlockHeaders> [ OMMERS ] </ommerBlockHeaders>
         <coinbase> MINER </coinbase>
         <account>
           <acctID> MINER </acctID>
           <balance> MINBAL => MINBAL +Int Rb < SCHED > </balance>
           ...
         </account>
         <activeAccounts> ... MINER |-> (_ => false) ... </activeAccounts>

    rule <k> (.K => #newAccount MINER) ~> #finalizeBlock ... </k>
         <coinbase> MINER </coinbase>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool MINER in_keys(ACCTS)

    rule <k> #rewardOmmers(.JSONList) => . ... </k>
    rule <k> #rewardOmmers([ _ , _ , OMMER , _ , _ , _ , _ , _ , OMMNUM , _ ] , REST) => #rewardOmmers(REST) ... </k>
         <schedule> SCHED </schedule>
         <coinbase> MINER </coinbase>
         <number> CURNUM </number>
         <account>
           <acctID> MINER </acctID>
           <balance> MINBAL => MINBAL +Int Rb < SCHED > /Int 32 </balance>
          ...
         </account>
         <account>
           <acctID> OMMER </acctID>
           <balance> OMMBAL => OMMBAL +Int Rb < SCHED > +Int (OMMNUM -Int CURNUM) *Int (Rb < SCHED > /Int 8) </balance>
          ...
         </account>
         <activeAccounts> ... MINER |-> (_ => false) OMMER |-> (_ => false) ... </activeAccounts>
```

-   `exception` only clears from the `<k>` cell if there is an exception preceding it.
-   `failure_` holds the name of a test that failed if a test does fail.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "exception" | "failure" String | "success"
 // ---------------------------------------------------------------------
    rule <k> #exception ~> exception => . ... </k>
    rule <k> success => . ... </k> <exit-code> _ => 0 </exit-code>
    rule failure _ => .
```

### Running Tests

-   `run` runs a given set of Ethereum tests (from the test-set).

Note that `TEST` is sorted here so that key `"network"` comes before key `"pre"`.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "run" JSON
 // -------------------------------------
    rule run { .JSONList } => .
    rule run { TESTID : { TEST:JSONList } , TESTS }
      => run ( TESTID : { #sortJSONList(TEST) } )
      ~> #if #hasPost?( { TEST } ) #then .K #else exception #fi
      ~> clear
      ~> run { TESTS }

    syntax Bool ::= "#hasPost?" "(" JSON ")" [function]
 // ---------------------------------------------------
    rule #hasPost? ({ .JSONList }) => false
    rule #hasPost? ({ (KEY:String) : _ , REST }) => (KEY in #postKeys) orBool #hasPost? ({ REST })
```

-   `#loadKeys` are all the JSON nodes which should be considered as loads before execution.

```{.k .uiuck .rvk}
    syntax Set ::= "#loadKeys" [function]
 // -------------------------------------
    rule #loadKeys => ( SetItem("env") SetItem("pre") SetItem("rlp") SetItem("network") SetItem("genesisRLP") )

    rule run TESTID : { KEY : (VAL:JSON) , REST } => load KEY : VAL ~> run TESTID : { REST } requires KEY in #loadKeys

    rule run TESTID : { "blocks" : [ { KEY : VAL , REST1 => REST1 }, .JSONList ] , ( REST2 => KEY : VAL , REST2 ) }
    rule run TESTID : { "blocks" : [ { .JSONList }, .JSONList ] , REST } => run TESTID : { REST }
```

-   `#execKeys` are all the JSON nodes which should be considered for execution (between loading and checking).

```{.k .uiuck .rvk}
    syntax Set ::= "#execKeys" [function]
 // -------------------------------------
    rule #execKeys => ( SetItem("exec") SetItem("lastblockhash") )

    rule run TESTID : { KEY : (VAL:JSON) , NEXT , REST } => run TESTID : { NEXT , KEY : VAL , REST } requires KEY in #execKeys

    rule run TESTID : { "exec" : (EXEC:JSON) } => load "exec" : EXEC ~> start ~> flush
    rule run TESTID : { "lastblockhash" : (HASH:String) } => startTx
```

-   `#postKeys` are a subset of `#checkKeys` which correspond to post-state account checks.
-   `#checkKeys` are all the JSON nodes which should be considered as checks after execution.

```{.k .uiuck .rvk}
    syntax Set ::= "#postKeys" [function] | "#allPostKeys" [function] | "#checkKeys" [function]
 // -------------------------------------------------------------------------------------------
    rule #postKeys    => ( SetItem("post") SetItem("postState") )
    rule #allPostKeys => ( #postKeys SetItem("expect") SetItem("export") SetItem("expet") )
    rule #checkKeys   => ( #allPostKeys SetItem("logs") SetItem("callcreates") SetItem("out") SetItem("gas")
                           SetItem("blockHeader") SetItem("transactions") SetItem("uncleHeaders") SetItem("genesisBlockHeader")
                         )

    rule run TESTID : { KEY : (VAL:JSON) , REST } => run TESTID : { REST } ~> check TESTID : { "post" : VAL } requires KEY in #allPostKeys
    rule run TESTID : { KEY : (VAL:JSON) , REST } => run TESTID : { REST } ~> check TESTID : { KEY    : VAL } requires KEY in #checkKeys andBool notBool KEY in #allPostKeys
```

-   `#discardKeys` are all the JSON nodes in the tests which should just be ignored.

```{.k .uiuck .rvk}
    syntax Set ::= "#discardKeys" [function]
 // ----------------------------------------
    rule #discardKeys => ( SetItem("//") SetItem("_info") )

    rule run TESTID : { KEY : _ , REST } => run TESTID : { REST } requires KEY in #discardKeys
```

State Manipulation
------------------

### Clearing State

-   `clear` clears all the execution state of the machine.
-   `clearX` clears the substate `X`, for `TX`, `BLOCK`, and `NETWORK`.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "clear"
 // ----------------------------------
    rule <k> clear => clearTX ~> clearBLOCK ~> clearNETWORK ... </k>
         <analysis> _ => .Map </analysis>

    syntax EthreumCommand ::= "clearTX"
 // -----------------------------------
    rule <k> clearTX => . ... </k>
         <output>       _ => .WordStack </output>
         <memoryUsed>   _ => 0          </memoryUsed>
         <callDepth>    _ => 0          </callDepth>
         <callStack>    _ => .List      </callStack>
         <callLog>      _ => .Set       </callLog>
         <program>      _ => .Map       </program>
         <programBytes> _ => .WordStack </programBytes>
         <jumpTable>    _ => .Map       </jumpTable>
         <id>           _ => 0          </id>
         <caller>       _ => 0          </caller>
         <callData>     _ => .WordStack </callData>
         <callValue>    _ => 0          </callValue>
         <regs>         _ => .Array     </regs>
         <localMem>     _ => .Array     </localMem>
         <pc>           _ => 0          </pc>
         <gas>          _ => 0          </gas>
         <previousGas>  _ => 0          </previousGas>
         <selfDestruct> _ => .Set       </selfDestruct>
         <log>          _ => .List      </log>
         <refund>       _ => 0          </refund>
         <gasPrice>     _ => 0          </gasPrice>
         <origin>       _ => 0          </origin>

    syntax EthreumCommand ::= "clearBLOCK"
 // --------------------------------------
    rule <k> clearBLOCK => . ... </k>
         <previousHash>      _ => 0             </previousHash>
         <ommersHash>        _ => 0             </ommersHash>
         <coinbase>          _ => 0             </coinbase>
         <stateRoot>         _ => 0             </stateRoot>
         <transactionsRoot>  _ => 0             </transactionsRoot>
         <receiptsRoot>      _ => 0             </receiptsRoot>
         <logsBloom>         _ => .WordStack    </logsBloom>
         <difficulty>        _ => 0             </difficulty>
         <number>            _ => 0             </number>
         <gasLimit>          _ => 0             </gasLimit>
         <gasUsed>           _ => 0             </gasUsed>
         <timestamp>         _ => 0             </timestamp>
         <extraData>         _ => .WordStack    </extraData>
         <mixHash>           _ => 0             </mixHash>
         <blockNonce>        _ => 0             </blockNonce>
         <ommerBlockHeaders> _ => [ .JSONList ] </ommerBlockHeaders>
         <blockhash>         _ => .List         </blockhash>

    syntax EthreumCommand ::= "clearNETWORK"
 // ----------------------------------------
    rule <k> clearNETWORK => . ... </k>
         <activeAccounts> _ => .Map    </activeAccounts>
         <accounts>       _ => .Bag    </accounts>
         <messages>       _ => .Bag    </messages>
         <schedule>       _ => DEFAULT </schedule>
```

### Loading State

-   `mkAcct_` creates an account with the supplied ID (assuming it's already been chopped to 160 bits).

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "mkAcct" Int
 // ---------------------------------------
    rule <k> mkAcct ACCT => #newAccount ACCT ... </k>
```

-   `load` loads an account or transaction into the world state.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "load" JSON
 // --------------------------------------
    rule load DATA : { .JSONList } => .
    rule load DATA : { KEY : VALUE , REST } => load DATA : { KEY : VALUE } ~> load DATA : { REST }
      requires REST =/=K .JSONList andBool DATA =/=String "transaction"

    rule load DATA : [ .JSONList ] => .
    rule load DATA : [ { TEST } , REST ] => load DATA : { TEST } ~> load DATA : [ REST ]
```

Here we perform pre-proccesing on account data which allows "pretty" specification of input.

```{.k .uiuck .rvk}
    rule load "pre" : { (ACCTID:String) : ACCT } => mkAcct #parseAddr(ACCTID) ~> load "account" : { ACCTID : ACCT }
    rule load "account" : { ACCTID: { KEY : VALUE , REST } } => load "account" : { ACCTID : { KEY : VALUE } } ~> load "account" : { ACCTID : { REST } } requires REST =/=K .JSONList

    rule load "account" : { ((ACCTID:String) => #parseAddr(ACCTID)) : ACCT }
    rule load "account" : { (ACCT:Int) : { "balance" : ((VAL:String)         => #parseWord(VAL)) } }
    rule load "account" : { (ACCT:Int) : { "nonce"   : ((VAL:String)         => #parseWord(VAL)) } }
    rule load "account" : { (ACCT:Int) : { "code"    : ((CODE:String)        => #parseByteStack(CODE)) } }
    rule load "account" : { (ACCT:Int) : { "storage" : ({ STORAGE:JSONList } => #parseMap({ STORAGE })) } }

```

The individual fields of the accounts are dealt with here.

```{.k .uiuck .rvk}
    rule <k> load "account" : { ACCT : { "balance" : (BAL:Int) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <balance> _ => BAL </balance>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if BAL =/=Int 0 #then false #else EMPTY #fi) ... </activeAccounts>

    rule <k> load "account" : { ACCT : { "code" : (CODE:WordStack) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> _ => CODE </code>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if CODE =/=K .WordStack #then false #else EMPTY #fi) ... </activeAccounts>

    rule <k> load "account" : { ACCT : { "nonce" : (NONCE:Int) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <nonce> _ => NONCE </nonce>
           ...
         </account>
         <activeAccounts> ... ACCT |-> (EMPTY => #if NONCE =/=Int 0 #then false #else EMPTY #fi) ... </activeAccounts>

    rule <k> load "account" : { ACCT : { "storage" : (STORAGE:Map) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <storage> _ => STORAGE </storage>
           ...
         </account>
```

Here we load the environmental information.

```{.k .uiuck .rvk}
    rule load "env" : { KEY : ((VAL:String) => #parseWord(VAL)) }
      requires KEY in (SetItem("currentTimestamp") SetItem("currentGasLimit") SetItem("currentNumber") SetItem("currentDifficulty"))
    rule load "env" : { KEY : ((VAL:String) => #parseHexWord(VAL)) }
      requires KEY in (SetItem("currentCoinbase") SetItem("previousHash"))
 // ----------------------------------------------------------------------
    rule <k> load "env" : { "currentCoinbase"   : (CB:Int)     } => . ... </k> <coinbase>     _ => CB     </coinbase>
    rule <k> load "env" : { "currentDifficulty" : (DIFF:Int)   } => . ... </k> <difficulty>   _ => DIFF   </difficulty>
    rule <k> load "env" : { "currentGasLimit"   : (GLIMIT:Int) } => . ... </k> <gasLimit>     _ => GLIMIT </gasLimit>
    rule <k> load "env" : { "currentNumber"     : (NUM:Int)    } => . ... </k> <number>       _ => NUM    </number>
    rule <k> load "env" : { "previousHash"      : (HASH:Int)   } => . ... </k> <previousHash> _ => HASH   </previousHash>
    rule <k> load "env" : { "currentTimestamp"  : (TS:Int)     } => . ... </k> <timestamp>    _ => TS     </timestamp>

    rule load "exec" : { KEY : ((VAL:String) => #parseWord(VAL)) }
      requires KEY in (SetItem("gas") SetItem("gasPrice") SetItem("value"))
    rule load "exec" : { KEY : ((VAL:String) => #parseHexWord(VAL)) }
      requires KEY in (SetItem("address") SetItem("caller") SetItem("origin"))
 // --------------------------------------------------------------------------
    rule <k> load "exec" : { "gasPrice" : (GPRICE:Int)   } => . ... </k> <gasPrice>  _ => GPRICE   </gasPrice>
    rule <k> load "exec" : { "gas"      : (GAVAIL:Int)   } => . ... </k> <gas>       _ => GAVAIL   </gas>
    rule <k> load "exec" : { "address"  : (ACCTTO:Int)   } => . ... </k> <id>        _ => ACCTTO   </id>
    rule <k> load "exec" : { "caller"   : (ACCTFROM:Int) } => . ... </k> <caller>    _ => ACCTFROM </caller>
    rule <k> load "exec" : { "gas"      : (GAVAIL:Int)   } => . ... </k> <gas>       _ => GAVAIL   </gas>
    rule <k> load "exec" : { "value"    : (VALUE:Int)    } => . ... </k> <callValue> _ => VALUE    </callValue>
    rule <k> load "exec" : { "origin"   : (ORIG:Int)     } => . ... </k> <origin>    _ => ORIG     </origin>
    rule <k> load "exec" : { "code"     : ((CODE:String)   => #parseByteStack(CODE)) } ... </k>

    rule load "exec" : { "data" : ((DATA:String) => #parseByteStack(DATA)) }
 // ------------------------------------------------------------------------
    rule <k> load "exec" : { "data" : (DATA:WordStack) } => . ... </k> <callData> _ => DATA </callData>
    rule <k> load "exec" : { "code" : (CODE:WordStack) } => . ... </k>
         <program>  _ => #asMapOps(#dasmOps(CODE, SCHED)) </program>
         <programBytes> _ => CODE </programBytes>
         <jumpTable> _ => #computeJumpTable(#asMapOps(#dasmOps(CODE, SCHED))) </jumpTable>
         <schedule> SCHED </schedule>
```

The `"network"` key allows setting the fee schedule inside the test.

```{.k .uiuck .rvk}
    rule <k> load "network" : SCHEDSTRING => . ... </k>
         <schedule> _ => #asScheduleString(SCHEDSTRING) </schedule>

    syntax Schedule ::= #asScheduleString ( String ) [function]
 // -----------------------------------------------------------
    rule #asScheduleString("EIP150")         => EIP150
    rule #asScheduleString("EIP158")         => EIP158
    rule #asScheduleString("Frontier")       => FRONTIER
    rule #asScheduleString("Homestead")      => HOMESTEAD
    rule #asScheduleString("Byzantium")      => BYZANTIUM
    rule #asScheduleString("Constantinople") => CONSTANTINOPLE
```

The `"rlp"` key loads the block information.

```{.k .uiuck .rvk}
    rule load "rlp" : (VAL:String => #rlpDecode(#unparseByteStack(#parseByteStack(VAL))))
    rule load "genesisRLP" : (VAL:String => #rlpDecode(#unparseByteStack(#parseByteStack(VAL))))
 // --------------------------------------------------------------------------------------------
    rule <k> load "rlp" : [ [ HP , HO , HC , HR , HT , HE , HB , HD , HI , HL , HG , HS , HX , HM , HN , .JSONList ] , BT , BU , .JSONList ]
          => load "transaction" : BT
         ...
         </k>
         <previousHash>      _ => #asUnsigned(#parseByteStackRaw(HP)) </previousHash>
         <ommersHash>        _ => #asUnsigned(#parseByteStackRaw(HO)) </ommersHash>
         <coinbase>          _ => #asUnsigned(#parseByteStackRaw(HC)) </coinbase>
         <stateRoot>         _ => #asUnsigned(#parseByteStackRaw(HR)) </stateRoot>
         <transactionsRoot>  _ => #asUnsigned(#parseByteStackRaw(HT)) </transactionsRoot>
         <receiptsRoot>      _ => #asUnsigned(#parseByteStackRaw(HE)) </receiptsRoot>
         <logsBloom>         _ => #parseByteStackRaw(HB)          </logsBloom>
         <difficulty>        _ => #asUnsigned(#parseByteStackRaw(HD)) </difficulty>
         <number>            _ => #asUnsigned(#parseByteStackRaw(HI)) </number>
         <gasLimit>          _ => #asUnsigned(#parseByteStackRaw(HL)) </gasLimit>
         <gasUsed>           _ => #asUnsigned(#parseByteStackRaw(HG)) </gasUsed>
         <timestamp>         _ => #asUnsigned(#parseByteStackRaw(HS)) </timestamp>
         <extraData>         _ => #parseByteStackRaw(HX)          </extraData>
         <mixHash>           _ => #asUnsigned(#parseByteStackRaw(HM)) </mixHash>
         <blockNonce>        _ => #asUnsigned(#parseByteStackRaw(HN)) </blockNonce>
         <ommerBlockHeaders> _ => BU                              </ommerBlockHeaders>

    rule <k> load "genesisRLP": [ [ HP, HO, HC, HR, HT, HE:String, HB, HD, HI, HL, HG, HS, HX, HM, HN, .JSONList ], _, _, .JSONList ] => .K ... </k>
         <blockhash> .List => ListItem(#blockHeaderHash(HP, HO, HC, HR, HT, HE, HB, HD, HI, HL, HG, HS, HX, HM, HN)) ListItem(#asUnsigned(#parseByteStackRaw(HP))) ... </blockhash>

    rule <k> load "transaction" : [ [ TN , TP , TG , TT , TV , TI , TW , TR , TS ] , REST => REST ] ... </k>
         <txOrder>   ... .List => ListItem(!ID) </txOrder>
         <txPending> ... .List => ListItem(!ID) </txPending>
         <messages>
           ( .Bag
          => <message>
               <msgID>      !ID:Int                                 </msgID>
               <txNonce>    #asUnsigned(#parseByteStackRaw(TN))         </txNonce>
               <txGasPrice> #asUnsigned(#parseByteStackRaw(TP))         </txGasPrice>
               <txGasLimit> #asUnsigned(#parseByteStackRaw(TG))         </txGasLimit>
               <to>         #asAccount(#parseByteStackRaw(TT))      </to>
               <value>      #asUnsigned(#parseByteStackRaw(TV))         </value>
               <v>          #asUnsigned(#parseByteStackRaw(TW))         </v>
               <r>          #padToWidth(32, #parseByteStackRaw(TR)) </r>
               <s>          #padToWidth(32, #parseByteStackRaw(TS)) </s>
               <data>       #parseByteStackRaw(TI)                  </data>
             </message>
           )
           ...
         </messages>
```

### Checking State

-   `check_` checks if an account/transaction appears in the world-state as stated.

```{.k .uiuck .rvk}
    syntax EthereumCommand ::= "check" JSON
 // ---------------------------------------
    rule #exception ~> check J:JSON => check J ~> #exception
    rule check DATA : { .JSONList } => . requires DATA =/=String "transactions"
    rule check DATA : { (KEY:String) : VALUE , REST } => check DATA : { KEY : VALUE } ~> check DATA : { REST }
      requires REST =/=K .JSONList andBool notBool DATA in (SetItem("callcreates") SetItem("transactions"))

    rule check DATA : [ .JSONList ] => . requires DATA =/=String "ommerHeaders"
    rule check DATA : [ { TEST } , REST ] => check DATA : { TEST } ~> check DATA : [ REST ] requires DATA =/=String "transactions"

    rule check (KEY:String) : { JS:JSONList => #sortJSONList(JS) }
      requires KEY in (SetItem("callcreates")) andBool notBool #isSorted(JS)

    rule check TESTID : { "post" : POST } => check "account" : POST ~> failure TESTID
    rule check "account" : { ACCTID: { KEY : VALUE , REST } } => check "account" : { ACCTID : { KEY : VALUE } } ~> check "account" : { ACCTID : { REST } } requires REST =/=K .JSONList
 // -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    rule check "account" : { ((ACCTID:String) => #parseAddr(ACCTID)) : ACCT }
    rule check "account" : { (ACCT:Int) : { "balance" : ((VAL:String)         => #parseWord(VAL)) } }
    rule check "account" : { (ACCT:Int) : { "nonce"   : ((VAL:String)         => #parseWord(VAL)) } }
    rule check "account" : { (ACCT:Int) : { "code"    : ((CODE:String)        => #parseByteStack(CODE)) } }
    rule check "account" : { (ACCT:Int) : { "storage" : ({ STORAGE:JSONList } => #parseMap({ STORAGE })) } }


    rule <k> check "account" : { ACCT : { "balance" : (BAL:Int) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <balance> BAL </balance>
           ...
         </account>

    rule <k> check "account" : { ACCT : { "nonce" : (NONCE:Int) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <nonce> NONCE </nonce>
           ...
         </account>

    rule <k> check "account" : { ACCT : { "storage" : (STORAGE:Map) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <storage> ACCTSTORAGE </storage>
           ...
         </account>
      requires #removeZeros(#adjustStorageValues(ACCTSTORAGE)) ==K STORAGE

    rule <k> check "account" : { ACCT : { "code" : (CODE:WordStack) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> CODE </code>
           ...
         </account>

    syntax Map ::= #adjustStorageValues(Map) [function]
 // ---------------------------------------------------
    rule #adjustStorageValues(K |-> V M) => K |-> chop(V) #adjustStorageValues(M)
    rule #adjustStorageValues(.Map) => .Map
```

Here we check the other post-conditions associated with an EVM test.

```{.k .uiuck .rvk}
    rule check TESTID : { "out" : OUT } => check "out" : OUT ~> failure TESTID
 // --------------------------------------------------------------------------
    rule check "out" : ((OUT:String) => #parseByteStack(OUT))
    rule <k> check "out" : OUT => . ... </k> <output> OUT </output>

    rule check TESTID : { "logs" : LOGS } => check "logs" : LOGS ~> failure TESTID
 // ------------------------------------------------------------------------------
    rule <k> check "logs" : HASH:String => . ... </k> <log> SL </log> requires #parseHexBytes(Keccak256(#rlpEncodeLogs(SL))) ==K #parseByteStack(HASH)

    syntax String ::= #rlpEncodeLogs(List)        [function]
                    | #rlpEncodeLogsAux(List)     [function]
                    | #rlpEncodeTopics(WordStack) [function]
 // --------------------------------------------------------
    rule #rlpEncodeLogs(SL) => #rlpEncodeLength(#rlpEncodeLogsAux(SL), 192)
    rule #rlpEncodeLogsAux(ListItem({ ACCT | TOPICS | DATA }) SL) => #rlpEncodeLength(#rlpEncodeBytes(ACCT, 20) +String #rlpEncodeLength(#rlpEncodeTopics(TOPICS), 192) +String #rlpEncodeString(#unparseByteStack(DATA)), 192) +String #rlpEncodeLogsAux(SL)
    rule #rlpEncodeLogsAux(.List) => ""
    rule #rlpEncodeTopics(TOPIC : TOPICS) => #rlpEncodeBytes(chop(TOPIC), 32) +String #rlpEncodeTopics(TOPICS)
    rule #rlpEncodeTopics(.WordStack) => ""

    rule check TESTID : { "gas" : GLEFT } => check "gas" : GLEFT ~> failure TESTID
 // ------------------------------------------------------------------------------
    rule check "gas" : ((GLEFT:String) => #parseWord(GLEFT))
    rule <k> check "gas" : GLEFT => . ... </k> //<gas> GLEFT </gas>

    rule check TESTID : { "callcreates" : CCREATES } => check "callcreates" : CCREATES ~> failure TESTID
 // ----------------------------------------------------------------------------------------------------
    rule check "callcreates" : { ("data" : (DATA:String)) , ("destination" : (ACCTTO:String)) , ("gasLimit" : (GLIMIT:String)) , ("value" : (VAL:String)) , .JSONList }
      => check "callcreates" : { #parseAddr(ACCTTO) | 0 | #parseWord(VAL) | #parseByteStack(DATA) }
    rule <k> check "callcreates" : C:Call => . ... </k> <callLog> CL </callLog> requires C in CL
    rule <callLog> SetItem({ _ | GLIMIT => 0 | _ | _ }) ... </callLog> requires GLIMIT =/=Int 0

    rule check TESTID : { "blockHeader" : BLOCKHEADER } => check "blockHeader" : BLOCKHEADER ~> failure TESTID
 // ----------------------------------------------------------------------------------------------------------
    rule check "blockHeader" : { KEY : VALUE , REST } => check "blockHeader" : { KEY : VALUE } ~> check "blockHeader" : { REST } requires REST =/=K .JSONList

    rule check "blockHeader" : { KEY : (VALUE:String => #parseByteStack(VALUE)) }

    rule check "blockHeader" : { KEY : (VALUE:WordStack => #asUnsigned(VALUE)) }
      requires KEY in ( SetItem("coinbase") SetItem("difficulty") SetItem("gasLimit") SetItem("gasUsed")
                        SetItem("mixHash") SetItem("nonce") SetItem("number") SetItem("parentHash")
                        SetItem("receiptTrie") SetItem("stateRoot") SetItem("timestamp")
                        SetItem("transactionsTrie") SetItem("uncleHash")
                      )

    rule <k> check "blockHeader" : { "bloom"            : VALUE } => . ... </k> <logsBloom>        VALUE </logsBloom>
    rule <k> check "blockHeader" : { "coinbase"         : VALUE } => . ... </k> <coinbase>         VALUE </coinbase>
    rule <k> check "blockHeader" : { "difficulty"       : VALUE } => . ... </k> <difficulty>       VALUE </difficulty>
    rule <k> check "blockHeader" : { "extraData"        : VALUE } => . ... </k> <extraData>        VALUE </extraData>
    rule <k> check "blockHeader" : { "gasLimit"         : VALUE } => . ... </k> <gasLimit>         VALUE </gasLimit>
    rule <k> check "blockHeader" : { "gasUsed"          : VALUE } => . ... </k> <gasUsed>          VALUE </gasUsed>
    rule <k> check "blockHeader" : { "mixHash"          : VALUE } => . ... </k> <mixHash>          VALUE </mixHash>
    rule <k> check "blockHeader" : { "nonce"            : VALUE } => . ... </k> <blockNonce>       VALUE </blockNonce>
    rule <k> check "blockHeader" : { "number"           : VALUE } => . ... </k> <number>           VALUE </number>
    rule <k> check "blockHeader" : { "parentHash"       : VALUE } => . ... </k> <previousHash>     VALUE </previousHash>
    rule <k> check "blockHeader" : { "receiptTrie"      : VALUE } => . ... </k> <receiptsRoot>     VALUE </receiptsRoot>
    rule <k> check "blockHeader" : { "stateRoot"        : VALUE } => . ... </k> <stateRoot>        VALUE </stateRoot>
    rule <k> check "blockHeader" : { "timestamp"        : VALUE } => . ... </k> <timestamp>        VALUE </timestamp>
    rule <k> check "blockHeader" : { "transactionsTrie" : VALUE } => . ... </k> <transactionsRoot> VALUE </transactionsRoot>
    rule <k> check "blockHeader" : { "uncleHash"        : VALUE } => . ... </k> <ommersHash>       VALUE </ommersHash>

    rule <k> check "blockHeader" : { "hash": HASH:WordStack } => . ...</k>
         <previousHash>     HP </previousHash>
         <ommersHash>       HO </ommersHash>
         <coinbase>         HC </coinbase>
         <stateRoot>        HR </stateRoot>
         <transactionsRoot> HT </transactionsRoot>
         <receiptsRoot>     HE </receiptsRoot>
         <logsBloom>        HB </logsBloom>
         <difficulty>       HD </difficulty>
         <number>           HI </number>
         <gasLimit>         HL </gasLimit>
         <gasUsed>          HG </gasUsed>
         <timestamp>        HS </timestamp>
         <extraData>        HX </extraData>
         <mixHash>          HM </mixHash>
         <blockNonce>       HN </blockNonce>
      requires #blockHeaderHash(HP, HO, HC, HR, HT, HE, HB, HD, HI, HL, HG, HS, HX, HM, HN) ==Int #asUnsigned(HASH)

    rule check TESTID : { "genesisBlockHeader" : BLOCKHEADER } => check "genesisBlockHeader" : BLOCKHEADER ~> failure TESTID
 // ------------------------------------------------------------------------------------------------------------------------
    rule check "genesisBlockHeader" : { KEY : VALUE , REST } => check "genesisBlockHeader" : { KEY : VALUE } ~> check "genesisBlockHeader" : { REST } requires REST =/=K .JSONList
    rule check "genesisBlockHeader" : { KEY : VALUE } => .K requires KEY =/=String "hash"

    rule check "genesisBlockHeader" : { "hash": (HASH:String => #asUnsigned(#parseByteStack(HASH))) }
    rule <k> check "genesisBlockHeader" : { "hash": HASH } => . ... </k>
         <blockhash> ... ListItem(HASH) ListItem(_) </blockhash>

    rule check TESTID : { "transactions" : TRANSACTIONS } => check "transactions" : TRANSACTIONS ~> failure TESTID
 // --------------------------------------------------------------------------------------------------------------
    rule <k> check "transactions" : [ .JSONList ] => . ... </k> <txOrder> .List </txOrder>
    rule check "transactions" : [ TRANSACTION , REST ] => check "transactions" : TRANSACTION ~> check "transactions" : [ REST ]
    rule check "transactions" : { KEY : VALUE , REST } => check "transactions" : (KEY : VALUE) ~> check "transactions" : { REST }
    rule <k> check "transactions" : { .JSONList } => . ... </k> <txOrder> ListItem(_) => .List ... </txOrder>

    rule check "transactions" : (KEY : (VALUE:String => #parseByteStack(VALUE)))

    rule check "transactions" : (KEY : (VALUE:WordStack => #padToWidth(32, VALUE))) requires (KEY ==String "r" orBool KEY ==String "s") andBool #sizeWordStack(VALUE) <Int 32

    rule check "transactions" : ("to" : (VALUE:WordStack => #asAccount(VALUE)))

    rule check "transactions" : (KEY : (VALUE:WordStack => #asUnsigned(VALUE)))
      requires KEY in ( SetItem("gasLimit") SetItem("gasPrice") SetItem("nonce")
                        SetItem("v") SetItem("value")
                      )

    rule <k> check "transactions" : ("data"     : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <data>       VALUE </data>       ... </message>
    rule <k> check "transactions" : ("gasLimit" : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <txGasLimit> VALUE </txGasLimit> ... </message>
    rule <k> check "transactions" : ("gasPrice" : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <txGasPrice> VALUE </txGasPrice> ... </message>
    rule <k> check "transactions" : ("nonce"    : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <txNonce>    VALUE </txNonce>    ... </message>
    rule <k> check "transactions" : ("r"        : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <r>          VALUE </r>          ... </message>
    rule <k> check "transactions" : ("s"        : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <s>          VALUE </s>          ... </message>
    rule <k> check "transactions" : ("to"       : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <to>         VALUE </to>         ... </message>
    rule <k> check "transactions" : ("v"        : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <v>          VALUE </v>          ... </message>
    rule <k> check "transactions" : ("value"    : VALUE) => . ... </k> <txOrder> ListItem(TXID) ... </txOrder> <message> <msgID> TXID </msgID> <value>      VALUE </value>      ... </message>
```

TODO: case with nonzero ommers.

```{.k .uiuck .rvk}
    rule check TESTID : { "uncleHeaders" : OMMERS } => check "ommerHeaders" : OMMERS ~> failure TESTID
 // --------------------------------------------------------------------------------------------------
    rule <k> check "ommerHeaders" : [ .JSONList ] => . ... </k> <ommerBlockHeaders> [ .JSONList ] </ommerBlockHeaders>
endmodule
```
