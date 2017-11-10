Ethereum Backwards-Compatibility Testing
========================================

Here we test the Ethereum test suite against the new IELE VM.
Actual execution of IELE is defined in [the IELE file](iele.md).

```{.k .uiuck}
requires "verification.k"
```

```{.k .uiuck .rvk}
requires "iele.k"

module ETHEREUM-SIMULATION
    imports IELE
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
To do so, we'll extend sort `JSON` with some IELE specific syntax, and provide a "pretti-fication" to the nicer input form.

```{.k .uiuck .rvk}
    syntax JSON ::= Int | WordStack | Ops | Map | SubstateLogEntry | Account
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
    rule <mode> NORMAL     </mode> <k> start => #load #regRange(#sizeRegs(VALUES)) VALUES ~> #execute    ... </k> <callData> VALUES </callData> <fid> _ => 1 </fid>
    rule <mode> VMTESTS    </mode> <k> start => #load #regRange(#sizeRegs(VALUES)) VALUES ~> #execute    ... </k> <callData> VALUES </callData> <fid> _ => 1 </fid>
 // rule <mode> GASANALYZE </mode> <k> start => #load #regRange(#sizeRegs(VALUES)) VALUES ~> #gasAnalyze ... </k> <callData> VALUES </callData> <fid> _ => 1 </fid>

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
          => #create ACCTFROM #newAddr(ACCTFROM, NONCE) (GLIMIT -Int G0(SCHED, CODE, true)) VALUE #dasmOps(CODE) #sizeWordStack(CODE) .Regs
          ~> #execute ~> #codeDeposit #newAddr(ACCTFROM, NONCE) #sizeWordStack(CODE) #dasmOps(CODE) %0 ~> #adjustGas ~> #finalizeTx(false) ~> startTx
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
          => #call ACCTFROM ACCTTO ACCTTO "deposit" (GLIMIT -Int G0(SCHED, DATA, false)) VALUE VALUE (#sizeWordStack(DATA) #asUnsigned(DATA) .Regs) false
          ~> #execute ~> #finishTx ~> #adjustGas ~> #finalizeTx(false) ~> startTx
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

    syntax EthereumCommand ::= "#adjustGas"
 // ---------------------------------------
    rule <k> #adjustGas => . ... </k>
         <gas> _ => GLIMIT -Int GUSED </gas>
         <gasUsed> GUSED </gasUsed>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID> TXID </msgID>
           <txGasLimit> GLIMIT </txGasLimit>
           ...
         </message>

    syntax EthereumCommand ::= "#finishTx"
 // --------------------------------------
    rule <k> #exception ~> #finishTx => #popCallStack ~> #popWorldState ~> #popSubstate ... </k>
    rule <k> #revert    ~> #finishTx => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ... </k> <gas> GAVAIL </gas>       

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
    rule <k> clear => . ... </k>
         <analysis> _ => .Map </analysis>
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
           <code> _ => #dasmOps(CODE) </code>
           <codeSize> _ => #sizeWordStack(CODE) </codeSize>
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
    rule load "exec" : { "data" : ((DATA:WordStack) => [#asUnsigned(DATA), #sizeWordStack(DATA)]) } 
 // -----------------------------------------------------------------------------------------------
    rule <k> load "exec" : { "data" : [DATA:Int, LEN:Int] } => . ... </k> <callData> _ => LEN DATA .Regs </callData>
    rule <k> load "exec" : { "code" : (CODE:WordStack) } => . ... </k>
         (<program>  _ </program> => #loadCode(#dasmOps(CODE), #sizeWordStack(CODE)))
         <schedule> SCHED </schedule>
```

The `"network"` key allows setting the fee schedule inside the test.

```{.k .uiuck .rvk}
    rule <k> load "network" : SCHEDSTRING => . ... </k>
         <schedule> _ => #asScheduleString(SCHEDSTRING) </schedule>

    syntax Schedule ::= #asScheduleString ( String ) [function]
 // -----------------------------------------------------------
    rule #asScheduleString("Byzantium")      => ALBE
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
           <code> OPS </code>
           <codeSize> SIZE </codeSize>
           ...
         </account>
         requires #dasmOps(CODE) ==K OPS andBool #sizeWordStack(CODE) ==Int SIZE

    syntax Map ::= #adjustStorageValues(Map) [function]
 // ---------------------------------------------------
    rule #adjustStorageValues(K |-> V M) => K |-> chop(V) #adjustStorageValues(M)
    rule #adjustStorageValues(.Map) => .Map
```

Here we check the other post-conditions associated with an EVM test.

```{.k .uiuck .rvk}
    rule check TESTID : { "out" : OUT } => check "out" : OUT ~> failure TESTID
 // --------------------------------------------------------------------------
    rule check "out" : ((OUT:String) => #parseHexWord(OUT))
    rule <k> check "out" : OUT => . ... </k> <output> OUT .Regs </output>
    rule <k> check "out" : 0   => . ... </k> <output> .Regs </output>

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
      => .

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
```
Disassembler
------------

After interpreting the strings representing programs as a `WordStack`, it should be changed into an `Ops` for use by the IELE semantics.

-   `#dasmOps` interperets `WordStack` as an `Ops`.
-   `#dasmOpCode` interperets a `Int` as an `OpCode`.
-   `#computeJumpTable` fills in the jump table from a `Map` of `OpCode`.

```{.k .uiuck .rvk}

    syntax Ops ::= #dasmOps ( WordStack )       [function]
                 | #dasmOps ( Ops , WordStack , K , Int ) [function, klabel(#dasmOpsAux)]
 // -----------------------------------------------------------------------------
    rule #dasmOps( 99 : NBITS : WS ) => #revOps(#dasmOps(REGISTERS(NBITS) ; .Ops, WS, .K, NBITS), .Ops)

    rule #dasmOps( OPS, .WordStack, .K, _ ) => OPS

    rule #dasmOps( OPS, W : WS, .K, NREGS ) => #dasmOps(OPS, WS, #dasmOpCode(W), NREGS)
      requires (W >=Int 0   andBool W <=Int 96)
        orBool (W >=Int 106 andBool W <=Int 239)
        orBool (W >=Int 249 andBool W <=Int 255)

    rule #dasmOps( OPS, W : WS, .K,             NREGS ) => #dasmOps(OPS, W : WS, #str(#pushLen(#drop(NREGS up/Int 8, WS)), #pushOffset(#drop(NREGS up/Int 8, WS))), NREGS)
      requires W >=Int 97 andBool W <Int 99
    rule #dasmOps( OPS, 97  : WS, #str(LEN, POS), NREGS ) => #dasmOps(OPS, WS, LOADPOS(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])), NREGS)
    rule #dasmOps( OPS, 98  : WS, #str(LEN, POS), NREGS ) => #dasmOps(OPS, WS, LOADNEG(LEN +Int POS, #asUnsigned(WS [ POS +Int (NREGS up/Int 8) .. LEN ])), NREGS)
    rule #dasmOps( OPS, 105 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, #drop(W1 *Int 256 +Int W2, WS), FUNCTION(#unparseByteStack(#take(W1 *Int 256 +Int W2, WS))), NREGS)

    rule #dasmOps( OPS, 100 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, JUMP(W1 *Int 256 +Int W2),      NREGS)
    rule #dasmOps( OPS, 101 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, JUMPI(W1 *Int 256 +Int W2),     NREGS)
    rule #dasmOps( OPS, 102 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, JUMPDEST(W1 *Int 256 +Int W2),  NREGS)
    rule #dasmOps( OPS, 103 : W1 : W2 : W3 : W4 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CALLDEST(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4), NREGS)
    rule #dasmOps( OPS, 104 : W1 : W2 : W3 : W4 : WS, .K, NREGS ) => #dasmOps(OPS, WS, EXTCALLDEST(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4), NREGS)
    rule #dasmOps( OPS, 240 : W1 : W2 : W3 : W4 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CREATE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4), NREGS)
    rule #dasmOps( OPS, 241 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, COPYCREATE(W1 *Int 256 +Int W2), NREGS)
    rule #dasmOps( OPS, 242 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 243 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, CALLCODE(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 244 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, DELEGATECALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 245 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, STATICCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)
    rule #dasmOps( OPS, 246 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, RETURN(W1 *Int 256 +Int W2), NREGS)
    rule #dasmOps( OPS, 247 : W1 : W2 : WS, .K, NREGS ) => #dasmOps(OPS, WS, REVERT(W1 *Int 256 +Int W2), NREGS)
    rule #dasmOps( OPS, 248 : W1 : W2 : W3 : W4 : W5 : W6 : WS, .K, NREGS ) => #dasmOps(OPS, WS, LOCALCALL(W1 *Int 256 +Int W2, W3 *Int 256 +Int W4, W5 *Int 256 +Int W6), NREGS)

    rule #dasmOps( OPS, WS, OP:OpCode, NREGS) => #dasmOps(#dasmRegs(OP, WS, NREGS) ; OPS, #drop(#opWidth(OP, NREGS) -Int #opCodeWidth(OP), WS), .K, NREGS)

    syntax Op ::= #dasmRegs ( OpCode , WordStack , Int ) [function]
                | #dasmRegs ( OpCode , Int , Int , Int ) [function, klabel(#dasmRegsAux)]
 // -------------------------------------------------------------------------------------
    rule #dasmRegs ( LOADPOS(N, W), WS, NREGS ) => #dasmRegs(LOADPOS(N, W), #asUnsigned(#take(NREGS up/Int 8, WS)),                            NREGS, (1 <<Int NREGS) -Int 1)
    rule #dasmRegs ( LOADNEG(N, W), WS, NREGS ) => #dasmRegs(LOADNEG(N, W), #asUnsigned(#take(NREGS up/Int 8, WS)),                            NREGS, (1 <<Int NREGS) -Int 1)
    rule #dasmRegs ( OP,            WS, NREGS ) => #dasmRegs(OP,            #asUnsigned(#take(#opWidth(OP, NREGS) -Int #opCodeWidth(OP), WS)), NREGS, (1 <<Int NREGS) -Int 1) [owise]

    rule #dasmRegs ( OP:NullOp,     R, W, M ) => OP %(R, W, M, 0)
    rule #dasmRegs ( OP:NullVoidOp, R, W, M ) => OP
    rule #dasmRegs ( OP:HeaderOp,   R, W, M ) => OP
    rule #dasmRegs ( OP:UnOp,       R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1)
    rule #dasmRegs ( OP:UnVoidOp,   R, W, M ) => OP %(R, W, M, 0)
    rule #dasmRegs ( OP:BinOp,      R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2)
    rule #dasmRegs ( OP:BinVoidOp,  R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1)
    rule #dasmRegs ( OP:TernOp,     R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3)
    rule #dasmRegs ( OP:TernVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2)
    rule #dasmRegs ( OP:QuadVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3)
    rule #dasmRegs ( OP:FiveVoidOp, R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3) %(R, W, M, 4)
    rule #dasmRegs ( OP:SixVoidOp,  R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1) %(R, W, M, 2) %(R, W, M, 3) %(R, W, M, 4) %(R, W, M, 5)

    rule #dasmRegs ( _:CallSixOpCode (_, ARGS, RETS) #as OP::CallSixOp,   R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1 +Int RETS) %(R, W, M, 2 +Int RETS)                         %(R, W, M, 1, RETS) %(R, W, M, RETS +Int 3, ARGS)
    rule #dasmRegs ( _:CallOpCode    (_, ARGS, RETS) #as OP::CallOp,      R, W, M ) => OP %(R, W, M, 0) %(R, W, M, 1 +Int RETS) %(R, W, M, 2 +Int RETS) %(R, W, M, 3 +Int RETS) %(R, W, M, 1, RETS) %(R, W, M, RETS +Int 4, ARGS)
    rule #dasmRegs ( LOCALCALL       (_, ARGS, RETS) #as OP::LocalCallOp, R, W, M ) => OP                                                                                       %(R, W, M, 0, RETS) %(R, W, M, RETS,        ARGS)

    rule #dasmRegs ( REVERT(RETS), R, W, M ) => REVERT(RETS) %(R, W, M, 0, RETS)
    rule #dasmRegs ( RETURN(RETS), R, W, M ) => RETURN(RETS) %(R, W, M, 0, RETS)

    syntax Reg ::= "%" "(" Int "," Int "," Int "," Int ")" [function]
 // -----------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX) => % ((REGS >>Int (IDX *Int WIDTH)) &Int MASK)

    syntax Regs ::= "%" "(" Int "," Int "," Int "," Int "," Int ")" [function]
 // --------------------------------------------------------------------------
    rule %(REGS, WIDTH, MASK, IDX, 0) => .Regs
    rule %(REGS, WIDTH, MASK, IDX, COUNT) => %(REGS, WIDTH, MASK, IDX +Int COUNT -Int 1) %(REGS, WIDTH, MASK, IDX, COUNT -Int 1) [owise]

    syntax Int ::= #opWidth ( OpCode , Int ) [function]
 // ---------------------------------------------------
    rule #opWidth ( LOADPOS(N, _), NREGS ) => 1 +Int N +Int (NREGS up/Int 8)
    rule #opWidth ( LOADNEG(N, _), NREGS ) => 1 +Int N +Int (NREGS up/Int 8)
    rule #opWidth ( OP, NREGS ) => #opCodeWidth(OP) +Int ((NREGS *Int #numArgs(OP)) up/Int 8) [owise]

    syntax Int ::= #opCodeWidth ( OpCode ) [function]
 // -------------------------------------------------
    rule #opCodeWidth( JUMPDEST(_) )      => 3
    rule #opCodeWidth( JUMP(_) )          => 3
    rule #opCodeWidth( JUMPI(_) )         => 3
    rule #opCodeWidth( LOCALCALL(_,_,_) ) => 7
    rule #opCodeWidth( RETURN(_) )        => 3
    rule #opCodeWidth( REVERT(_) )        => 3
    rule #opCodeWidth( _:CallOp )         => 5
    rule #opCodeWidth( _:CallSixOp )      => 5
    rule #opCodeWidth( OP )               => 1 [owise]

    syntax Int ::= #numArgs ( OpCode ) [function]
 // ---------------------------------------------
    rule #numArgs ( _:NullOp )     => 1
    rule #numArgs ( _:NullVoidOp ) => 0
    rule #numArgs ( _:HeaderOp )   => 0
    rule #numArgs ( _:UnOp )       => 2
    rule #numArgs ( _:UnVoidOp )   => 1
    rule #numArgs ( _:BinOp )      => 3
    rule #numArgs ( _:BinVoidOp )  => 2
    rule #numArgs ( _:TernOp )     => 4
    rule #numArgs ( _:TernVoidOp ) => 3
    rule #numArgs ( _:QuadVoidOp ) => 4
    rule #numArgs ( _:FiveVoidOp ) => 5
    rule #numArgs ( _:SixVoidOp )  => 6
    rule #numArgs ( _:CallSixOpCode(_, ARGS, RETS) ) => 3 +Int ARGS +Int RETS
    rule #numArgs ( _:CallOpCode   (_, ARGS, RETS) ) => 4 +Int ARGS +Int RETS
    rule #numArgs ( LOCALCALL      (_, ARGS, RETS) ) => ARGS +Int RETS
    rule #numArgs ( RETURN(RETS) )                => RETS
    rule #numArgs ( REVERT(RETS) )                => RETS

    syntax OpCode ::= #dasmOpCode ( Int) [function]
 // -----------------------------------------------------------
    rule #dasmOpCode(   0 ) => STOP
    rule #dasmOpCode(   1 ) => ADD
    rule #dasmOpCode(   2 ) => MUL
    rule #dasmOpCode(   3 ) => SUB
    rule #dasmOpCode(   4 ) => DIV
    rule #dasmOpCode(   6 ) => MOD
    rule #dasmOpCode(   7 ) => EXP
    rule #dasmOpCode(   8 ) => ADDMOD
    rule #dasmOpCode(   9 ) => MULMOD
    rule #dasmOpCode(  10 ) => EXPMOD
    rule #dasmOpCode(  11 ) => SIGNEXTEND
    rule #dasmOpCode(  12 ) => TWOS
    rule #dasmOpCode(  16 ) => LT
    rule #dasmOpCode(  17 ) => GT
    rule #dasmOpCode(  20 ) => EQ
    rule #dasmOpCode(  21 ) => ISZERO
    rule #dasmOpCode(  22 ) => AND
    rule #dasmOpCode(  23 ) => OR
    rule #dasmOpCode(  24 ) => XOR
    rule #dasmOpCode(  25 ) => NOT
    rule #dasmOpCode(  26 ) => BYTE
    rule #dasmOpCode(  32 ) => SHA3
    rule #dasmOpCode(  48 ) => ADDRESS
    rule #dasmOpCode(  49 ) => BALANCE
    rule #dasmOpCode(  50 ) => ORIGIN
    rule #dasmOpCode(  51 ) => CALLER
    rule #dasmOpCode(  52 ) => CALLVALUE
    rule #dasmOpCode(  56 ) => CODESIZE
    rule #dasmOpCode(  58 ) => GASPRICE
    rule #dasmOpCode(  59 ) => EXTCODESIZE
    rule #dasmOpCode(  64 ) => BLOCKHASH
    rule #dasmOpCode(  65 ) => COINBASE
    rule #dasmOpCode(  66 ) => TIMESTAMP
    rule #dasmOpCode(  67 ) => NUMBER
    rule #dasmOpCode(  68 ) => DIFFICULTY
    rule #dasmOpCode(  69 ) => GASLIMIT
    rule #dasmOpCode(  80 ) => MLOAD8
    rule #dasmOpCode(  81 ) => MLOAD256
    rule #dasmOpCode(  82 ) => MLOAD
    rule #dasmOpCode(  83 ) => MSTORE8
    rule #dasmOpCode(  84 ) => MSTORE256
    rule #dasmOpCode(  85 ) => MSTORE
    rule #dasmOpCode(  86 ) => SLOAD
    rule #dasmOpCode(  87 ) => SSTORE
    rule #dasmOpCode(  89 ) => MSIZE
    rule #dasmOpCode(  90 ) => GAS
    rule #dasmOpCode(  96 ) => MOVE
    rule #dasmOpCode( 160 ) => LOG0
    rule #dasmOpCode( 161 ) => LOG1
    rule #dasmOpCode( 162 ) => LOG2
    rule #dasmOpCode( 163 ) => LOG3
    rule #dasmOpCode( 164 ) => LOG4
    rule #dasmOpCode( 255 ) => SELFDESTRUCT
    rule #dasmOpCode(   W ) => INVALID [owise]


endmodule
```
