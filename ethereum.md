Ethereum Backwards-Compatibility Testing
========================================

Here we test the Ethereum test suite against the new IELE VM.
Actual execution of IELE is defined in [the IELE file](iele.md).

```{.k .uiuck .rvk .standalone .node}
requires "iele.k"
requires "iele-binary.k"

module ETHEREUM-SIMULATION
    imports IELE
    imports IELE-BINARY
```

```{.k .uiuck}
    imports VERIFICATION
```

```{.k .rvk .standalone .node}
    imports K-REFLECTION
```

A IELE simulation is a list of IELE commands.
Some IELE commands take a specification of IELE state (eg. for an account or transaction).

```{.k .uiuck .rvk .standalone .node}
    syntax IELESimulation ::= ".IELESimulation"
                                | IELECommand IELESimulation
 // --------------------------------------------------------
    rule .IELESimulation => .
    rule IEC:IELECommand IES:IELESimulation => IEC ~> IES

    syntax IELESimulation ::= JSON
 // ------------------------------
    rule <k> JSONINPUT:JSON => run JSONINPUT success .IELESimulation </k>
```

For verification purposes, it's much easier to specify a program in terms of its op-codes and not the hex-encoding that the tests use.
To do so, we'll extend sort `JSON` with some IELE specific syntax, and provide a "pretti-fication" to the nicer input form.

```{.k .uiuck .rvk .standalone .node}
    syntax JSON ::= Int | WordStack | Map | SubstateLogEntry | Account
 // ------------------------------------------------------------------

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

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "start"
 // ------------------------------
    rule <mode> NORMAL     </mode> <k> start => #loads #regRange(#sizeRegs(VALUES)) VALUES ~> #execute    ... </k> <callData> VALUES </callData> <fid> _ => deposit </fid>
    rule <mode> VMTESTS    </mode> <k> start => #loads #regRange(#sizeRegs(VALUES)) VALUES ~> #execute    ... </k> <callData> VALUES </callData> <fid> _ => deposit </fid>
 // rule <mode> GASANALYZE </mode> <k> start => #loads #regRange(#sizeRegs(VALUES)) VALUES ~> #gasAnalyze ... </k> <callData> VALUES </callData> <fid> _ => deposit </fid>

    syntax IELECommand ::= "flush"
 // ------------------------------
    rule <k> #end       ~> flush => #finalizeTx(false)               ... </k>
    rule <k> #exception CODE ~> flush => #finalizeTx(false) ~> #exception CODE ... </k>
```

-   `startTx` computes the sender of the transaction, and places loadTx on the `k` cell.
-   `loadTx(_)` loads the next transaction to be executed into the current state.
-   `#adjustGas` fakes the gas usage of the transaction since the EVM-to-IELE conversion does not preserve gas usage.
-   `finishTx` is a place-holder for performing necessary cleanup after a transaction.

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "startTx"
 // --------------------------------
    rule <k> startTx => #finalizeBlock ... </k>
         <txPending> .List </txPending>

    rule <k> startTx => loadTx(#sender(TN, TP, TG, TT, TV, #unparseByteStack(DATA), TW, TR, TS)) ... </k>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID>      TXID </msgID>
           <txNonce>    TN   </txNonce>
           <txGasPrice> TP   </txGasPrice>
           <txGasLimit> TG   </txGasLimit>
           <sendto>     TT   </sendto>
           <value>      TV   </value>
           <v>          TW   </v>
           <r>          TR   </r>
           <s>          TS   </s>
           <data>       DATA </data>
           ...
         </message>

    syntax IELECommand ::= loadTx ( Int )
 // -------------------------------------
    rule <k> loadTx(ACCTFROM)
          => #create ACCTFROM #newAddr(ACCTFROM, NONCE) (GLIMIT -Int G0(SCHED, CODE, ARGS, true)) VALUE #dasmContract(CODE, Main) ARGS
          ~> #codeDeposit #newAddr(ACCTFROM, NONCE) #sizeWordStack(CODE) #dasmContract(CODE, Main) %0 %1 true ~> #adjustGas ~> #finalizeTx(false) ~> startTx
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
           <sendto>     .Account </sendto>
           <value>      VALUE    </value>
           <data>       CODE     </data>
           <args>       ARGS     </args>
           ...
         </message>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL => BAL -Int (GLIMIT *Int GPRICE) </balance>
           <nonce> NONCE => NONCE +Int 1 </nonce>
           ...
         </account>

    rule <k> loadTx(ACCTFROM)
          => #call ACCTFROM ACCTTO FUNC (GLIMIT -Int G0(SCHED, .WordStack, ARGS, false)) VALUE ARGS false
          ~> #finishTx ~> #adjustGas ~> #finalizeTx(false) ~> startTx
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
           <sendto>     ACCTTO </sendto>
           <value>      VALUE  </value>
           <args>       ARGS   </args>
           <func>       FUNC   </func>
           ...
         </message>
         <account>
           <acctID> ACCTFROM </acctID>
           <balance> BAL => BAL -Int (GLIMIT *Int GPRICE) </balance>
           <nonce> NONCE => NONCE +Int 1 </nonce>
           ...
         </account>
      requires ACCTTO =/=K .Account

    syntax IELECommand ::= "#adjustGas"
 // -----------------------------------
    rule <k> #adjustGas => . ... </k>
         <checkGas> false </checkGas>
         <gas> _ => GLIMIT -Int GUSED </gas>
         <refund> _ => 0 </refund>
         <gasUsed> GUSED </gasUsed>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID> TXID </msgID>
           <txGasLimit> GLIMIT </txGasLimit>
           ...
         </message>

    rule <k> #adjustGas => . ... </k> <checkGas> true </checkGas>

    syntax IELECommand ::= "#finishTx"
 // ----------------------------------
    rule <k> #exception _ ~> #finishTx => #popCallStack ~> #popWorldState ~> #popSubstate ... </k>
    rule <k> #revert _    ~> #finishTx => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ... </k> <gas> GAVAIL </gas>       

    rule <k> #end ~> #finishTx => #popCallStack ~> #dropWorldState ~> #dropSubstate ~> #refund GAVAIL ... </k>
         <id> ACCT </id>
         <gas> GAVAIL </gas>
         <txPending> ListItem(TXID:Int) ... </txPending>
         <message>
           <msgID>  TXID </msgID>
           <sendto> TT   </sendto>
           ...
         </message>
      requires TT =/=K .Account
```

-   `#finalizeBlock` is used to signal that block finalization procedures should take place (after transactions have executed).
-   `#rewardOmmers(_)` pays out the reward to uncle blocks so that blocks are orphaned less often.

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "#finalizeBlock"
 // ---------------------------------------
    rule <k> #finalizeBlock => . ... </k>
         <schedule> SCHED </schedule>
         <beneficiary> MINER </beneficiary>
         <account>
           <acctID> MINER </acctID>
           <balance> MINBAL => MINBAL +Int Rb < SCHED > </balance>
           ...
         </account>

    rule <k> (.K => #newAccount MINER) ~> #finalizeBlock ... </k>
         <beneficiary> MINER </beneficiary>
         <activeAccounts> ACCTS </activeAccounts>
      requires notBool MINER in ACCTS
```

-   `exception` only clears from the `<k>` cell if there is an exception preceding it.
-   `failure_` holds the name of a test that failed if a test does fail.

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "exception" | "failure" String | "success"
 // -----------------------------------------------------------------
    rule <k> #exception _ ~> exception => . ... </k>
    rule <k> success => . ... </k> <exit-code> _ => 0 </exit-code>
    rule failure _ => .
```

### Running Tests

-   `run` runs a given set of Ethereum tests (from the test-set).

Note that `TEST` is sorted here so that key `"network"` comes before key `"pre"`.

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "run" JSON
 // ---------------------------------
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

```{.k .uiuck .rvk .standalone .node}
    syntax Set ::= "#loadKeys" [function]
 // -------------------------------------
    rule #loadKeys => ( SetItem("env") SetItem("pre") SetItem("blockHeader") SetItem("transactions") SetItem("uncleHeaders") SetItem("network") SetItem("genesisRLP") SetItem("checkGas") )

    rule run TESTID : { KEY : (VAL:JSON) , REST } => load KEY : VAL ~> run TESTID : { REST } requires KEY in #loadKeys

    rule run TESTID : { "blocks" : [ { KEY : VAL , REST1 => REST1 }, .JSONList ] , ( REST2 => KEY : VAL , REST2 ) }
    rule run TESTID : { "blocks" : [ { .JSONList }, .JSONList ] , REST } => run TESTID : { REST }
```

-   `#execKeys` are all the JSON nodes which should be considered for execution (between loading and checking).

```{.k .uiuck .rvk .standalone .node}
    syntax Set ::= "#execKeys" [function]
 // -------------------------------------
    rule #execKeys => ( SetItem("exec") SetItem("lastblockhash") )

    rule run TESTID : { KEY : (VAL:JSON) , NEXT , REST } => run TESTID : { NEXT , KEY : VAL , REST } requires KEY in #execKeys

    rule run TESTID : { "exec" : (EXEC:JSON) } => load "exec" : EXEC ~> start ~> flush
    rule run TESTID : { "lastblockhash" : (HASH:String) } => startTx
```

-   `#postKeys` are a subset of `#checkKeys` which correspond to post-state account checks.
-   `#checkKeys` are all the JSON nodes which should be considered as checks after execution.

```{.k .uiuck .rvk .standalone .node}
    syntax Set ::= "#postKeys" [function] | "#allPostKeys" [function] | "#checkKeys" [function]
 // -------------------------------------------------------------------------------------------
    rule #postKeys    => ( SetItem("post") SetItem("postState") )
    rule #allPostKeys => ( #postKeys SetItem("expect") SetItem("export") SetItem("expet") )
    rule #checkKeys   => ( #allPostKeys SetItem("logs") SetItem("callcreates") SetItem("out") SetItem("gas")
                           SetItem("genesisBlockHeader")
                         )

    rule run TESTID : { KEY : (VAL:JSON) , REST } => run TESTID : { REST } ~> check TESTID : { "post" : VAL } requires KEY in #allPostKeys
    rule run TESTID : { KEY : (VAL:JSON) , REST } => run TESTID : { REST } ~> check TESTID : { KEY    : VAL } requires KEY in #checkKeys andBool notBool KEY in #allPostKeys
```

-   `#discardKeys` are all the JSON nodes in the tests which should just be ignored.

```{.k .uiuck .rvk .standalone .node}
    syntax Set ::= "#discardKeys" [function]
 // ----------------------------------------
    rule #discardKeys => ( SetItem("//") SetItem("_info") SetItem("rlp") )

    rule run TESTID : { KEY : _ , REST } => run TESTID : { REST } requires KEY in #discardKeys
```

State Manipulation
------------------

### Clearing State

-   `clear` clears all the execution state of the machine.

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "clear"
 // ------------------------------
    rule <k> clear => . ... </k>
         <schedule> _ => DEFAULT </schedule>
         (<iele> _ </iele> => <iele> ... .Bag </iele>)

```

### Loading State

-   `mkAcct_` creates an account with the supplied ID (assuming it's already been chopped to 160 bits).

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "mkAcct" Int
 // -----------------------------------
    rule <k> mkAcct ACCT => #newAccount ACCT ... </k>
```

-   `load` loads an account or transaction into the world state.

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "load" JSON
 // ----------------------------------
    rule load DATA : { .JSONList } => .
    rule load DATA : { KEY : VALUE , REST } => load DATA : { KEY : VALUE } ~> load DATA : { REST }
      requires REST =/=K .JSONList andBool DATA =/=String "transactions"

    rule load DATA : [ .JSONList ] => .
    rule load DATA : [ { TEST } , REST ] => load DATA : { TEST } ~> load DATA : [ REST ]
```

Here we perform pre-proccesing on account data which allows "pretty" specification of input.

```{.k .uiuck .rvk .standalone .node}
    rule load "pre" : { (ACCTID:String) : ACCT } => mkAcct #parseAddr(ACCTID) ~> load "account" : { ACCTID : ACCT }
    rule load "account" : { ACCTID: { KEY : VALUE , REST } } => load "account" : { ACCTID : { KEY : VALUE } } ~> load "account" : { ACCTID : { REST } } requires REST =/=K .JSONList

    rule load "account" : { ((ACCTID:String) => #parseAddr(ACCTID)) : ACCT }
    rule load "account" : { (ACCT:Int) : { "balance" : ((VAL:String)         => #parseWord(VAL)) } }
    rule load "account" : { (ACCT:Int) : { "nonce"   : ((VAL:String)         => #parseWord(VAL)) } }
    rule load "account" : { (ACCT:Int) : { "code"    : ((CODE:String)        => #parseByteStack(CODE)) } }
    rule load "account" : { (ACCT:Int) : { "storage" : ({ STORAGE:JSONList } => #parseMap({ STORAGE })) } }

```

The individual fields of the accounts are dealt with here.

```{.k .uiuck .rvk .standalone .node}
    rule <k> load "account" : { ACCT : { "balance" : (BAL:Int) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <balance> _ => BAL </balance>
           ...
         </account>

    rule <k> load "account" : { ACCT : { "code" : (CODE:WordStack) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> _ => #dasmContract(CODE, Main) </code>
           ...
         </account>

    rule <k> load "account" : { ACCT : { "nonce" : (NONCE:Int) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <nonce> _ => NONCE </nonce>
           ...
         </account>

    rule <k> load "account" : { ACCT : { "storage" : (STORAGE:Map) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <storage> _ => STORAGE </storage>
           ...
         </account>
```

Here we load the environmental information.

```{.k .uiuck .rvk .standalone .node}
    rule load "env" : { KEY : ((VAL:String) => #parseWord(VAL)) }
      requires KEY in (SetItem("currentTimestamp") SetItem("currentGasLimit") SetItem("currentNumber") SetItem("currentDifficulty"))
    rule load "env" : { KEY : ((VAL:String) => #parseHexWord(VAL)) }
      requires KEY in (SetItem("currentCoinbase") SetItem("previousHash"))
 // ----------------------------------------------------------------------
    rule <k> load "env" : { "currentCoinbase"   : (CB:Int)     } => . ... </k> <beneficiary>     _ => CB     </beneficiary>
    rule <k> load "env" : { "currentDifficulty" : (DIFF:Int)   } => . ... </k> <difficulty>   _ => DIFF   </difficulty>
    rule <k> load "env" : { "currentGasLimit"   : (GLIMIT:Int) } => . ... </k> <gasLimit>     _ => GLIMIT </gasLimit>
    rule <k> load "env" : { "currentNumber"     : (NUM:Int)    } => . ... </k> <number>       _ => NUM    </number>
    rule <k> load "env" : { "previousHash"      : (HASH:Int)   } => . ... </k>
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
    rule <k> load "exec" : { "data" : [DATA:Int, LEN:Int] } => . ... </k> <callData> _ => LEN , DATA , .Ints </callData>
    rule <k> load "exec" : { "code" : (CODE:WordStack) } => . ... </k>
         (<program>  _ </program> => #loadCode(#dasmContract(CODE, Main)))
         <schedule> SCHED </schedule>
```

The `"network"` key allows setting the fee schedule inside the test.
Since IELE is a new language with no hard forks yet, we only support the latest EVM gas schedule.

```{.k .uiuck .rvk .standalone .node}
    rule <k> load "network" : SCHEDSTRING => . ... </k>
         <schedule> _ => #asScheduleString(SCHEDSTRING) </schedule>

    syntax Schedule ::= #asScheduleString ( String ) [function]
 // -----------------------------------------------------------
    rule #asScheduleString("Albe")      => ALBE

    rule <k> load "checkGas" : CHECKGAS => . ... </k>
         <checkGas> _ => CHECKGAS </checkGas>
```

The `"blockHeader"` key loads the block information.

```{.k .uiuck .rvk .standalone .node}
    rule load "blockHeader" : { "nonce" : (HN:String) } => .
    rule load "blockHeader" : { "receiptTrie" : (HE:String) } => .
    rule load "blockHeader" : { "hash" : _ } => .
    rule load "blockHeader" : { "uncleHash" : (HO:String) } => .
    rule load "blockHeader" : { "mixHash" : (HM:String) } => .
    rule load "blockHeader" : { "parentHash" : (HP:String) } => .
    rule load "blockHeader" : { "extraData" : (HX:String) } => .
    rule load "blockHeader" : { "stateRoot" : (HR:String) } => .
    rule load "blockHeader" : { "transactionsTrie" : (HT:String) } => .
    rule load "blockHeader" : { "bloom" : (HB:String) } => .

    rule <k> load "blockHeader" : { "gasLimit" : (HL:String) } => . ...</k> 
         <gasLimit> _ => #asUnsigned(#parseByteStack(HL)) </gasLimit>

    rule <k> load "blockHeader" : { "number" : (HI:String) } => . ...</k> 
         <number> _ => #asUnsigned(#parseByteStack(HI)) </number>

    rule <k> load "blockHeader" : { "difficulty" : (HD:String) } => . ...</k> 
         <difficulty> _ => #asUnsigned(#parseByteStack(HD)) </difficulty>

    rule <k> load "blockHeader" : { "timestamp" : (HS:String) } => . ...</k> 
         <timestamp> _ => #asUnsigned(#parseByteStack(HS)) </timestamp>

    rule <k> load "blockHeader" : { "coinbase" : (HC:String) } => . ...</k> 
         <beneficiary> _ => #asUnsigned(#parseByteStack(HC)) </beneficiary>

    rule <k> load "blockHeader" : { "gasUsed" : (HG:String) } => . ...</k> 
         <gasUsed> _ => #asUnsigned(#parseByteStack(HG)) </gasUsed>

    rule load "genesisRLP" : (VAL:String => #rlpDecode(#unparseByteStack(#parseByteStack(VAL))))
 // --------------------------------------------------------------------------------------------
    rule <k> load "genesisRLP": [ [ HP, HO, HC, HR, HT, HE:String, HB, HD, HI, HL, HG, HS, HX, HM, HN, .JSONList ], _, _, .JSONList ] => .K ... </k>
         <blockhash> .List => ListItem(#blockHeaderHash(HP, HO, HC, HR, HT, HE, HB, HD, HI, HL, HG, HS, HX, HM, HN)) ListItem(#asUnsigned(#parseByteStackRaw(HP))) ... </blockhash>
```

The `"transactions"` key loads the transactions.

```{.k .uiuck .rvk .standalone .node}
    rule load "transactions" : { TX } => load "transactions" : { #sortJSONList(TX) }
         requires notBool #isSorted(TX)

    rule <k> load "transactions" : { "arguments" : [ ARGS ],  "data" : TI , "function" : FUNC, "gasLimit" : TG , "gasPrice" : TP , "nonce" : TN , "r" : TR , "s" : TS , "to" : TT , "v" : TW , "value" : TV , .JSONList } => . ... </k>
         <txOrder>   ... .List => ListItem(!ID) </txOrder>
         <txPending> ... .List => ListItem(!ID) </txPending>
         <messages>
           ( .Bag
          => <message>
               <msgID>      !ID:Int                              </msgID>
               <txNonce>    #asUnsigned(#parseByteStack(TN))     </txNonce>
               <txGasPrice> #asUnsigned(#parseByteStack(TP))     </txGasPrice>
               <txGasLimit> #asUnsigned(#parseByteStack(TG))     </txGasLimit>
               <sendto>     #asAccount(#parseByteStack(TT))      </sendto>
               <func>       {#parseToken("IeleName", FUNC)}:>IeleName        </func>
               <value>      #asUnsigned(#parseByteStack(TV))     </value>
               <v>          #asUnsigned(#parseByteStack(TW))     </v>
               <r>          #padToWidth(32, #parseByteStack(TR)) </r>
               <s>          #padToWidth(32, #parseByteStack(TS)) </s>
               <data>       #parseByteStack(TI)                  </data>
               <args>       #toInts(ARGS)                        </args>
             </message>
           )
           ...
         </messages>

    syntax Ints ::= #toInts ( JSONList ) [function]
 // -----------------------------------------------
    rule #toInts(.JSONList) => .Ints
    rule #toInts(WORD:String , ARGS) => #parseHexWord(WORD) , #toInts(ARGS)
```

### Checking State

-   `check_` checks if an account/transaction appears in the world-state as stated.

```{.k .uiuck .rvk .standalone .node}
    syntax IELECommand ::= "check" JSON
 // -----------------------------------
    rule #exception CODE ~> check J:JSON => check J ~> #exception CODE
    rule check DATA : { .JSONList } => . requires DATA =/=String "transactions"
    rule check DATA : { (KEY:String) : VALUE , REST } => check DATA : { KEY : VALUE } ~> check DATA : { REST }
      requires REST =/=K .JSONList andBool notBool DATA in (SetItem("callcreates") SetItem("transactions"))

    rule check DATA : [ .JSONList ] => . requires DATA =/=String "ommerHeaders" andBool DATA =/=String "out"
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
      requires #removeZeros(ACCTSTORAGE) ==K STORAGE

    rule <k> check "account" : { ACCT : { "code" : (CODE:WordStack) } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> CONTRACT </code>
           ...
         </account>
         requires #dasmContract(CODE, #mainContract(CONTRACT)) ==K CONTRACT
    rule <k> check "account" : { ACCT : { "code" : .WordStack } } => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           ...
         </account>
```

Here we check the other post-conditions associated with an EVM test.

```{.k .uiuck .rvk .standalone .node}
    rule check TESTID : { "out" : OUT } => check "out" : OUT ~> failure TESTID
 // --------------------------------------------------------------------------
    rule check "out" : ((OUT:String) => #parseHexWord(OUT))
    rule <k> check "out" : OUT => . ... </k> <output> OUT , .Ints </output>
    rule <k> check "out" : 0   => . ... </k> <output> .Ints </output>
    rule <k> check "out" : [ OUT ] => . ... </k> <output> OUTPUT </output> requires #toInts(OUT) ==K OUTPUT

    rule check TESTID : { "logs" : LOGS } => check "logs" : LOGS ~> failure TESTID
 // ------------------------------------------------------------------------------
    rule <k> check "logs" : HASH:String => . ... </k> <logData> SL </logData> requires #parseHexBytes(Keccak256(#rlpEncodeLogs(SL))) ==K #parseByteStack(HASH)

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
    rule <k> check "gas" : GLEFT => . ... </k> <checkGas> false </checkGas>
    rule <k> check "gas" : GLEFT => . ... </k> <checkGas> true  </checkGas> <gas> GLEFT </gas>

    rule check TESTID : { "callcreates" : CCREATES } => check "callcreates" : CCREATES ~> failure TESTID
 // ----------------------------------------------------------------------------------------------------
    rule check "callcreates" : { ("data" : (DATA:String)) , ("destination" : (ACCTTO:String)) , ("gasLimit" : (GLIMIT:String)) , ("value" : (VAL:String)) , .JSONList }
      => .

    rule check TESTID : { "genesisBlockHeader" : BLOCKHEADER } => check "genesisBlockHeader" : BLOCKHEADER ~> failure TESTID
 // ------------------------------------------------------------------------------------------------------------------------
    rule check "genesisBlockHeader" : { KEY : VALUE , REST } => check "genesisBlockHeader" : { KEY : VALUE } ~> check "genesisBlockHeader" : { REST } requires REST =/=K .JSONList
    rule check "genesisBlockHeader" : { KEY : VALUE } => .K requires KEY =/=String "hash"

    rule check "genesisBlockHeader" : { "hash": (HASH:String => #asUnsigned(#parseByteStack(HASH))) }
    rule <k> check "genesisBlockHeader" : { "hash": HASH } => . ... </k>
         <blockhash> ... ListItem(HASH) ListItem(_) </blockhash>
endmodule
```

