```{.k .node}
module IELE-NODE
    imports IELE
    imports IELE-BINARY
    imports K-REFLECTION
    imports COLLECTIONS

    syntax Int ::= #getBalance(Int) [function, hook(BLOCKCHAIN.getBalance)]
                 | #getNonce(Int) [function, hook(BLOCKCHAIN.getNonce)]
    syntax Bool ::= #isCodeEmpty(Int) [function, hook(BLOCKCHAIN.isCodeEmpty)]
 // ---------------------------------------------------------------------------
    rule <k> #loadAccount ACCT => . ... </k>
         <activeAccounts> ACCTS (.Set => SetItem(ACCT)) </activeAccounts>
         <accounts>
           ( .Bag
          => <account>
               <acctID> ACCT </acctID>
               <balance> #getBalance(ACCT) </balance>
               <code> #if #isCodeEmpty(ACCT) #then #emptyCode #else .Contract #fi </code>
               <storage> .Map </storage>
               <nonce> #getNonce(ACCT) </nonce>
             </account>
           )
           ...
         </accounts>
      requires notBool ACCT in ACCTS

    syntax Int ::= #getStorageData(Int, Int) [function, hook(BLOCKCHAIN.getStorageData)]
 // --------------------------------------------------------------------------------
    rule <k> #lookupStorage(ACCT, INDEX) => . ... </k>
         <account>
           <acctID>  ACCT                                                         </acctID>
           <storage> STORAGE => STORAGE [ INDEX <- #getStorageData(ACCT, INDEX) ] </storage>
           ...
         </account>
      requires notBool INDEX in_keys(STORAGE)

    syntax String ::= #getCode(Int) [function, hook(BLOCKCHAIN.getCode)]
 // ----------------------------------------------------------------
    rule <k> #lookupCode(ACCT) => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> .Contract => #dasmContract(#parseByteStackRaw(#getCode(ACCT)), Main) </code>
           ...
         </account>

    syntax Int ::= #getBlockhash(Int) [function, hook(BLOCKCHAIN.getBlockhash)]
 // -----------------------------------------------------------------------
    rule #exec REG = call @iele.blockhash ( N ) => #load REG #getBlockhash(N)
      requires N >=Int 0  andBool N <Int 256
    rule #exec REG = call @iele.blockhash ( N ) => #load REG 0
      requires N <Int 0 orBool N >=Int 256

    syntax IELESimulation ::= runVM(iscreate: Bool, to: Int, from: Int, code: String, args: List, value: Int, gasprice: Int, gas: Int, beneficiary: Int, difficulty: Int, number: Int, gaslimit: Int, timestamp: Int, function: String)

    rule <k> (.K => #loadAccount ACCTFROM) ~> runVM(... from: ACCTFROM) ... </k>
         <activeAccounts> .Set </activeAccounts>

    rule <k> runVM(true, _, ACCTFROM, CODESTR, ARGS, VALUE, GPRICE, GAVAIL, CB, DIFF, NUMB, GLIMIT, TS, _)
          => #fun(CODE => #fun(CONTRACT =>
             #checkContract CONTRACT
          ~> #create ACCTFROM #newAddr(ACCTFROM, NONCE -Int 1) GAVAIL VALUE CONTRACT #toInts(ARGS)
          ~> #codeDeposit #newAddr(ACCTFROM, NONCE -Int 1) #sizeWordStack(CODE) CONTRACT %0 %1 true)(#if #isValidContract(CODE) #then #dasmContract(CODE, Main) #else #illFormed #fi))(#parseByteStackRaw(CODESTR))
         ...
         </k>
         <schedule> SCHED </schedule>
         <gasPrice> _ => GPRICE </gasPrice>
         <origin> _ => ACCTFROM </origin>
         <callDepth> _ => -1 </callDepth>
         <beneficiary> _ => CB </beneficiary>
         <difficulty> _ => DIFF </difficulty>
         <number> _ => NUMB </number>
         <gasLimit> _ => GLIMIT </gasLimit>
         <timestamp> _ => TS </timestamp>
         <account>
           <acctID> ACCTFROM </acctID>
           <nonce> NONCE </nonce>
           ...
         </account>
         <activeAccounts> ACCTS </activeAccounts>
      requires ACCTFROM in ACCTS

    rule <k> runVM(false, ACCTTO, ACCTFROM, _, ARGS, VALUE, GPRICE, GAVAIL, CB, DIFF, NUMB, GLIMIT, TS, FUNC)
          => #call ACCTFROM ACCTTO @ String2IeleName(FUNC) GAVAIL VALUE #toInts(ARGS) false
          ~> #endVM
         ...
         </k>
         <schedule> SCHED </schedule>
         <gasPrice> _ => GPRICE </gasPrice>
         <origin> _ => ACCTFROM </origin>
         <callDepth> _ => -1 </callDepth>
         <beneficiary> _ => CB </beneficiary>
         <difficulty> _ => DIFF </difficulty>
         <number> _ => NUMB </number>
         <gasLimit> _ => GLIMIT </gasLimit>
         <timestamp> _ => TS </timestamp>
         <activeAccounts> ACCTS </activeAccounts>
      requires ACCTFROM in ACCTS

    syntax IELECommand ::= "#endVM"
 // -------------------------------
    rule <k> #exception STATUS ~> #endVM => #popCallStack ~> #popWorldState ~> #popSubstate ~> STATUS </k>
         <output> _ => .Ints </output>
    rule <k> #revert OUT       ~> #endVM => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ~> OUT </k>
         <gas> GAVAIL </gas>       

    rule <k> #end ~> #endVM => #popCallStack ~> #dropWorldState ~> #dropSubstate ~> #refund GAVAIL ~> 0 </k>
         <gas> GAVAIL </gas>

    // the k cell doesn't completely empty during function call/return or at any other time while executing a contract,
    // therefore this rule can only apply at the end following an #endVM or a #codeDeposit _ _ _ _ _ true.
    rule <k> _:Int </k>
         (<account>
           <balance> 0 </balance>
           <code> #emptyCode </code>
           <nonce> 0 </nonce>
           ...
         </account> => .Bag)

    syntax Ints ::= #toInts(List) [function, klabel(ListToInts)]
 // ------------------------------------------------------------
    rule #toInts(.List) => .Ints
    rule #toInts(ListItem(I) L) => I , #toInts(L)

    syntax List ::= #toList(Ints) [function]
 // ----------------------------------------
    rule #toList(.Ints) => .List
    rule #toList(I , L) => ListItem(I) #toList(L)

    syntax KItem ::= vmResult(return: List,gas: Int,refund: Int,status: Int,selfdestruct: List,logs: List,AccountsCell, touched: List)
    syntax KItem ::= extractConfig(GeneratedTopCell) [function]
 // ----------------------------------------------------------
    rule extractConfig(<generatedTop>... <schedule> SCHED </schedule> <output> OUT </output> <gas> GAVAIL </gas> <refund> REFUND </refund> <k> STATUS:Int </k> <selfDestruct> SD </selfDestruct> <logData> LOGS </logData> <accounts> ACCTS </accounts> ... </generatedTop>) => vmResult(#toList(OUT),GAVAIL up/Int Sgasdivisor < SCHED >,REFUND,STATUS,Set2List(SD),LOGS,<accounts> ACCTS </accounts>, .List)

endmodule
```
