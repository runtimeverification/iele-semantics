```{.k .uiuck .rvk .node}
module IELE-NODE
    imports IELE
    imports IELE-BINARY
    imports K-REFLECTION

    syntax Int ::= #getBalance(Int) [function, hook(MANTIS.getBalance)]
                 | #getNonce(Int) [function, hook(MANTIS.getNonce)]
 // ---------------------------------------------------------------------------
    rule <k> #newAccount ACCT => . ... </k>
         <activeAccounts> ACCTS (.Set => SetItem(ACCT)) </activeAccounts>
         <accounts>
           ( .Bag
          => <account>
               <acctID> ACCT </acctID>
               <balance> #getBalance(ACCT) </balance>
               <code> .Contract </code>
               <storage> .Map </storage>
               <nonce> #getNonce(ACCT) </nonce>
             </account>
           )
           ...
         </accounts>
      requires notBool ACCT in ACCTS

    syntax Int ::= #getStorageData(Int, Int) [function, hook(MANTIS.getStorageData)]
 // --------------------------------------------------------------------------------
    rule <k> #lookupStorage(ACCT, INDEX) => . ... </k>
         <account>
           <acctID>  ACCT                                                         </acctID>
           <storage> STORAGE => STORAGE [ INDEX <- #getStorageData(ACCT, INDEX) ] </storage>
           ...
         </account>
      requires notBool INDEX in_keys(STORAGE)

    syntax String ::= #getCode(Int) [function, hook(MANTIS.getCode)]
 // ----------------------------------------------------------------
    rule <k> #lookupCode(ACCT) => . ... </k>
         <account>
           <acctID> ACCT </acctID>
           <code> .Contract => #dasmContract(#parseByteStackRaw(#getCode(ACCT)), Main) </code>
           ...
         </account>

    syntax Int ::= #getBlockhash(Int) [function, hook(MANTIS.getBlockhash)]
 // -----------------------------------------------------------------------
    rule #exec REG = call @iele.blockhash ( N ) => #load REG #getBlockhash(N)
      requires N >=Int 0  andBool N <Int 256
    rule #exec REG = call @iele.blockhash ( N ) => #load REG 0
      requires N <Int 0 orBool N >=Int 256

    syntax IELECommand ::= runVM(iscreate: Bool, to: Int, from: Int, code: String, args: List, value: Int, gasprice: Int, gas: Int, beneficiary: Int, difficulty: Int, number: Int, gaslimit: Int, timestamp: Int, function: String)

    rule <k> (.K => #newAccount ACCTFROM) ~> runVM(... from: ACCTFROM) ... </k>
         <activeAccounts> .Set </activeAccounts>

    rule <k> runVM(true, _, ACCTFROM, CODESTR, ARGS, VALUE, GPRICE, GAVAIL, CB, DIFF, NUMB, GLIMIT, TS, _)
          => #fun(CODE => #create ACCTFROM #newAddr(ACCTFROM, NONCE -Int 1) (GAVAIL -Int G0(SCHED, CODE, #toInts(ARGS), true)) VALUE #dasmContract(CODE, Main) #toInts(ARGS)
          ~> #codeDeposit #newAddr(ACCTFROM, NONCE -Int 1) #sizeWordStack(CODE) #dasmContract(CODE, Main) %0 %1 true)(#parseByteStackRaw(CODESTR))
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

    rule <k> runVM(false, ACCTTO, ACCTFROM, _, ARGS, VALUE, GPRICE, GAVAIL, CB, DIFF, NUMB, GLIMIT, TS, FUNC)
          => #call ACCTFROM ACCTTO {#parseToken("IeleName", FUNC)}:>IeleName (GAVAIL -Int G0(SCHED, .WordStack, #toInts(ARGS), false)) VALUE #toInts(ARGS) false
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

    syntax IELECommand ::= "#endVM"
 // -------------------------------
    rule <k> #exception STATUS ~> #endVM => #popCallStack ~> #popWorldState ~> #popSubstate ~> STATUS </k>
         <output> _ => .Ints </output>
    rule <k> #revert OUT       ~> #endVM => #popCallStack ~> #popWorldState ~> #popSubstate ~> #refund GAVAIL ~> OUT </k>
         <gas> GAVAIL </gas>       

    rule <k> #end ~> #endVM => #popCallStack ~> #dropWorldState ~> #dropSubstate ~> #refund GAVAIL ~> 0 </k>
         <gas> GAVAIL </gas>

    syntax Ints ::= #toInts(List) [function, klabel(ListToInts)]
 // ------------------------------------------------------------
    rule #toInts(.List) => .Ints
    rule #toInts(ListItem(I) L) => I , #toInts(L)

endmodule
```