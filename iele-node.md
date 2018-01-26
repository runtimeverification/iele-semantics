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

endmodule
```
