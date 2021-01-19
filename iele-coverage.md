```k
requires "iele-testing.md"

module IELE-COVERAGE
    imports IELE-TESTING

    configuration
        <kiele/>
        <coverage>
          <coverageHash>  0 </coverageHash>
          <coverageIndex> 0 </coverageIndex>
          <bytecodeCoverages>
            <bytecodeCoverage multiplicity="*" type="Map">
              <bytecodeHash> 0      </bytecodeHash>
              <bytecode>     ""     </bytecode>
              <coverageData> .Bytes </coverageData>
            </bytecodeCoverage>
          </bytecodeCoverages>
        </coverage>

    syntax KItem ::= #initCoverage( ProgramCell )
                   | #initCoverage( Contract, Int )
 // -----------------------------------------------
    rule <k> #initCoverage( <program> ... <contractCode> CONTRACT </contractCode> </program> )
          => #initCoverage( CONTRACT, #parseHexWord(Keccak256(#contractBytes(CONTRACT))) ) ... </k>

    rule <k> #initCoverage( CONTRACT, HASH ) => . ... </k>
         <coverageHash> _ => HASH </coverageHash>
         <bytecodeCoverage>
           <bytecodeHash> HASH </bytecodeHash>
           <bytecode>     CODE </bytecode>
           <coverageData> _    </coverageData>
         </bytecodeCoverage>
      requires CODE ==String #contractBytes(CONTRACT)

    rule <k> #initCoverage( CONTRACT, HASH ) => . ... </k>
         <coverageHash> _ => HASH </coverageHash>
         <bytecodeCoverages>
           ( .Bag
          => <bytecodeCoverage>
               <bytecodeHash> HASH                                                </bytecodeHash>
               <bytecode>     #contractBytes(CONTRACT)                            </bytecode>
               <coverageData> .Bytes[0 .. lengthString(#contractBytes(CONTRACT))] </coverageData>
               ...
             </bytecodeCoverage>
           )
           ...
         </bytecodeCoverages>
      [owise]

endmodule
```
