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

```

Index Contract Instructions
===========================

- To collect coverage we need to know which instruction in the contract we are executing.
- #indexContract will traverse a Contract and give an index to each of the instructions in the ContractDefinitions.

```k
    syntax Instruction ::= index( Int, Instruction )

    syntax Contract ::= #indexContract( Contract )                                            [function]
                      | #indexContract( ctx: K, index: Int, currentDefs: TopLevelDefinitions,
                                        currentInstrs: Instructions, result: Contract )       [function, klabel(indexContractAus)]
 // ----------------------------------------------------------------------------------------------------
    rule #indexContract( CONTRACT ) => #indexContract( CONTRACT ~> indexContractReturn, 0, .TopLevelDefinitions, .Instructions, .Contract )

    syntax KItem ::= "indexContractReturn"
 // --------------------------------------
    rule #indexContract( ... ctx: indexContractReturn ~> _, result: RESULT ) => #reverseContract( RESULT )

    rule #indexContract( ... ctx: ( .Contract => .K ) ~> _ )
    rule #indexContract( ... ctx: ( CONTRACT:ContractDefinition CONTRACTS => CONTRACT ~> CONTRACTS ) ~> _ )

    rule #indexContract( ... ctx:         ( CONTRACT:ContractDefinition => .K ) ~> _,
                             currentDefs: DEFS => .TopLevelDefinitions,
                             result:      RESULT => CONTRACT.withDefs( #reverseDefs( DEFS ) ) RESULT
                       )
      requires CONTRACT.defs ==K .TopLevelDefinitions

    rule #indexContract( ... ctx:         ( CONTRACT => CONTRACT.defs ~> CONTRACT.withNoDefs ) ~> _,
                             index:       _ => 0,
                             currentDefs: _ => .TopLevelDefinitions
                       )
      requires CONTRACT.defs =/=K .TopLevelDefinitions
```

Iterate through the TopLevelDefinitions
---------------------------------------

```k
    rule #indexContract( ... ctx: ( .TopLevelDefinitions => .K ) ~> _ )

    rule #indexContract( ... ctx: ( DEF:FunctionDefinition DEFS => DEF ~> DEFS ) ~> _ )
    rule #indexContract( ... ctx: ( DEF:TopLevelDefinition DEFS => DEFS ) ~> _, currentDefs: CURDEFS => DEF CURDEFS )
      requires isGlobalDefinition(DEF) orBool isContractDeclaration(DEF)

    rule #indexContract( ... ctx: ( FUNC => FUNC.blocks ~> FUNC.withBlocks( .LabeledBlocks ) ) ~> _, currentInstrs: _ => .Instructions )
      requires FUNC.blocks =/=K .LabeledBlocks

    rule #indexContract( ... ctx:           ( FUNC => .K ) ~> _,
                             currentDefs:   DEFS   => FUNC.withBlocks( #toBlocks( INSTRS ) ) DEFS,
                             currentInstrs: INSTRS => .Instructions
                       )
      requires FUNC.blocks ==K .LabeledBlocks
```

Index the Instructions
----------------------

```k
    rule #indexContract( ... ctx: ( .Instructions .LabeledBlocks => .K ) ~> _ )
    rule #indexContract( ... ctx: (               .LabeledBlocks => .K ) ~> _ )

    rule #indexContract( ... ctx:           ( .Instructions LABEL : UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _,
                             currentInstrs: INSTRS => label ( LABEL ) INSTRS
                       )
    rule #indexContract( ... ctx:           ( INSTR:Instruction     UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _,
                             index:         I      => I +Int 1,
                             currentInstrs: INSTRS => index( I, INSTR ) INSTRS
                       )
```

indexContract Helpers
---------------------

- FUNC.withBlocks( BLOCKS ) : Insert Blocks into a function definition
- FUNC.blocks               : Get the Blocks from a function definition
- CONTRACT.withDefs( DEFS ) : Insert TopLevelDefinitions into a contract
- CONTRACT.defs             : Get the TopLevelDefinitions from a contract
- CONTRACT.withNoDefs       : Get a contract without its TopLevelDefinitions
- #reverse{Contract,Defs}   : Reverse a list of ContractDefinition or TopLevelDefinition

```k
    syntax FunctionDefinition ::= FunctionDefinition ".withBlocks" "(" Blocks ")" [function]
 // ----------------------------------------------------------------------------------------
    rule define        SIG { _ } .withBlocks ( BLOCKS ) => define        SIG { BLOCKS }
    rule define public SIG { _ } .withBlocks ( BLOCKS ) => define public SIG { BLOCKS }

    syntax Blocks ::= FunctionDefinition "." "blocks" [function]
 // ------------------------------------------------------------
    rule define        SIG { BLOCKS } . blocks => BLOCKS
    rule define public SIG { BLOCKS } . blocks => BLOCKS

    syntax ContractDefinition ::= ContractDefinition ".withDefs" "(" TopLevelDefinitions ")" [function]
 // ---------------------------------------------------------------------------------------------------
    rule contract NAME             { _ } .withDefs ( DEFS ) => contract NAME             { DEFS }
    rule contract NAME ! SIZE CODE { _ } .withDefs ( DEFS ) => contract NAME ! SIZE CODE { DEFS }

    syntax TopLevelDefinitions ::= ContractDefinition ".defs"       [function]
    syntax ContractDefinition  ::= ContractDefinition ".withNoDefs" [function]
 // --------------------------------------------------------------------------
    rule contract NAME { DEFS }             .defs => DEFS
    rule contract NAME ! SIZE CODE { DEFS } .defs => DEFS

    rule contract NAME { DEFS }             .withNoDefs => contract NAME             { .TopLevelDefinitions }
    rule contract NAME ! SIZE CODE { DEFS } .withNoDefs => contract NAME ! SIZE CODE { .TopLevelDefinitions }

    syntax Contract ::= #reverseContract( Contract           ) [function]
                      | #reverseContract( Contract, Contract ) [function, klabel(reverseContractAux)]
 // -------------------------------------------------------------------------------------------------
    rule #reverseContract( CONTRACTS ) => #reverseContract( CONTRACTS, .Contract )
    rule #reverseContract( CONTRACT CONTRACTS, RESULT ) => #reverseContract( CONTRACTS, CONTRACT RESULT )
    rule #reverseContract( .Contract,          RESULT ) => RESULT

    syntax TopLevelDefinitions ::= #reverseDefs( TopLevelDefinitions                      ) [function]
                                 | #reverseDefs( TopLevelDefinitions, TopLevelDefinitions ) [function, klabel(reverseDefsAux)]
 // --------------------------------------------------------------------------------------------------------------------------
    rule #reverseDefs( DEFS ) => #reverseDefs( DEFS, .TopLevelDefinitions )
    rule #reverseDefs( DEF DEFS,             RESULT ) => #reverseDefs( DEFS, DEF RESULT )
    rule #reverseDefs( .TopLevelDefinitions, RESULT ) => RESULT

endmodule
```
