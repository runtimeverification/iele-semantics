```k
requires "iele.md"
requires "iele-binary.md"

module IELE-COVERAGE
    imports IELE
    imports IELE-BINARY

    configuration
      <kiele-coverage>
        <enableCoverage> $ENABLECOVERAGE:Bool </enableCoverage>
        <coverageHash>   0                    </coverageHash>
        <coverageIndex>  0                    </coverageIndex>
        <opcodeCoverage> .Coverage            </opcodeCoverage>
        <bytecodeCoverages>
          <bytecodeCoverage multiplicity="*" type="Map">
            <bytecodeHash> 0      </bytecodeHash>
            <bytecode>     ""     </bytecode>
            <coverageData> .Bytes </coverageData>
          </bytecodeCoverage>
        </bytecodeCoverages>
      </kiele-coverage>
```

Coverage Initialization Semantics
=================================

```k
    syntax KItem ::= "#initCoverage"
                   | "#finishCoverage"
 // ----------------------------------
    rule <k> #initCoverage => . ... </k>
         <contractCode> CONTRACT </contractCode>
         <coverageHash> HASH </coverageHash>
         <opcodeCoverage> _ => coverage() </opcodeCoverage>
         <bytecodeCoverage> <bytecodeHash> HASH </bytecodeHash> <bytecode> CODE </bytecode> ... </bytecodeCoverage>
      requires CODE ==String #contractBytes(CONTRACT)

    rule <k> #initCoverage ... </k>
         <contractCode> CONTRACT </contractCode>
         <coverageHash> HASH => keccak(String2Bytes(#contractBytes(CONTRACT))) </coverageHash>
         <bytecodeCoverage> <bytecodeHash> HASH </bytecodeHash> <bytecode> CODE </bytecode> ... </bytecodeCoverage>
      requires CODE =/=String #contractBytes(CONTRACT)

    rule <k> #initCoverage ... </k>
         <contractCode> CONTRACT </contractCode>
         <coverageHash> _ => keccak(String2Bytes(#contractBytes(CONTRACT))) </coverageHash>
         <bytecodeCoverages>
           ( .Bag
          => <bytecodeCoverage>
               <bytecodeHash> keccak(String2Bytes(#contractBytes(CONTRACT)))     </bytecodeHash>
               <bytecode>     #contractBytes(CONTRACT)                           </bytecode>
               <coverageData> padRightBytes(.Bytes, #sizeContract(CONTRACT), 0) </coverageData>
               ...
             </bytecodeCoverage>
           )
           ...
         </bytecodeCoverages>
      [owise]

    rule <k> #finishCoverage => . ... </k>
         <coverageHash>  HASH </coverageHash>
         <coverageIndex> I    </coverageIndex>
         <opcodeCoverage> OPCODE_COVERAGE => .Coverage </opcodeCoverage>
         <bytecodeCoverage>
           <bytecodeHash> HASH </bytecodeHash>
           <coverageData> DATA => DATA[I <- DATA[I] |Int #coverageAsByte(OPCODE_COVERAGE)] </coverageData>
           ...
         </bytecodeCoverage>

    rule <k> (. => #finishCoverage) ~> #end                                           ... </k> <opcodeCoverage> coverage(...)                 </opcodeCoverage> <typeChecking> false </typeChecking> [priority(35)]
    rule <k> (. => #finishCoverage) ~> #exception _                                   ... </k> <opcodeCoverage> coverage(... exception: true) </opcodeCoverage> <typeChecking> false </typeChecking> [priority(35)]
    rule <k> (. => #finishCoverage) ~> index( _, _ ) _::Instructions _::LabeledBlocks ... </k> <opcodeCoverage> coverage(...)                 </opcodeCoverage> <typeChecking> false </typeChecking> [priority(35)]
    rule <k> (. => #finishCoverage) ~> index( _, _ ) _::Instructions                  ... </k> <opcodeCoverage> coverage(...)                 </opcodeCoverage> <typeChecking> false </typeChecking> [priority(35)]

    rule <k> index( I, OP ) OPS::Instructions BLOCKS::LabeledBlocks => #initCoverage ~> OP OPS BLOCKS ... </k>
         <typeChecking> false </typeChecking>
         <coverageIndex>  _ => I    </coverageIndex>
         <opcodeCoverage> .Coverage </opcodeCoverage>
      [priority(35)]

    rule <k> index( I, OP ) OPS::Instructions => #initCoverage ~> OP OPS ... </k>
         <typeChecking> false </typeChecking>
         <coverageIndex>  _ => I    </coverageIndex>
         <opcodeCoverage> .Coverage </opcodeCoverage>
      [priority(35)]
```

Coverage Collection Semantics
=============================

```k
    syntax Coverage ::= ".Coverage" | coverage ( visited: Bool, exception: Bool, b3: Bool, b4: Bool, b5: Bool, b6: Bool, b7: Bool, b8: Bool )
 // -----------------------------------------------------------------------------------------------------------------------------------------

    syntax Coverage ::= coverage() [function]
 // -----------------------------------------
    rule coverage() => coverage( false, false, false, false, false, false, false, false )

    syntax Int ::= #coverageAsByte    ( Coverage       ) [function]
                 | #coverageAsByteAux ( Coverage , Int ) [function]
 // ---------------------------------------------------------------
    rule #coverageAsByte(C) => #coverageAsByteAux(C, 0)

    rule #coverageAsByteAux(_, B) => B [owise]
    rule #coverageAsByteAux( coverage(... visited   : true => false) , B => B |Int 1   )
    rule #coverageAsByteAux( coverage(... exception : true => false) , B => B |Int 2   )
    rule #coverageAsByteAux( coverage(... b3        : true => false) , B => B |Int 4   )
    rule #coverageAsByteAux( coverage(... b4        : true => false) , B => B |Int 8   )
    rule #coverageAsByteAux( coverage(... b5        : true => false) , B => B |Int 16  )
    rule #coverageAsByteAux( coverage(... b6        : true => false) , B => B |Int 32  )
    rule #coverageAsByteAux( coverage(... b7        : true => false) , B => B |Int 64  )
    rule #coverageAsByteAux( coverage(... b8        : true => false) , B => B |Int 128 )

    rule <k> #exceptional? [ _:Instruction ] ... </k> <opcodeCoverage> coverage(... visited:   false => true ) </opcodeCoverage> [priority(35)]
    rule <k> #exception _                    ... </k> <opcodeCoverage> coverage(... exception: false => true ) </opcodeCoverage> [priority(35)]

    rule <k> #exec br I:Int, _ ... </k> <opcodeCoverage> coverage(... b3: false => true ) </opcodeCoverage> requires I =/=Int 0 [priority(35)]
    rule <k> #exec br 0    , _ ... </k> <opcodeCoverage> coverage(... b4: false => true ) </opcodeCoverage>                     [priority(35)]

    syntax KItem ::= coverageMap( BytecodeCoveragesCell )
    syntax KItem ::= extractCoverage(KieleCoverageCell) [function, symbol]
 // ----------------------------------------------------------------------
    rule extractCoverage(<kiele-coverage>... <bytecodeCoverages> BYTECODECOVERAGES </bytecodeCoverages> ...</kiele-coverage>) => coverageMap( <bytecodeCoverages> BYTECODECOVERAGES </bytecodeCoverages> )
```

Semantics Overrides
===================

- These rules work around issues of already existing rules made by the coverage semantics

```k
    rule <k> index( _, OP ) OPS::Instructions BLOCKS::LabeledBlocks => OP OPS BLOCKS ... </k> <typeChecking> true </typeChecking> [priority(35)]
    rule <k> index( _, OP ) OPS::Instructions                       => OP OPS        ... </k> <typeChecking> true </typeChecking> [priority(35)]

    rule #registers( index( _, OP) => OP )
```

Index Contract Instructions
===========================

- To collect coverage we need to know which instruction in the contract we are executing.
- #indexContract will traverse a Contract and give an index to each of the instructions in the bottom ContractDefinition.

```k
    rule [[ #dasmContract( BS, NAME ) => #indexContract( #reverseContract( #dasmContract( 0, lengthBytes(BS), BS, NAME ) ) ) ]]
         <enableCoverage> true </enableCoverage>
      [priority(35)]

    rule [[ #subcontract ( (contract NAME ! _ _ { _ } #as CONTRACT) _, NAME ) => #indexContract( CONTRACT .Contract ) ]]
         <enableCoverage> true </enableCoverage>
      [priority(35)]

    syntax Instruction ::= index( Int, Instruction )

    syntax Contract ::= #indexContract( Contract )                                            [function]
                      | #indexContract( ctx: K, index: Int, currentDefs: TopLevelDefinitions,
                                        currentInstrs: Instructions, result: Contract )       [function, klabel(indexContractAus)]
 // ----------------------------------------------------------------------------------------------------
    rule #indexContract( CONTRACT ) => #indexContract( CONTRACT, 0, .TopLevelDefinitions, .Instructions, .Contract )

    syntax KItem ::= "indexContractReturn"
 // --------------------------------------
    rule #indexContract( ... ctx: indexContractReturn ~> .Contract ~> _, result: RESULT ) => RESULT
    rule #indexContract( ... ctx: indexContractReturn ~> (CONTRACT:ContractDefinition CONTRACTS => CONTRACTS) ~> _, result: RESULT => CONTRACT RESULT )

    rule #indexContract( ... ctx: ( .Contract => .K ) ~> _ )
    rule #indexContract( ... ctx: ( CONTRACT:ContractDefinition CONTRACTS => CONTRACT ~> indexContractReturn ~> CONTRACTS ) ~> _ )

    rule #indexContract( ... ctx:         ( CONTRACT:ContractDefinition => .K ) ~> _,
                             currentDefs: DEFS,
                             result:      RESULT => CONTRACT.withDefs( #reverseDefs( DEFS ) ) RESULT
                       )
      requires CONTRACT.defs ==K .TopLevelDefinitions

    rule #indexContract( ... ctx: ( CONTRACT => CONTRACT.defs ~> CONTRACT.withNoDefs ) ~> _ )
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

    rule #indexContract( ... ctx:           ( .Instructions LABEL : UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => LABEL : UNLABELEDBLOCK LABELEDBLOCKS ) ~> _)
    rule #indexContract( ... ctx:           (               LABEL : UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _,
                             currentInstrs: INSTRS => label ( LABEL ) INSTRS
                       )
    rule #indexContract( ... ctx:           ( INSTR:Instruction     UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _,
                             index:         I      => I +Int 1,
                             currentInstrs: INSTRS => index( I, INSTR ) INSTRS
                       )
```

Helper Functions
----------------

- FUNC.withBlocks( BLOCKS ) : Insert Blocks into a function definition
- FUNC.blocks               : Get the Blocks from a function definition
- CONTRACT.withDefs( DEFS ) : Insert TopLevelDefinitions into a contract
- CONTRACT.defs             : Get the TopLevelDefinitions from a contract
- CONTRACT.withNoDefs       : Get a contract without its TopLevelDefinitions
- #reverse{Contract,Defs}   : Reverse a list of ContractDefinition or TopLevelDefinition
- #sizeContract( CONTRACT ) : Get the size of a contract in Instructions

```k
    syntax FunctionDefinition ::= FunctionDefinition ".withBlocks" "(" Blocks ")" [function]
 // ----------------------------------------------------------------------------------------
    rule define        SIG { _ } .withBlocks ( BLOCKS ) => define        SIG { BLOCKS }
    rule define public SIG { _ } .withBlocks ( BLOCKS ) => define public SIG { BLOCKS }

    syntax Blocks ::= FunctionDefinition "." "blocks" [function]
 // ------------------------------------------------------------
    rule define        _SIG { BLOCKS } . blocks => BLOCKS
    rule define public _SIG { BLOCKS } . blocks => BLOCKS

    syntax ContractDefinition ::= ContractDefinition ".withDefs" "(" TopLevelDefinitions ")" [function]
 // ---------------------------------------------------------------------------------------------------
    rule contract NAME             { _ } .withDefs ( DEFS ) => contract NAME             { DEFS }
    rule contract NAME ! SIZE CODE { _ } .withDefs ( DEFS ) => contract NAME ! SIZE CODE { DEFS }

    syntax TopLevelDefinitions ::= ContractDefinition ".defs"       [function]
    syntax ContractDefinition  ::= ContractDefinition ".withNoDefs" [function]
 // --------------------------------------------------------------------------
    rule contract _NAME               { DEFS } .defs => DEFS
    rule contract _NAME ! _SIZE _CODE { DEFS } .defs => DEFS

    rule contract NAME             { _DEFS } .withNoDefs => contract NAME             { .TopLevelDefinitions }
    rule contract NAME ! SIZE CODE { _DEFS } .withNoDefs => contract NAME ! SIZE CODE { .TopLevelDefinitions }

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

    syntax Int ::= #sizeContract( Contract ) [function]
                 | #sizeContract( K, Int )   [function, klabel(contractSizeAux)]
 // ----------------------------------------------------------------------------
    rule #sizeContract( CONTRACT ) => #sizeContract( #reverseContract( CONTRACT ), 0 )

    rule #sizeContract( .Contract ~> _, I ) => I

    rule #sizeContract( ( CONTRACT:ContractDefinition _CONTRACTS:Contract => CONTRACT ~> .Contract ) ~> _, _ )
    rule #sizeContract( ( CONTRACT:ContractDefinition => CONTRACT.defs ) ~> _, _ )
    rule #sizeContract( ( .TopLevelDefinitions => .K ) ~> _, _ )
    rule #sizeContract( ( DEF:TopLevelDefinition DEFS:TopLevelDefinitions => DEF ~> DEFS ) ~> _, _ )
    rule #sizeContract( ( DEF:TopLevelDefinition => .K ) ~> _, _ ) requires isGlobalDefinition(DEF) orBool isContractDeclaration(DEF)
    rule #sizeContract( ( FUNC => FUNC.blocks ) ~> _, _ )
    rule #sizeContract( ( .Instructions .LabeledBlocks => .K ) ~> _, _ )
    rule #sizeContract( (               .LabeledBlocks => .K ) ~> _, _ )
    rule #sizeContract( ( .Instructions _ : UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _, _ )
    rule #sizeContract( (               _ : UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _, _ )
    rule #sizeContract( ( _INSTR UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _, I => I +Int 1 )

endmodule
```
