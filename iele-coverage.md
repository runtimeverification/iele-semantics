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
               <bytecodeHash> HASH                                               </bytecodeHash>
               <bytecode>     #contractBytes(CONTRACT)                           </bytecode>
               <coverageData> padRightBytes(.Bytes, #sizeContract(CONTRACT), 48) </coverageData>
               ...
             </bytecodeCoverage>
           )
           ...
         </bytecodeCoverages>
      [owise]
```

Semantics Overrides
===================

- These higher priority rules inject coverage related semantics into already existing rules

```k
    rule <k> #mkCall ACCTFROM ACCTTO CODE FUNC GLIMIT VALUE ARGS STATIC:Bool
          => #initCoverage(CODE) ~> #initVM(ARGS) ~> #initFun(FUNC, #sizeRegs(ARGS), false)
         ...
         </k>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => ARGS </callData>
         <callValue> _ => VALUE </callValue>
         <id> _ => ACCTTO </id>
         <gas> _ => GLIMIT </gas>
         <caller> _ => ACCTFROM </caller>
         (<program> _ </program> => CODE:ProgramCell)
         <static> OLDSTATIC:Bool => OLDSTATIC orBool STATIC </static>
      [priority(35)]

    rule <mode> EXECMODE </mode>
         <k> #mkCreate ACCTFROM ACCTTO CODE GAVAIL VALUE ARGS
          => #initCoverage( CODE, #parseHexWord(Keccak256(#contractBytes(CODE))) ) ~> #initVM(ARGS) ~> #initFun(init, #sizeRegs(ARGS), true)
         ...
         </k>
         <schedule> SCHED </schedule>
         <id> ACCT => ACCTTO </id>
         <gas> OLDGAVAIL => GAVAIL </gas>
         (<program> _ </program> => #loadCode(CODE))
         <caller> _ => ACCTFROM </caller>
         <callDepth> CD => CD +Int 1 </callDepth>
         <callData> _ => .Ints </callData>
         <callValue> _ => VALUE </callValue>
         <account>
           <acctID> ACCTTO </acctID>
           <nonce> NONCE => NONCE +Int 1 </nonce>
           ...
         </account>
      [priority(35)]

    rule <k> #popCallStack => #initCoverage( <program> PROGRAM </program> ) ... </k>
         <callFrame> _ => <program> PROGRAM </program> FRAME </callFrame>
         <callStack> (ListItem(<callFrame> <program> PROGRAM </program> FRAME </callFrame>) => .List) ... </callStack> [priority(35)]

    rule <k> index( _, OP ) OPS::Instructions BLOCKS::LabeledBlocks => OP OPS BLOCKS ... </k> <typeChecking> true </typeChecking> [priority(35)]
    rule <k> index( _, OP ) OPS::Instructions                       => OP OPS        ... </k> <typeChecking> true </typeChecking> [priority(35)]

    rule <typeChecking> false </typeChecking>
         <k> index( I, OP ) OPS::Instructions BLOCKS::LabeledBlocks => OP OPS BLOCKS ... </k>
         <coverageHash> HASH </coverageHash>
         <bytecodeCoverage>
           <bytecodeHash> HASH                  </bytecodeHash>
           <coverageData> DATA => DATA[I <- 49] </coverageData>
           ...
         </bytecodeCoverage>
      [priority(35)]

    rule <typeChecking> false </typeChecking>
         <k> index( I, OP ) OPS::Instructions => OP OPS ... </k>
         <coverageHash> HASH </coverageHash>
         <bytecodeCoverage>
           <bytecodeHash> HASH                  </bytecodeHash>
           <coverageData> DATA => DATA[I <- 49] </coverageData>
           ...
         </bytecodeCoverage>
      [priority(35)]

    rule #dasmContract( BS, NAME ) => #indexContract( #dasmContract( 0, lengthBytes(BS), BS, NAME ) ) [priority(35)]

    rule #registers( index( _, OP) => OP )
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

    syntax Int ::= #sizeContract( Contract ) [function]
                 | #sizeContract( K, Int )   [function, klabel(contractSizeAux)]
 // ----------------------------------------------------------------------------
    rule #sizeContract( CONTRACT ) => #sizeContract( CONTRACT, 0 )

    rule #sizeContract( .Contract ~> _, I ) => I

    rule #sizeContract( ( CONTRACT:ContractDefinition CONTRACTS:Contract => CONTRACT ~> CONTRACTS ) ~> _, _ )
    rule #sizeContract( ( CONTRACT:ContractDefinition => CONTRACT.defs ) ~> _, _ )
    rule #sizeContract( ( .TopLevelDefinitions => .K ) ~> _, _ )
    rule #sizeContract( ( DEF:TopLevelDefinition DEFS:TopLevelDefinitions => DEF ~> DEFS ) ~> _, _ )
    rule #sizeContract( ( DEF:TopLevelDefinition => .K ) ~> _, _ ) requires isGlobalDefinition(DEF) orBool isContractDeclaration(DEF)
    rule #sizeContract( ( FUNC => FUNC.blocks ) ~> _, _ )
    rule #sizeContract( ( .Instructions .LabeledBlocks => .K ) ~> _, _ )
    rule #sizeContract( (               .LabeledBlocks => .K ) ~> _, _ )
    rule #sizeContract( ( .Instructions _ : UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _, _ )
    rule #sizeContract( ( INSTR UNLABELEDBLOCK:Instructions LABELEDBLOCKS:LabeledBlocks => UNLABELEDBLOCK LABELEDBLOCKS ) ~> _, I => I +Int 1 )

endmodule
```
