module Main where

import Test.Tasty
import Test.Tasty.HUnit

import IeleParser
import IeleParserImplementation

import Control.Applicative (many)
import Data.Either (isLeft)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

main :: IO ()
main =
  defaultMain
    (testGroup
      "All Tests"
      [ testGroup "Token Tests" tokenTests
      , testGroup "Instruction Tests" instructionTests
      , testGroup "Other Tests" otherTests
      ]
    )

ieleNameExamples :: [(IeleName, String)]
ieleNameExamples =
  [ (IeleName "test", "test")
  , (IeleName "-1.bc7", "-1.bc7")
  , (IeleName "1234", "1234")
  , (IeleName "-1", "-1")
  ]
ieleNameCounterexamples :: [String]
ieleNameCounterexamples =
  ["@a", "12a"]

intTokenExamples :: [(IntToken, String)]
intTokenExamples =
  [ (IntToken 0, "0")
  , (IntToken 1, "1")
  , (IntToken 123, "123")
  , (IntToken (-123), "-123")
  , (IntToken 0, "false")
  , (IntToken 1, "true")
  ]

intTokenCounterexamples :: [String]
intTokenCounterexamples =
  ["@a", "12a", "bcd", "12.5"]

globalNameExamples :: [(GlobalName, String)]
globalNameExamples =
  [ (GlobalName (IeleName "0"), "@0")
  , (GlobalName (IeleName "a"), "@a")
  , (GlobalName (IeleName "a.bc"), "@a.bc")
  , (GlobalName (IeleName "ab"), "@ ab")
  ]

globalNameCounterexamples :: [String]
globalNameCounterexamples =
  ["@", "a", "@@bcd", "@@", "%a"]

localNameExamples :: [(LocalName, String)]
localNameExamples =
  [ (LocalName (IeleName "0"), "%0")
  , (LocalName (IeleName "a"), "%a")
  , (LocalName (IeleName "a.bc"), "%a.bc")
  , (LocalName (IeleName "ab"), "% ab")
  ]

localNameCounterexamples :: [String]
localNameCounterexamples =
  ["%", "a", "%%bcd", "%%", "@a"]

localNamesExamples :: [(LocalNames, String)]
localNamesExamples =
  [ (LocalNames [LocalName (IeleName "a")], "%a")
  , (LocalNames [LocalName (IeleName "a"), LocalName (IeleName "b")], "%a,%b")
  , (LocalNames [LocalName (IeleName "a"), LocalName (IeleName "b")], "%a, %b")
  , (LocalNames [], "")
  ]

localNamesCounterexamples :: [String]
localNamesCounterexamples =
  ["%"]

lValueExamples :: [(LValue, String)]
lValueExamples =
  [ (LValueLocalName (LocalName (IeleName "a")), "%a")
  , (LValueGlobalName (GlobalName (IeleName "a")), "@a")
  ]

lValueCounterexamples :: [String]
lValueCounterexamples =
  ["%", "a", "@"]

nonEmptyLValuesExamples :: [(LValues, String)]
nonEmptyLValuesExamples =
  [ (LValues [LValueLocalName (LocalName (IeleName "a"))], "%a")
  , ( LValues
        [ LValueLocalName (LocalName (IeleName "a"))
        , LValueGlobalName (GlobalName (IeleName "b"))]
    , "%a,@b"
    )
  , ( LValues
      [ LValueGlobalName (GlobalName (IeleName "a"))
      , LValueLocalName (LocalName (IeleName "b"))
      ]
    , "@a, %b"
    )
  ]

nonEmptyLValuesCounterexamples :: [String]
nonEmptyLValuesCounterexamples =
  ["%", "@", ""]

operandExamples :: [(Operand, String)]
operandExamples =
  [ (OperandLValue (LValueLocalName (LocalName (IeleName "a"))), "%a")
  , (OperandLValue (LValueGlobalName (GlobalName (IeleName "a"))), "@a")
  , (OperandInt (IntToken 10), "10")
  ]

operandCounterexamples :: [String]
operandCounterexamples =
  ["%", "a", "@"]

operandsExamples :: [(Operands, String)]
operandsExamples =
  [ (Operands
      [OperandLValue (LValueLocalName (LocalName (IeleName "a")))], "%a")
  , ( Operands
      [ OperandLValue (LValueLocalName (LocalName (IeleName "a")))
      , OperandLValue (LValueGlobalName (GlobalName (IeleName "b")))
      ]
    , "%a,@b"
    )
  , ( Operands
      [ OperandLValue (LValueGlobalName (GlobalName (IeleName "a")))
      , OperandLValue (LValueLocalName (LocalName (IeleName "b")))
      ]
    , "@a, %b"
    )
  , (Operands [], "")
  ]

operandsCounterexamples :: [String]
operandsCounterexamples =
  ["%", "@"]


nonEmptyOperandsExamples :: [(Operands, String)]
nonEmptyOperandsExamples =
    [ ( Operands
        [OperandLValue (LValueLocalName (LocalName (IeleName "a")))]
      , "%a"
      )
    , ( Operands
        [ OperandLValue (LValueLocalName (LocalName (IeleName "a")))
        , OperandLValue (LValueGlobalName (GlobalName (IeleName "b")))
        ]
      , "%a,@b"
      )
    , ( Operands
        [ OperandLValue (LValueGlobalName (GlobalName (IeleName "a")))
        , OperandLValue (LValueLocalName (LocalName (IeleName "b")))
        ]
      , "@a, %b"
      )
    ]

nonEmptyOperandsCounterexamples :: [String]
nonEmptyOperandsCounterexamples =
    ["%", "@", ""]


tokenTests :: [TestTree]
tokenTests =
    [ singleCharTests '@' at "a?.1"
    , singleCharTests ')' closedParenthesis "a?.1"
    , singleCharTests '}' closedCurlyBrace "a?.1"
    , singleCharTests ':' colon "a?.1"
    , singleCharTests ',' comma "a?.1"
    , singleCharTests '=' equal "a?.1"
    , singleCharTests '-' minus "a?.1"
    , singleCharsTests "Iele name first char" ".bC._-" ieleNameFirstChar "1@ %"
    , singleCharsTests "Iele name non-first char" ".bC._-7" ieleNameNonFirstChar "@ %"
    , singleCharTests '{' openCurlyBrace "a?."
    , singleCharTests '(' openParenthesis "a?."
    , singleCharTests '%' percent "a?."
    , testToken "Skip keyword" [((), "global")] (skipKeyword "global") [" global", "globals", "other"] IgnoresSpaces
    , testToken "Iele Name" ieleNameExamples ieleNameToken ieleNameCounterexamples IgnoresSpaces
    , testToken "Int Token" intTokenExamples intToken intTokenCounterexamples IgnoresSpaces
    , testToken "GlobalName" globalNameExamples globalName globalNameCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "LocalName" localNameExamples localName localNameCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "LocalNames" localNamesExamples localNames localNamesCounterexamples NoConcatenationOrSpaces
    , testToken "LValue" lValueExamples lValue lValueCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "NonEmptyLValues" nonEmptyLValuesExamples nonEmptyLValues nonEmptyLValuesCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "Operand" operandExamples operand operandCounterexamples IgnoresSpaces
    , testToken "Operands" operandsExamples operands operandsCounterexamples NoConcatenationOrSpaces
    , testToken "NonEmptyOperands" nonEmptyOperandsExamples nonEmptyOperands nonEmptyOperandsCounterexamples ConcatenationAndIgnoresSpaces
    ]

instructionTests :: [TestTree]
instructionTests =
  [ testInstruction
      "Assign"
      AssignInst
        { assignLeft = LValueLocalName (LocalName (IeleName "a"))
        , assignRight = OperandInt (IntToken 10)
        }
      assignInst
      InstructionAssign
      "%a=10"
  , testInstruction
      "Load"
      LoadInst
        { loadLeft = LValueLocalName (LocalName (IeleName "a"))
        , loadIndex = OperandInt (IntToken 10)
        , loadSizeInBytes = OperandInt (IntToken 11)
        }
      loadInst
      InstructionLoad
      "%a=load 10, 11"
  , testInstruction
      "Store"
      StoreInst
        { storeValue = OperandInt (IntToken 9)
        , storeIndex = OperandInt (IntToken 10)
        , storeSizeInBytes = OperandInt (IntToken 11)
        }
      storeInst
      InstructionStore
      "store 9, 10, 11"
  , testInstruction
      "SLoad"
      SLoadInst
        { sloadLeft = LValueLocalName (LocalName (IeleName "a"))
        , sloadIndex = OperandInt (IntToken 10)
        , sloadSizeInBytes = OperandInt (IntToken 11)
        }
      sloadInst
      InstructionSLoad
      "%a=sload 10, 11"
  , testInstruction
      "SStore"
      SStoreInst
        { sstoreValue = OperandInt (IntToken 9)
        , sstoreIndex = OperandInt (IntToken 10)
        , sstoreSizeInBytes = OperandInt (IntToken 11)
        }
      sstoreInst
      InstructionSStore
      "sstore 9, 10, 11"
  , testInstruction
      "IsZero"
      IsZeroInst
        { isZeroLeft = LValueLocalName (LocalName (IeleName "a"))
        , isZeroRight = OperandInt (IntToken 10)
        }
      isZeroInst
      InstructionIsZero
      "%a=iszero 10"
  , testInstruction
      "Not"
      NotInst
        { notLeft = LValueLocalName (LocalName (IeleName "a"))
        , notRight = OperandInt (IntToken 10)
        }
      notInst
      InstructionNot
      "%a = not 10"
  , testBinaryOperation "add" AddInst
  , testBinaryOperation "mul" MulInst
  , testBinaryOperation "sub" SubInst
  , testBinaryOperation "div" DivInst
  , testBinaryOperation "exp" ExpInst
  , testBinaryOperation "mod" ModInst
  , testBinaryOperation "and" AndInst
  , testBinaryOperation "or" OrInst
  , testBinaryOperation "xor" XorInst
  , testBinaryOperation "sha3" Sha3Inst
  , testBinaryOperation "byte" ByteInst
  , testBinaryOperation "sext" SExtInst
  , testBinaryOperation "twos" TwosInst
  , testTernaryOperation "addmod" AddModInst
  , testTernaryOperation "mulmod" MulModInst
  , testTernaryOperation "expmod" ExpModInst
  , testPredicateOperation "le" LePredicateInst
  , testPredicateOperation "lt" LtPredicateInst
  , testPredicateOperation "ge" GePredicateInst
  , testPredicateOperation "gt" GtPredicateInst
  , testPredicateOperation "eq" EqPredicateInst
  , testPredicateOperation "ne" NePredicateInst
  , testInstruction
      "Jump"
      (JumpInst (IeleName "a"))
      jumpInst
      InstructionJump
      "br a"
  , testInstruction
      "CondJump"
      CondJumpInst
        { condJumpOperand = OperandInt (IntToken 10)
        , condJumpLabel = IeleName "a"
        }
      condJumpInst
      InstructionCondJump
      "br 10, a"
  , testInstruction
      "LocalCall"
      LocalCallInst
        { localCallLValues =
          LValues
            [ LValueLocalName (LocalName (IeleName "a"))
            , LValueGlobalName (GlobalName (IeleName "b"))]
        , localCallName = GlobalName (IeleName "c")
        , localCallOperands =
          Operands
            [ OperandInt (IntToken 10)
            , OperandInt (IntToken 11)]
        }
      localCallInst
      InstructionLocalCall
      "%a,@b=call@c(10,11)"
  , testInstruction
      "empty arguments LocalCall"
      LocalCallInst
        { localCallLValues =
          LValues
            [ LValueLocalName (LocalName (IeleName "a"))
            , LValueGlobalName (GlobalName (IeleName "b"))]
        , localCallName = GlobalName (IeleName "c")
        , localCallOperands = Operands []
        }
      localCallInst
      InstructionLocalCall
      "%a,@b=call@c()"
  , testInstruction
      "empty lvalues LocalCall"
      LocalCallInst
        { localCallLValues = LValues []
        , localCallName = GlobalName (IeleName "c")
        , localCallOperands =
          Operands
            [ OperandInt (IntToken 10)
            , OperandInt (IntToken 11)
            ]
    }
      localCallInst
      InstructionLocalCall
      "call@c(10,11)"
  , testInstruction
      "empty lvalues arguments LocalCall"
      LocalCallInst
        { localCallLValues = LValues []
        , localCallName = GlobalName (IeleName "c")
        , localCallOperands = Operands []
        }
      localCallInst
      InstructionLocalCall
      "call@c()"
  , testInstruction
      "AccountCall"
      AccountCallInst
        { accountCallLValues =
          LValues
            [ LValueLocalName (LocalName (IeleName "a"))
            , LValueGlobalName (GlobalName (IeleName "b"))]
        , accountCallName = GlobalName (IeleName "c")
        , accountCallAddress = OperandInt (IntToken 9)
        , accountCallOperands =
          Operands
            [ OperandInt (IntToken 10)
            , OperandInt (IntToken 11)]
        , accountCallSend = OperandInt (IntToken 12)
        , accountCallGasLimit = OperandInt (IntToken 13)
        }
      accountCallInst
      InstructionAccountCall
      "%a,@b=call@c at 9(10,11)send 12, gaslimit 13"
  , testInstruction
      "empty arguments AccountCall"
      AccountCallInst
        { accountCallLValues =
          LValues
            [ LValueLocalName (LocalName (IeleName "a"))
            , LValueGlobalName (GlobalName (IeleName "b"))]
        , accountCallName = GlobalName (IeleName "c")
        , accountCallAddress = OperandInt (IntToken 9)
        , accountCallOperands = Operands []
        , accountCallSend = OperandInt (IntToken 12)
        , accountCallGasLimit = OperandInt (IntToken 13)
        }
      accountCallInst
      InstructionAccountCall
      "%a,@b=call@c at 9()send 12, gaslimit 13"
  , testInstruction
      "empty lvalues AccountCall"
      AccountCallInst
        { accountCallLValues = LValues []
        , accountCallName = GlobalName (IeleName "c")
        , accountCallAddress = OperandInt (IntToken 9)
        , accountCallOperands =
          Operands
            [ OperandInt (IntToken 10)
            , OperandInt (IntToken 11)]
        , accountCallSend = OperandInt (IntToken 12)
        , accountCallGasLimit = OperandInt (IntToken 13)
        }
      accountCallInst
      InstructionAccountCall
      "call@c at 9(10,11)send 12, gaslimit 13"
  , testInstruction
      "empty lvalues arguments AccountCall"
      AccountCallInst
        { accountCallLValues = LValues []
        , accountCallName = GlobalName (IeleName "c")
        , accountCallAddress = OperandInt (IntToken 9)
        , accountCallOperands = Operands []
        , accountCallSend = OperandInt (IntToken 12)
        , accountCallGasLimit = OperandInt (IntToken 13)
        }
      accountCallInst
      InstructionAccountCall
      "call@c at 9()send 12, gaslimit 13"
  , testInstruction
      "Send"
      SendInst
        { sendValue = OperandInt (IntToken 10)
        , sendDestinationAccount = OperandInt (IntToken 11)
        }
      sendInst
      InstructionSend
      "send 10 to 11"
  , testInstruction
      "Return"
      (ReturnInst
        (Operands [OperandInt (IntToken 10), OperandInt (IntToken 11)])
      )
      returnInst
      InstructionReturn
      "ret (10, 11)"
  , testInstruction
      "empty Return"
      (ReturnInst (Operands []))
      returnInst
      InstructionReturn
      "ret void"
  , testInstruction
      "Revert"
      (RevertInst
        (Operands [OperandInt (IntToken 10), OperandInt (IntToken 11)])
      )
      revertInst
      InstructionRevert
      "revert (10, 11)"
  , testInstruction
      "empty Revert"
      (RevertInst (Operands []))
      revertInst
      InstructionRevert
      "revert void"
  , testInstruction
      "Stop"
      StopInst
      stopInst
      InstructionStop
      "stop"
  , testInstruction
      "Log"
      LogInst
        { logIndex = OperandInt (IntToken 10)
        , logSizeInBytes = OperandInt (IntToken 11)
        , logContent =
            Operands [OperandInt (IntToken 12), OperandInt (IntToken 13)]
        }
      logInst
      InstructionLog
      "log 10,11,12,13"
  , testInstruction
      "empty Log"
      LogInst
        { logIndex = OperandInt (IntToken 10)
        , logSizeInBytes = OperandInt (IntToken 11)
        , logContent = Operands []
        }
      logInst
      InstructionLog
      "log 10,11"
  , testInstruction
      "CreateInst"
      CreateInst
        { createLValue = LValueLocalName (LocalName (IeleName "a"))
        , createContractName = IeleName "b"
        , createOperands =
            Operands [OperandInt (IntToken 10), OperandInt (IntToken 11)]
        , createSendValue = OperandInt (IntToken 12)
        }
      createInst
      InstructionCreate
      "%a=create b(10,11) send 12"
  , testInstruction
      "SelfDestruct"
      SelfdestructInst
        {selfdestructAccountToSendBalance = OperandInt (IntToken 10)}
      selfDestructInst
      InstructionSelfdestruct
      "selfdestruct 10"
  ]

otherTests :: [TestTree]
otherTests =
  [ testCase
      "empty LabelledBlock"
      (parseSuccess
        LabeledBlock
          { labeledBlockLabel = IeleName "a"
          , labeledBlockInstructions = Instructions []
          }
        labeledBlock
        "a:"
      )
  , testCase
      "LabelledBlock"
      (parseSuccess
        LabeledBlock
          { labeledBlockLabel = IeleName "a"
          , labeledBlockInstructions =
              Instructions [InstructionStop StopInst, InstructionStop StopInst]
          }
        labeledBlock
        "a:stop stop"
      )
  , testCase
      "empty LabelledBlocks"
      (parseSuccess
        (LabeledBlocks [])
        labeledBlocks
        ""
      )
  , testCase
      "LabelledBlocks"
      (parseSuccess
        (LabeledBlocks
          [ LabeledBlock
              { labeledBlockLabel = IeleName "a"
              , labeledBlockInstructions =
                  Instructions [InstructionStop StopInst]
              }
          , LabeledBlock
              { labeledBlockLabel = IeleName "a"
              , labeledBlockInstructions = Instructions []
              }
          , LabeledBlock
              { labeledBlockLabel = IeleName "a"
              , labeledBlockInstructions =
                  Instructions [InstructionStop StopInst]
              }
          ]
        )
        labeledBlocks
        "a:stop a:a:stop"
      )
  , testCase
      "stop label in LabelledBlocks"
      (parseSuccess
        (LabeledBlocks
          [ LabeledBlock
              { labeledBlockLabel = IeleName "a"
              , labeledBlockInstructions =
                  Instructions [InstructionStop StopInst]
              }
          , LabeledBlock
              { labeledBlockLabel = IeleName "stop"
              , labeledBlockInstructions =
                  Instructions [InstructionStop StopInst]
              }
          ]
        )
        labeledBlocks
        "a:stop stop :stop"
      )
  , testCase
      "empty FunctionParameters"
      (parseSuccess
        (FunctionParameters (LocalNames []))
        functionParameters
        ""
      )
  , testCase
      "FunctionParameters"
      (parseSuccess
        (FunctionParameters
          (LocalNames [LocalName (IeleName "a"), LocalName (IeleName "b")])
        )
        functionParameters
        "%a,%b"
      )
  , testCase
      "empty parameters FunctionSignature"
      (parseSuccess
        FunctionSignature
          { functionSignatureName = GlobalName (IeleName "a")
          , functionSignatureParameters = FunctionParameters (LocalNames [])
          }
        functionSignature
        "@a()"
      )
  , testCase
      "FunctionSignature"
      (parseSuccess
        FunctionSignature
          { functionSignatureName = GlobalName (IeleName "a")
          , functionSignatureParameters =
              FunctionParameters
                (LocalNames
                  [LocalName (IeleName "a"), LocalName (IeleName "b")])
          }
        functionSignature
        "@a(%a,%b)"
      )
  , testCase
      "FunctionDefinition"
      (parseSuccess
        FunctionDefinition
          { functionDefinitionSignature =
              FunctionSignature
                { functionSignatureName = GlobalName (IeleName "a")
                , functionSignatureParameters =
                    FunctionParameters (LocalNames [])
                }
          , functionDefinitionBlocks =
              LabeledBlocks
                [ LabeledBlock
                    { labeledBlockLabel = IeleName "b"
                    , labeledBlockInstructions =
                        Instructions [InstructionStop StopInst]
                    }
                ]
          }
        functionDefinition
        "define@a(){b:stop}"
      )
  , testCase
      "GlobalVariableDefinition"
      (parseSuccess
        GlobalVariableDefinition
          { globalVariableName = GlobalName (IeleName "a")
          , globalVariableValue = IntToken 10
          }
        globalVariableDefinition
        "@a=10"
      )
  , testCase
      "function TopLevelDefinition"
      (parseSuccess
        (TopLevelDefinitionFunction
          FunctionDefinition
            { functionDefinitionSignature =
                FunctionSignature
                  { functionSignatureName = GlobalName (IeleName "a")
                  , functionSignatureParameters =
                      FunctionParameters (LocalNames [])
                  }
            , functionDefinitionBlocks =
                LabeledBlocks
                  [ LabeledBlock
                      { labeledBlockLabel = IeleName "b"
                      , labeledBlockInstructions =
                          Instructions [InstructionStop StopInst]
                      }
                  ]
            }
        )
        topLevelDefinition
        "define@a(){b:stop}"
      )
  , testCase
      "global variable TopLevelDefinition"
      (parseSuccess
        (TopLevelDefinitionGlobalVariable
          GlobalVariableDefinition
            { globalVariableName = GlobalName (IeleName "a")
            , globalVariableValue = IntToken 10
            }
        )
        topLevelDefinition
        "@a=10"
      )
  , testCase
      "TopLevelDefinitions"
      (parseSuccess
        (TopLevelDefinitions
          [ TopLevelDefinitionFunction
              FunctionDefinition
                { functionDefinitionSignature =
                    FunctionSignature
                      { functionSignatureName = GlobalName (IeleName "a")
                      , functionSignatureParameters =
                          FunctionParameters (LocalNames [])
                      }
                , functionDefinitionBlocks =
                    LabeledBlocks
                      [ LabeledBlock
                          { labeledBlockLabel = IeleName "b"
                          , labeledBlockInstructions =
                              Instructions [InstructionStop StopInst]
                          }
                      ]
                }
          , TopLevelDefinitionGlobalVariable
              GlobalVariableDefinition
                { globalVariableName = GlobalName (IeleName "a")
                , globalVariableValue = IntToken 10
                }
          ]
        )
        topLevelDefinitions
        "define@a(){b:stop}@a=10"
      )
  ]

------------------------------------
-- Generic test utilities
------------------------------------

parseSuccess :: (Show a, Eq a) => a -> Parser a -> String -> Assertion
parseSuccess expected parser input =
  assertEqual
    "Expecting parse success!"
    (Right expected)
    (parse parser "" input)

parseFailure :: (Show a, Eq a) => Parser a -> String -> Assertion
parseFailure parser input =
  assertBool "Expecting parse failure!" (isLeft (parse parser "" input))

------------------------------------
-- Instruction test utilities
------------------------------------

testBinaryOperation :: String -> BinaryOperationType ->  TestTree
testBinaryOperation name operationType =
  testInstruction
    name
    BinaryOperationInst
      { binaryOperationType = operationType
      , binaryOperationLeft = LValueLocalName (LocalName (IeleName "a"))
      , binaryOperationFirst = OperandInt (IntToken 10)
      , binaryOperationSecond = OperandInt (IntToken 11)
      }
    binaryOperationInst
    InstructionBinaryOperation
    ("%a=" ++ name ++ " 10,11")

testPredicateOperation :: String -> PredicateOperationType ->  TestTree
testPredicateOperation name operationType =
  testInstruction
    name
    PredicateOperationInst
      { predicateOperationType = operationType
      , predicateOperationLeft = LValueLocalName (LocalName (IeleName "a"))
      , predicateOperationFirst = OperandInt (IntToken 10)
      , predicateOperationSecond = OperandInt (IntToken 11)
      }
    predicateOperationInst
    InstructionPredicateOperation
    ("%a=cmp " ++ name ++ " 10,11")

testTernaryOperation :: String -> TernaryOperationType ->  TestTree
testTernaryOperation name operationType =
  testInstruction
    name
    TernaryOperationInst
      { ternaryOperationType = operationType
      , ternaryOperationLeft = LValueLocalName (LocalName (IeleName "a"))
      , ternaryOperationFirst = OperandInt (IntToken 10)
      , ternaryOperationSecond = OperandInt (IntToken 11)
      , ternaryOperationDivisor = OperandInt (IntToken 12)
      }
    ternaryOperationInst
    InstructionTernaryOperation
    ("%a=" ++ name ++ " 10,11, 12")

testInstruction :: (Show a, Eq a) =>
  String -> a -> Parser a -> (a -> Instruction)-> String -> TestTree
testInstruction name expectedValue parser instructionConstructor input =
  let expectedAsInstruction = instructionConstructor expectedValue
  in
    testGroup
      name
      [ testCase
          ("Simple parse '" ++ input ++ "'")
          (parseSuccess expectedValue parser input)
      , testCase
          ("Double parse '" ++ input ++ "'")
          ( parseSuccess [expectedValue, expectedValue]
              (many parser)
              (input ++ " " ++ input)
          )
      , testCase
          ("Parse as instruction '" ++ input ++ "'")
          (parseSuccess expectedAsInstruction instruction input)
      , testCase
          ("Parse as instructions '" ++ input ++ "'")
          ( parseSuccess
              (Instructions [expectedAsInstruction, expectedAsInstruction])
              instructions
              (input ++ " " ++ input)
          )
      ]


------------------------------------
-- Token test utilities
------------------------------------

data TokenType =
    ConcatenationAndIgnoresSpaces
  | Concatenation
  | IgnoresSpaces
  | NoConcatenationOrSpaces

singleCharTests :: Char -> Parser Char -> String -> TestTree
singleCharTests ch parser unparseable =
  testToken
    (ch : " Tests")
    [(ch, ch : "")]
    parser
    (map ( : "") unparseable)
    ConcatenationAndIgnoresSpaces

singleCharsTests :: String -> String -> Parser Char -> String -> TestTree
singleCharsTests name chars parser unparseable =
  testToken
    (name ++ " Tests")
    (map (\x -> (x, x : "")) chars)
    parser
    (map ( : "") unparseable)
    Concatenation

testToken :: (Show a, Eq a) =>
  String -> [(a, String)] -> Parser a -> [String] -> TokenType -> TestTree
testToken name inputAndExpected parser unparseable tokenType =
  testGroup name (tokenTestItems inputAndExpected parser unparseable tokenType)

negativeTokenTestCase :: (Show a, Eq a) => Parser a -> String -> TestTree
negativeTokenTestCase parser input =
  testCase ("Should not parse " ++ input) (parseFailure parser input)

negativeTokenTestItems :: (Show a, Eq a) => Parser a -> [String] -> [TestTree]
negativeTokenTestItems parser unparseable =
  map (negativeTokenTestCase parser) unparseable

positiveTokenTestCase :: (Show a, Eq a) => Parser a -> (a, String) -> TestTree
positiveTokenTestCase parser (expectedValue, input) =
  testCase
    ("Simple parse '" ++ input ++ "'")
    (parseSuccess expectedValue parser input)

positiveTokenTestItems :: (Show a, Eq a) =>
  Parser a -> [(a, String)] -> [TestTree]
positiveTokenTestItems parser expectedValues =
  map (positiveTokenTestCase parser) expectedValues

specialTokenTestItems :: (Show a, Eq a) =>
  Parser a -> [(a, String)] -> TokenType -> [TestTree]
specialTokenTestItems _ [] _ = []
specialTokenTestItems _ _ NoConcatenationOrSpaces = []
specialTokenTestItems parser ((expectedValue, input) : _) IgnoresSpaces =
  [ positiveTokenTestCase
      (many parser)
      ([expectedValue, expectedValue], input ++ " \n\t" ++ input)
  ]
specialTokenTestItems parser ((expectedValue, input) : _) Concatenation =
  [ positiveTokenTestCase
      (many parser)
      ([expectedValue, expectedValue], input ++ input)
  ]
specialTokenTestItems parser inputs ConcatenationAndIgnoresSpaces =
     specialTokenTestItems parser inputs Concatenation
  ++ specialTokenTestItems parser inputs IgnoresSpaces

tokenTestItems :: (Show a, Eq a) =>
  [(a, String)] -> Parser a -> [String] -> TokenType -> [TestTree]
tokenTestItems positiveTests parser negativeTests tokenType =
     positiveTokenTestItems parser positiveTests
  ++ specialTokenTestItems parser positiveTests tokenType
  ++ negativeTokenTestItems parser negativeTests
