{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (LT,EQ,GT)

import Test.Tasty
import Test.Tasty.HUnit

import IeleParserImplementation
import IeleTypes hiding (instructions)
import IeleInstructions

import Control.Applicative (many)
import Data.Either (isLeft)
import Text.Parsec (parse, eof)
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
  [ (IeleNameText "test", "test")
  , (IeleNameText "-1.bc7", "-1.bc7")
  , (IeleNameNumber 1234, "1234")
  , (IeleNameText "-1", "-1")
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
  , (IntToken 10, "0x00a")
  , (IntToken (-127), "-0x7f")
  , (IntToken 0, "false")
  , (IntToken 1, "true")
  ]

intTokenCounterexamples :: [String]
intTokenCounterexamples =
  ["@a", "12a", "bcd", "12.5"]

globalNameExamples :: [(GlobalName, String)]
globalNameExamples =
  [ (GlobalName "0", "@0")
  , (GlobalName "a", "@a")
  , (GlobalName "a.bc", "@a.bc")
  , (GlobalName "ab", "@ ab")
  ]

globalNameCounterexamples :: [String]
globalNameCounterexamples =
  globalNameInclReservedCounterexamples ++ ["@iele.foo"]

globalNameInclReservedExamples :: [(GlobalName, String)]
globalNameInclReservedExamples =
  [(GlobalName "iele.foo","@iele.foo")] ++ globalNameExamples

globalNameInclReservedCounterexamples :: [String]
globalNameInclReservedCounterexamples =
  ["@", "a", "@@bcd", "@@", "%a"]


localNameExamples :: [(LocalName, String)]
localNameExamples =
  [ (LocalName (IeleNameNumber 0), "%0")
  , (LocalName (IeleNameText "a"), "%a")
  , (LocalName (IeleNameText "a.bc"), "%a.bc")
  , (LocalName (IeleNameText "ab"), "% ab")
  ]

localNameCounterexamples :: [String]
localNameCounterexamples =
  ["%", "a", "%%bcd", "%%", "@a", "%iele.localName"]

lValueExamples :: [(LValue, String)]
lValueExamples =
  [ (LValueLocalName (LocalName (IeleNameText "a")), "%a")
  ]

lValueCounterexamples :: [String]
lValueCounterexamples =
  ["%", "@a", "a", "@"]

nonEmptyLValuesExamples :: [([LValue], String)]
nonEmptyLValuesExamples =
    [ ([LValueLocalName "%a"], "%a")
    , ([LValueLocalName "%a", LValueLocalName "%b"], "%a, %b")
    ]

nonEmptyLValuesCounterexamples :: [String]
nonEmptyLValuesCounterexamples =
  ["%", "@", ""]

lvaluesExamples :: [([LValue], String)]
lvaluesExamples =
  [ ([LValueLocalName "%a"], "%a")
  , ([LValueLocalName "%a", LValueLocalName "%b"], "%a,%b")
  , ([LValueLocalName "%a", LValueLocalName "%b"], "%a, %b")
  , ([], "")
  ]

lvaluesCounterexamples :: [String]
lvaluesCounterexamples =
  ["%", "@", "@a"]

operandExamples :: [(Operand,String)]
operandExamples =
  [ (RegOperand "%b", "%b")
  , (GlobalOperand "@1", "@1")
  , (ImmOperand (IntToken 1), "true")
  , (ImmOperand (IntToken 0), "false")
  , (ImmOperand (IntToken 10), "10")
  , (ImmOperand (IntToken (-10)), "-10")
  ]

operandCounterexamples =
  ["abc","%iele.reserved","%a %a","@iele.reserved"]

functionParametersExamples :: [([LValue], String)]
functionParametersExamples =
  [ (["%a"], "(%a)")
  , (["%a", "%b"], "( %a,%b)")
  , (["%a", "%b"], "(%a, %b )")
  , ([], "()")
  ]

functionParametersCounterexamples :: [String]
functionParametersCounterexamples =
  ["%"]

tokenTests :: [TestTree]
tokenTests =
    [ singleCharTests '@' at "a?.1"
    , singleCharTests ')' closeParenthesis "a?.1"
    , singleCharTests '}' closeCurlyBrace "a?.1"
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
    , testToken "GlobalNameInclReserved"
        globalNameInclReservedExamples globalNameInclReserved globalNameInclReservedCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "LocalName" localNameExamples localName localNameCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "LValue" lValueExamples lValue lValueCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "LValues" lvaluesExamples lValues lvaluesCounterexamples NoConcatenationOrSpaces
    , testToken "NonEmptyLValues" nonEmptyLValuesExamples nonEmptyLValues nonEmptyLValuesCounterexamples ConcatenationAndIgnoresSpaces
    , testToken "Operand" operandExamples operand operandCounterexamples ConcatenationAndIgnoresSpaces
    ]

instructionTests :: [TestTree]
instructionTests =
  [ testIeleInst
      "Load Immediate Pos"
      (LiOp LOADPOS "%a" 10)
      "%a=10"
  , testIeleInst
      "Load Immediate Neg"
      (LiOp LOADNEG "%a" 10)
      "%a=-10"
  , testIeleInst
      "Move"
      (Op  MOVE "%a" ["%b"])
      "%a=%b"
  , testIeleInst
      "Load"
      (Op MLOAD "%a" ["%10"])
      "%a=load %10"
  , testIeleInst
      "LoadN"
      (Op MLOADN "%a" ["%10","%off","%size"])
      "%a=load %10, %off, %size"
  , testIeleInst
      "Store"
      (VoidOp MSTORE ["%9","%10"])
      "store %9, %10"
  , testIeleInst
      "StoreN"
      (VoidOp MSTOREN ["%9","%10","%off","%size"])
      "store %9, %10, %off, %size"
  , testIeleInst
      "SLoad"
      (Op SLOAD "%a" ["%10"])
      "%a=sload %10"
  , testIeleInst
      "SStore"
      (VoidOp SSTORE ["%9","%10"])
      "sstore %9, %10"
  , testIeleInst
      "IsZero"
      (Op ISZERO "%a" ["%10"])
      "%a=iszero %10"
  , testIeleInst
      "Not"
      (Op NOT "%a" ["%b"])
      "%a = not %b"
  , testIeleInst
      "Log2"
      (Op LOG2 "%a" ["%b"])
      "%a = log2 %b"
  , testBinaryOperation "add" ADD
  , testBinaryOperation "mul" MUL
  , testBinaryOperation "sub" SUB
  , testBinaryOperation "div" DIV
  , testBinaryOperation "exp" EXP
  , testBinaryOperation "mod" MOD

  , testTernaryOperation "addmod" ADDMOD
  , testTernaryOperation "mulmod" MULMOD
  , testTernaryOperation "expmod" EXPMOD

  , testBinaryOperation "byte" BYTE
  , testBinaryOperation "shift" SHIFT
  , testBinaryOperation "sext" SIGNEXTEND
  , testBinaryOperation "twos" TWOS
  , testBinaryOperation "bswap" BSWAP

  , testBinaryOperation "and" AND
  , testBinaryOperation "or" OR
  , testBinaryOperation "xor" XOR

  , testPredicateOperation "lt" LT
  , testPredicateOperation "le" LE
  , testPredicateOperation "gt" GT
  , testPredicateOperation "ge" GE
  , testPredicateOperation "eq" EQ
  , testPredicateOperation "ne" NE

  , testIeleInst "sha3"
      (Op SHA3 "%a" ["%b"])
      "%a = sha3 %b"

  , testIeleInst
      "Jump"
      (VoidOp (JUMP "a") [])
      "br a"
  , testIeleInst
      "CondJump"
      (VoidOp (JUMPI "a") ["%10"])
      "br %10, a"

  , testIeleInst
      "LocalCall"
      (CallOp (LOCALCALL "@c" (mkArgs 2) (mkRets 2)) ["%a","%b"] ["%10","%11"])
      "%a,%b=call@c(%10,%11)"
  , testIeleInst
      "empty arguments LocalCall"
      (CallOp (LOCALCALL "@c" (mkArgs 0) (mkRets 2)) ["%a","%b"] [])
      "%a,%b=call@c()"
  , testIeleInst
      "empty results LocalCall"
      (CallOp (LOCALCALL "@c" (mkArgs 2) (mkRets 0)) [] ["%10","%11"])
      "call@c(%10,%11)"
  , testIeleInst
      "empty lvalues arguments LocalCall"
      (CallOp (LOCALCALL "@c" (mkArgs 0) (mkRets 0)) [] [])
      "call@c()"

  , testIeleInst
      "AccountCall"
      (CallOp (CALL "@c" (mkArgs 2) (mkRets (3-1))) ["%ok","%a","%b"] ["%gas","%acct","%amt","%10","%11"])
      "%ok,%a,%b=call@c at %acct(%10,%11)send %amt, gaslimit %gas"
  , testIeleInst
      "empty arguments AccountCall"
      (CallOp (CALL "@c" (mkArgs 0) (mkRets (2-1))) ["%ok","%b"] ["%gas","%acct","%amt"])
      "%ok , %b = call @c at %acct() send %amt, gaslimit %gas"
  , testIeleInst
      "StaticCall"
      (CallOp (STATICCALL "@c" (mkArgs 2) (mkRets (3-1))) ["%ok","%a","%b"] ["%gas","%acct","%amt","%10","%11"])
      "%ok,%a,%b=staticcall@c at %acct(%10,%11)send %amt, gaslimit %gas"
  , testIeleInst
      "empty arguments StaticCall"
      (CallOp (STATICCALL "@c" (mkArgs 0) (mkRets (2-1))) ["%ok","%b"] ["%gas","%acct","%amt"])
      "%ok , %b = staticcall @c at %acct() send %amt, gaslimit %gas"
  , testCase
      "reject empty lvalues AccountCall"
      (parseFailure instruction
        "call@c at %acct(%10,%11)send %amt, gaslimit %gas")
  , testIeleInst
      "Return"
      (VoidOp (RETURN (mkArgs 2)) ["%10","%11"])
      "ret %10, %11"
  , testIeleInst
      "empty Return"
      (VoidOp (RETURN (mkArgs 0)) [])
      "ret void"
  , testIeleInst
      "Revert"
      (VoidOp (REVERT (mkArgs 1)) ["%10"])
      "revert %10"
  , testIeleInst
      "Log 0"
      (VoidOp (LOG 0) ["%idx"])
      "log %idx"
  , testIeleInst
      "Log 1"
      (VoidOp (LOG 1) ["%idx","%1"])
      "log %idx, %1"
  , testIeleInst
      "Log 2"
      (VoidOp (LOG 2) ["%idx","%1","%2"])
      "log %idx, %1, %2"
  , testIeleInst
      "Log 3"
      (VoidOp (LOG 3) ["%idx","%1","%2","%3"])
      "log %idx, %1, %2, %3"
  , testIeleInst
      "Log 4"
      (VoidOp (LOG 4) ["%idx","%1","%2","%3","%4"])
      "log %idx, %1, %2, %3, %4"
  , testCase
      "Reject Log 5"
      (parseFailure (instruction <* eof) "log %idx, %1, %2, %3, %4, %5")
  , testIeleInst
      "create"
      (CallOp (CREATE "b" (mkArgs 2)) ["%a","%b"] ["%12","%10","%11"])
      "%a,%b=create b(%10,%11) send %12"
  , testIeleInst
      "copycreate"
      (CallOp (COPYCREATE (mkArgs 2)) ["%a","%b"] ["%val","%acct","%10","%11"])
      "%a,%b=copycreate %acct(%10,%11) send %val"
  , testIeleInst
      "SelfDestruct"
      (VoidOp SELFDESTRUCT ["%10"])
      "selfdestruct %10"
  ]

otherTests :: [TestTree]
otherTests =
  let dummy = LiOp LOADPOS "%0" 0 in
  [ testCase
      "empty LabelledBlock"
      (parseSuccess
        (LabeledBlock "a" [])
        labeledBlock
        "a:"
      )
  , testCase
      "LabelledBlock"
      (parseSuccess
        (LabeledBlock "a" [dummy,dummy])
        labeledBlock
        "a:%0=0 %0=0"
      )
  , testCase
      "empty LabelledBlocks"
      (parseSuccess
        []
        labeledBlocks
        ""
      )
  , testCase
      "LabelledBlocks"
      (parseSuccess
        [ LabeledBlock "a" [dummy]
        , LabeledBlock "a" []
        , LabeledBlock "a" [dummy]
        ]
        labeledBlocks
        "a:%0=0 a:a:%0=0"
      )
  , testCase
      "stop label in LabelledBlocks"
      (parseSuccess
        [ LabeledBlock "a" [dummy]
        , LabeledBlock "stop" [dummy]
        ]
        labeledBlocks
        "a:%0=0 stop :%0=0"
      )
  , testCase
      "empty FunctionParameters"
      (parseSuccess
        []
        functionParameters
        "()"
      )
  , testCase
      "FunctionParameters"
      (parseSuccess
        ["%a", "%b"]
        functionParameters
        "(%a,%b)"
      )
  , testToken "FunctionParameters" functionParametersExamples functionParameters functionParametersCounterexamples NoConcatenationOrSpaces
  , testCase
      "FunctionDefinition"
      (parseSuccess
        (FunctionDefinition False "@a" []
          [dummy]
          [ LabeledBlock "b" [dummy]])
        functionDefinition
        "define@a(){%0=0 b:%0=0}"
      )
  , testCase
      "public function definition"
      (parseSuccess
        (FunctionDefinition True "@10" []
          [dummy]
          [ LabeledBlock "b" [dummy] ])
        functionDefinition
        "define public @10(){%0=0 b:%0=0}"
      )
  , testCase
      "private function definition"
      (parseSuccess
        (TopLevelDefinitionFunction
          (FunctionDefinition False "@a" []
            []
            [ LabeledBlock "b" [dummy] ]))
        topLevelDefinition
        "define@a(){b:%0=0}"
      )
  , testCase
      "external contract declaration"
      (parseSuccess
        (TopLevelDefinitionContract "a")
        topLevelDefinition
        "external contract a"
      )
  , testCase
      "global definition"
      (parseSuccess
        (TopLevelDefinitionGlobal "@a" 12)
        topLevelDefinition
        "@a = 12")
  , testCase
      "TopLevelDefinitions"
      (parseSuccess
        [TopLevelDefinitionFunction
          (FunctionDefinition False "@a" []
            [dummy,dummy]
            [ LabeledBlock "b" [dummy,dummy]
            , LabeledBlock "c" [dummy]])
         , TopLevelDefinitionContract "a"
         , TopLevelDefinitionGlobal "@b" 10
         ]
        (many topLevelDefinition)
        "define@a(){%0=0 %0=0 b:%0=0 %0=0 c:%0=0}external contract a @b=10"
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
    (parse (parser <* eof) "" input)

parseFailure :: (Show a, Eq a) => Parser a -> String -> Assertion
parseFailure parser input =
  assertBool "Expecting parse failure!" (isLeft (parse (parser <* eof) "" input))

------------------------------------
-- IeleOp test utilities
------------------------------------

testBinaryOperation :: String -> IeleOpcode1 ->  TestTree
testBinaryOperation name op =
  testIeleInst
    name
    (Op op "%a" [RegOperand "%10",GlobalOperand "@b"])
    ("%a=" ++ name ++ " %10,@b")

testPredicateOperation :: String -> IeleOpcode1 ->  TestTree
testPredicateOperation name op =
  testIeleInst
    name
    (Op op "%a" [ImmOperand (IntToken 10),RegOperand "%11"])
    ("%a=cmp " ++ name ++ " 10,%11")

testTernaryOperation :: String -> IeleOpcode1 ->  TestTree
testTernaryOperation name op =
  testIeleInst
    name
    (Op op "%a" [RegOperand "%ab1",ImmOperand (IntToken 15),GlobalOperand "@12"])
    ("%a=" ++ name ++ " %ab1,15, @12")

testInstruction :: String -> Instruction -> String -> TestTree
testInstruction name expected input =
    testGroup
      name
      [ testCase
          ("Simple parse '" ++ input ++ "'")
          (parseSuccess expected (instruction <* eof) input)
      , testCase
          ("Double parse '" ++ input ++ "'")
          ( parseSuccess [expected, expected]
              (many instruction <* eof)
              (input ++ " " ++ input)
          )
      ]

testIeleInst :: String -> IeleOpP -> String -> TestTree
testIeleInst name expected input = testInstruction name expected input

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
