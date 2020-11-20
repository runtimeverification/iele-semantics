{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module IeleParserImplementation
    ( accountCallInst
    , assignInst
    , at
    , binaryInst
    , closeCurlyBrace
    , closeParenthesis
    , colon
    , comma
    , contract
    , createInst
    , equal
    , functionDefinition
    , functionParameters
    , globalName
    , globalNameInclReserved
    , ieleNameFirstChar
    , ieleNameNonFirstChar
    , ieleNameToken
    , ieleParser
    , ieleParser1
    , instruction
    , instructions
    , intToken
    , isZeroInst
    , log2Inst
    , jumpInst
    , labeledBlock
    , labeledBlocks
    , lexeme
    , loadInst
    , localCallInst
    , localName
    , logInst
    , lValue
    , lValues
    , operand
    , operands
    , minus
    , nonEmptyLValues
    , notInst
    , openCurlyBrace
    , openParenthesis
    , percent
    , predicateInst
    , returnInst
    , revertInst
    , selfDestructInst
    , skipKeyword
    , sloadInst
    , sstoreInst
    , storeInst
    , ternaryInst
    , topLevelDefinition
    , whitespace
    , withResult
    ) where
import Prelude hiding (LT,EQ,GT)
import Numeric(readHex)

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

import Control.Applicative ((<|>), (<*), many)
import Control.Monad (void)
import Text.Parsec (try, (<?>), skipMany, satisfy)
import Text.Parsec.Char (char, digit, hexDigit, letter, oneOf, noneOf, string)
import Text.Parsec.Combinator (eof, many1, choice, notFollowedBy, sepBy, sepBy1)
import Text.Parsec.String (Parser)

import Data.Word
import Data.Data
import Data.Char

import IeleTypes hiding (instructions)
import IeleInstructions

whitespace :: Parser ()
whitespace = void (many (spaceChar <|> lineComment))
 where
  spaceChar = void (oneOf " \n\t")
  lineComment = try (string "//") *> skipMany (satisfy (/='\n'))

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

comma :: Parser Char
comma = lexeme (char ',')

at :: Parser Char
at = lexeme (char '@')

percent :: Parser Char
percent = lexeme (char '%')

equal :: Parser Char
equal = lexeme (char '=')

minus :: Parser Char
minus = lexeme (char '-')

colon :: Parser Char
colon = lexeme (char ':')

openParenthesis :: Parser Char
openParenthesis = lexeme (char '(')

closeParenthesis :: Parser Char
closeParenthesis = lexeme (char ')')

parens :: Parser a -> Parser a
parens p = openParenthesis *> p <* closeParenthesis

openCurlyBrace :: Parser Char
openCurlyBrace = lexeme (char '{')

closeCurlyBrace :: Parser Char
closeCurlyBrace = lexeme (char '}')

braces :: Parser a -> Parser a
braces p = openCurlyBrace *> p <* closeCurlyBrace

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
commaSep0 :: Parser a -> Parser [a]
commaSep0 = (`sepBy` comma)

commaSep1 :: Parser a -> Parser [a]
commaSep1 = (`sepBy1` comma)

ieleNameFirstChar :: Parser Char
ieleNameFirstChar = letter <|> oneOf "._-$"

ieleNameNonFirstChar :: Parser Char
ieleNameNonFirstChar = ieleNameFirstChar <|> digit

skipKeyword :: String -> Parser ()
skipKeyword s = (try $ lexeme $ string s *> notFollowedBy ieleNameNonFirstChar)
  <?> s

---------------------------------------------

positiveInt :: Parser Int
positiveInt = lexeme (read <$> many1 digit <* notFollowedBy ieleNameNonFirstChar)
  <?> "number"

ieleNameTokenNormal :: Parser IeleName
ieleNameTokenNormal =
  lexeme $ IeleNameText <$> ((:) <$> ieleNameFirstChar <*> many ieleNameNonFirstChar)

ieleNameTokenNumber :: Parser IeleName
ieleNameTokenNumber =
    lexeme $ IeleNameNumber . read <$> many1 digit <* notFollowedBy ieleNameNonFirstChar

escape :: Parser Char
escape = do
  d <- char '\\'
  c1 <- oneOf "0123456789abcdefABCDEF"
  c2 <- oneOf "0123456789abcdefABCDEF"
  return (chr (((digitToInt c1) * 16) + (digitToInt c2)))

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

character :: Parser Char
character = nonEscape <|> escape

ieleNameTokenString :: Parser IeleName
ieleNameTokenString =
  lexeme $ do
    char '"'
    chars <- many character
    char '"'
    return (IeleNameText chars)

ieleNameTokenNormalOrString :: Parser IeleName
ieleNameTokenNormalOrString = ieleNameTokenNormal <|> ieleNameTokenString

ieleNameToken :: Parser IeleName
ieleNameToken = ieleNameTokenNormalOrString <|> ieleNameTokenNumber

positiveDecIntToken :: Parser IntToken
positiveDecIntToken = lexeme $ IntToken . read <$>
  many1 digit <* notFollowedBy ieleNameNonFirstChar

positiveHexIntToken :: Parser IntToken
positiveHexIntToken = lexeme $ do
  try (string "0x")
  body <- many1 hexDigit <* notFollowedBy ieleNameNonFirstChar
  case readHex body of
    [(v,"")] -> return (IntToken v)
    _ -> fail $ "bad hexadecimal literal 0x"++body

positiveIntToken :: Parser IntToken
positiveIntToken = positiveHexIntToken <|> positiveDecIntToken

negativeIntToken :: Parser IntToken
negativeIntToken = fmap (\(IntToken i) -> IntToken (negate i))
   (char '-' *> positiveIntToken)

trueToken :: Parser IntToken
trueToken = IntToken 1 <$ skipKeyword "true"

falseToken :: Parser IntToken
falseToken = IntToken 0 <$ skipKeyword "false"

intToken :: Parser IntToken
intToken = negativeIntToken <|> positiveIntToken <|> trueToken <|> falseToken
  <?> "literal value"

globalNameInclReserved :: Parser GlobalName
globalNameInclReserved = GlobalName <$ at <*> ieleNameToken

globalName :: Parser GlobalName
globalName = do
  name <- globalNameInclReserved
  case name of
    GlobalName (IeleNameText str) | take 5 str == "iele." ->
      fail $ "All names beginning \"@iele.\" are reserved"
    _ -> pure name

localName :: Parser LocalName
localName = do
  name <- percent *> ieleNameToken
  case name of
    IeleNameText str | take 5 str == "iele." ->
      fail $ "All names beginning \"%iele.\" are reserved"
    _ -> pure (LocalName name)

lValue :: Parser LValue
lValue = LValueLocalName <$> localName

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
lValues :: Parser [LValue]
lValues = commaSep0 lValue

operand :: Parser Operand
operand = RegOperand <$> lValue
      <|> ImmOperand <$> intToken
      <|> GlobalOperand <$> globalName

operandInclReserved :: Parser Operand
operandInclReserved = RegOperand <$> lValue
      <|> ImmOperand <$> intToken
      <|> GlobalOperand <$> globalNameInclReserved

operands :: Parser [Operand]
operands = commaSep0 operand

assignInst :: LValue -> Parser Instruction
assignInst result = build <$> operand
  where
   build (ImmOperand (IntToken i)) = loadImm result i
   build arg = Op MOVE result [arg]

withResult :: (LValue -> Parser a) -> Parser a
withResult p = try (lValue <* equal) >>= p

callInsts :: Parser IeleOpP
callInsts = try localCallInst
        <|> try accountCallInst
        <|> try staticCallInst
        <|> try callAddressInst

ieleOp1 :: Parser Instruction
ieleOp1 = do
  result <- try (lValue <* equal)
  choice [parser result | parser <- oneResults]
 where
  oneResults =
    [ assignInst
    , loadInst
    , sloadInst
    , isZeroInst
    , log2Inst
    , notInst
    , shaInst
    , binaryInst
    , ternaryInst
    , predicateInst]

ieleVoidOp :: Parser IeleOpP
ieleVoidOp = choice
  [ storeInst
  , sstoreInst
  , jumpInst
  , returnInst
  , revertInst
  , selfDestructInst
  , logInst
  ]
  <?> "instruction without result"

simpleOp :: String -> Int -> ([Operand] -> IeleOpP) -> Parser IeleOpP
simpleOp name arity build = build <$ skipKeyword name <*> simpleArgs arity

simpleArgs :: Int -> Parser [Operand]
simpleArgs 0 = pure []
simpleArgs arity = sequenceA (operand:replicate (arity-1) (comma *> operand))

simpleOp1 :: String -> Int -> IeleOpcode1 -> (LValue -> Parser IeleOpP)
simpleOp1 name arity opcode = simpleOp name arity . Op opcode

simpleOp0 :: String -> Int -> IeleOpcode0P -> Parser IeleOpP
simpleOp0 name arity opcode = simpleOp name arity (VoidOp opcode)

loadInst :: LValue -> Parser IeleOpP
loadInst result =
  try (simpleOp "load" 3 (Op MLOADN result))
   <|> simpleOp "load" 1 (Op MLOAD result)

storeInst :: Parser IeleOpP
storeInst =
  try (simpleOp0 "store" 4 MSTOREN)
  <|>  simpleOp0 "store" 2 MSTORE

sloadInst :: LValue -> Parser IeleOpP
sloadInst = simpleOp1 "sload" 1 SLOAD

sstoreInst :: Parser IeleOpP
sstoreInst = simpleOp0 "sstore" 2 SSTORE

jumpInst :: Parser IeleOpP
jumpInst = do
  skipKeyword "br"
  condition <- Just <$> operand <* comma <|> pure Nothing
  lbl <- ieleNameToken
  pure $ case condition of
    Nothing -> VoidOp (JUMP lbl) []
    Just reg -> VoidOp (JUMPI lbl) [reg]

isZeroInst :: LValue -> Parser IeleOpP
isZeroInst = simpleOp1 "iszero" 1 ISZERO

log2Inst :: LValue -> Parser IeleOpP
log2Inst = simpleOp1 "log2" 1 LOG2

notInst :: LValue -> Parser IeleOpP
notInst = simpleOp1 "not" 1 NOT

shaInst :: LValue -> Parser IeleOpP
shaInst = simpleOp1 "sha3" 1 SHA3

enumParser :: [(String,a)] -> Parser a
enumParser options =
  choice [val <$ skipKeyword name | (name,val) <- options]

binaryOperations :: [(String,IeleOpcode1)]
binaryOperations =
  [("add",ADD)
  ,("mul",MUL)
  ,("sub",SUB)
  ,("div",DIV)
  ,("mod",MOD)
  ,("exp",EXP)
  ,("and",AND)
  ,("or", OR)
  ,("xor",XOR)
  ,("byte",BYTE)
  ,("shift",SHIFT)
  ,("sext",SIGNEXTEND)
  ,("twos",TWOS)
  ,("bswap",BSWAP)
  ]

binaryInst :: LValue -> Parser IeleOpP
binaryInst result =
  choice [simpleOp1 name 2 opcode result | (name,opcode) <- binaryOperations]

ternaryOperators :: [(String, IeleOpcode1)]
ternaryOperators = [("addmod",ADDMOD)
                   ,("mulmod",MULMOD)
                   ,("expmod",EXPMOD)
                   ]

ternaryInst :: LValue -> Parser IeleOpP
ternaryInst result =
  flip Op result <$> enumParser ternaryOperators <*> simpleArgs 3

predicateFlags :: [(String, IeleOpcode1)]
predicateFlags = [("lt",LT)
                 ,("le",LE)
                 ,("gt",GT)
                 ,("ge",GE)
                 ,("eq",EQ)
                 ,("ne",NE)
                 ]

predicateInst :: LValue -> Parser IeleOpP
predicateInst result = skipKeyword "cmp" >>
  flip Op result <$> enumParser predicateFlags <*> simpleArgs 2

nonEmptyLValues :: Parser [LValue]
nonEmptyLValues = commaSep1 lValue

callResult :: Parser [LValue]
callResult =  nonEmptyLValues <* equal <|> pure []

builtins :: Map GlobalName (IeleOpcodeQuery,Int)
builtins = Map.fromList
  [bi "iele.callvalue" CALLVALUE 0
  ,bi "iele.gaslimit" GASLIMIT 0
  ,bi "iele.gasprice" GASPRICE 0
  ,bi "iele.gas" GAS 0
  ,bi "iele.address" ADDRESS 0
  ,bi "iele.origin" ORIGIN 0
  ,bi "iele.caller" CALLER 0
  ,bi "iele.codesize" CODESIZE 0
  ,bi "iele.beneficiary" BENEFICIARY 0
  ,bi "iele.timestamp" TIMESTAMP 0
  ,bi "iele.number" NUMBER 0
  ,bi "iele.difficulty" DIFFICULTY 0
  ,bi "iele.msize" MSIZE 0

  ,bi "iele.extcodesize" EXTCODESIZE 1
  ,bi "iele.blockhash" BLOCKHASH 1
  ,bi "iele.balance" BALANCE 1
  ]
 where bi name opcode arity = (GlobalName (IeleNameText name),(opcode,arity))

localCallInst :: Parser IeleOpP
localCallInst = do
  results <- callResult
  name <- skipKeyword "call" *> operandInclReserved
  args <- parens operands
  case name of
    GlobalOperand globalName ->
      let call = CallOp (LOCALCALL globalName (argsLength args) (retsLength results)) results args in
      case Map.lookup globalName builtins of
        Nothing | globalName /= GlobalName (IeleNameText "iele.invalid") -> pure call
                | not (null results) -> fail $ "builtin iele.invalid returns no results"
                | not (null args) -> fail $ "builtin iele.invalid takes no arguments"
                | otherwise -> pure $ VoidOp INVALID []
        Just (op,arity)
          | length results /= 1 -> fail $ "builtin "++show globalName++" returns exactly one result"
          | arity == length args -> pure $ Op (IeleOpcodesQuery op) (head results) args
          | otherwise -> fail $ "builtin "++show name++" expects "++show arity++" arguments"
    _ -> 
      pure $ CallOp (LOCALCALLDYN (argsLength args) (retsLength results)) results (name : args)


accountCallInst :: Parser IeleOpP
accountCallInst = do
  results <- nonEmptyLValues <* equal
  name <- skipKeyword "call" *> operandInclReserved
  tgt <- skipKeyword "at" *> operand
  args <- parens operands
  value <- skipKeyword "send" *> operand
  gas <- comma *> skipKeyword "gaslimit" *> operand
  case name of
    GlobalOperand globalName ->
      let op = CALL globalName (argsLength args) (fmap (subtract 1) (retsLength results)) in
      pure (CallOp op results ([gas,tgt,value]++args))
    _ ->
      let op = CALLDYN (argsLength args) (fmap (subtract 1) (retsLength results)) in
      pure (CallOp op results ([name,gas,tgt,value]++args))

staticCallInst :: Parser IeleOpP
staticCallInst = do
  results <- nonEmptyLValues <* equal
  name <- skipKeyword "staticcall" *> operandInclReserved
  tgt <- skipKeyword "at" *> operand
  args <- parens operands
  gas <- skipKeyword "gaslimit" *> operand
  case name of
    GlobalOperand globalName ->
      let op = STATICCALL globalName (argsLength args) (fmap (subtract 1) (retsLength results)) in
      pure (CallOp op results ([gas,tgt]++args))
    _ ->
      let op = STATICCALLDYN (argsLength args) (fmap (subtract 1) (retsLength results)) in
      pure (CallOp op results ([name,gas,tgt]++args))

callAddressInst :: Parser IeleOpP
callAddressInst = do
  result <- lValue <* equal
  name <- skipKeyword "calladdress" *> globalName
  tgt <- skipKeyword "at" *> operand
  pure (CallOp (CALLADDRESS name) [result] [tgt])

argumentsOrVoid :: Parser [Operand]
argumentsOrVoid = commaSep1 operand <|> [] <$ skipKeyword "void"

returnInst :: Parser IeleOpP
returnInst = skipKeyword "ret" *>
  fmap (\args -> VoidOp (RETURN (argsLength args)) args) argumentsOrVoid

revertInst :: Parser IeleOpP
revertInst = simpleOp0 "revert" 1 (REVERT)

selfDestructInst :: Parser IeleOpP
selfDestructInst = simpleOp0 "selfdestruct" 1 SELFDESTRUCT

upto :: Int -> Parser a -> Parser [a]
upto 0 _ = pure []
upto n p = ((:) <$> p <*> upto (n-1) p) <|> pure []

logInst :: Parser IeleOpP
logInst = do
  skipKeyword "log"
  arg1 <- operand
  args <- upto 4 (comma *> operand)
  pure $ VoidOp (LOG (fromIntegral (length args))) (arg1:args)

createInst :: Parser Instruction
createInst =
  build <$> lValue <* comma <*> lValue <* equal
        <* skipKeyword "create" <*> ieleNameToken <*> parens operands
        <* skipKeyword "send" <*> operand
 where
   build status addr name args val =
     CallOp (CREATE name (argsLength args)) [status,addr] (val:args)

copycreateInst :: Parser Instruction
copycreateInst =
  build <$> lValue <* comma <*> lValue <* equal
        <* skipKeyword "copycreate" <*> operand <*> parens operands
        <* skipKeyword "send" <*> operand
 where
   build status addr from args val =
     CallOp (COPYCREATE (argsLength args)) [status,addr] (val:from:args)

instruction :: Parser Instruction
instruction = try callInsts
     <|> try createInst
     <|> try copycreateInst
     <|> ieleOp1
     <|> ieleVoidOp
     <?> "instruction"

instructions :: Parser [Instruction]
instructions = many instruction

blockLabel :: Parser IeleName
blockLabel = try $ ieleNameToken <* void colon

labeledBlock :: Parser LabeledBlockP
labeledBlock =
  LabeledBlock <$> blockLabel <*> instructions

labeledBlocks :: Parser [LabeledBlockP]
labeledBlocks = many labeledBlock

functionParameters :: Parser [LValue]
functionParameters = parens (commaSep0 lValue)
  <?> "function parameters"

functionDefinition :: Parser FunctionDefinitionP
functionDefinition = do
  skipKeyword "define"
  public <- True <$ skipKeyword "public" <|> pure False
  name <- globalName
  params <- functionParameters
  openCurlyBrace
  entry <- instructions
  blocks <- labeledBlocks
  closeCurlyBrace
  return (FunctionDefinition public name params entry blocks)

topLevelDefinition :: Parser TopLevelDefinition
topLevelDefinition =
      TopLevelDefinitionContract <$ skipKeyword "external" <* skipKeyword "contract"
        <*> ieleNameTokenNormalOrString
  <|> TopLevelDefinitionFunction <$> functionDefinition
  <|> TopLevelDefinitionGlobal <$> globalName <* equal <*> int
 where
  int = fmap (\(IntToken i) -> i) intToken

contract :: Parser ContractP
contract = ContractP <$ skipKeyword "contract" <*> ieleNameTokenNormalOrString
                     <*> (Just <$ char '!' <*> positiveInt <|> pure Nothing)
                     <*> braces (many topLevelDefinition)
                     <?> "contract"

ieleParser1 :: Parser ContractP
ieleParser1 = whitespace *> contract <* eof

ieleParser :: Parser [ContractP]
ieleParser = whitespace *> many contract <* eof
