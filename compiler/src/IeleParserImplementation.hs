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

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

import Control.Applicative ((<|>), (<*), many)
import Control.Monad (void)
import Text.Parsec (try, (<?>), skipMany, satisfy)
import Text.Parsec.Char (char, digit, letter, oneOf, string)
import Text.Parsec.Combinator (eof, many1, choice, notFollowedBy, sepBy, sepBy1)
import Text.Parsec.String (Parser)

import Data.Word
import Data.Data

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

ieleNameTokenNotNumber :: Parser IeleName
ieleNameTokenNotNumber =
  lexeme $ IeleNameText <$> ((:) <$> ieleNameFirstChar <*> many ieleNameNonFirstChar)

ieleNameTokenNumber :: Parser IeleName
ieleNameTokenNumber =
    lexeme $ IeleNameNumber . read <$> many1 digit <* notFollowedBy ieleNameNonFirstChar

ieleNameToken :: Parser IeleName
ieleNameToken = ieleNameTokenNotNumber <|> ieleNameTokenNumber

positiveIntToken :: Parser IntToken
positiveIntToken = lexeme $ IntToken . read <$>
  many1 digit <* notFollowedBy ieleNameNonFirstChar

negativeIntToken :: Parser IntToken
negativeIntToken = lexeme $ IntToken . read <$>
  ((:) <$> minus <*> many1 digit) <* notFollowedBy ieleNameNonFirstChar

trueToken :: Parser IntToken
trueToken = IntToken 1 <$ skipKeyword "true"

falseToken :: Parser IntToken
falseToken = IntToken 0 <$ skipKeyword "false"

intToken :: Parser IntToken
intToken = positiveIntToken <|> negativeIntToken <|> trueToken <|> falseToken

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
localName = LocalName <$ percent <*> ieleNameToken


lValue :: Parser LValue
lValue =
      LValueGlobalName <$> globalName
  <|> LValueLocalName <$> localName

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
lValues :: Parser [LValue]
lValues = commaSep0 lValue

assignInst :: LValue -> Parser Instruction
assignInst result=
  loadImmediate result <|> Op MOVE result . (:[]) <$> lValue

loadImmediate :: LValue -> Parser Instruction
loadImmediate result = build <$> intToken
 where
  build (IntToken i)
    | i >= 0 = LiOp LOADPOS result i
    | otherwise = LiOp LOADNEG result (negate i)

withResult :: (LValue -> Parser a) -> Parser a
withResult p = try (lValue <* equal) >>= p

ieleOp :: Parser Instruction
ieleOp = try localCallInst
     <|> try accountCallInst
     <|> try staticCallInst
     <|> ieleOp1
     <|> ieleVoidOp

ieleOp1 :: Parser Instruction
ieleOp1 = do
  result <- try (lValue <* equal)
  choice . map ($ result) $
    [ assignInst
    , loadInst
    , sloadInst
    , isZeroInst
    , notInst
    , shaInst
    , binaryInst
    , ternaryInst
    , predicateInst
    , createInst
    , copycreateInst]

ieleVoidOp :: Parser Instruction
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

simpleOp :: String -> Int -> ([LValue] -> Instruction) -> Parser Instruction
simpleOp name arity build = build <$ skipKeyword name <*> simpleArgs arity

simpleArgs :: Int -> Parser [LValue]
simpleArgs 0 = pure []
simpleArgs arity = sequenceA (lValue:replicate (arity-1) (comma *> lValue))

simpleOp1 :: String -> Int -> IeleOpcode1P -> (LValue -> Parser Instruction)
simpleOp1 name arity opcode = simpleOp name arity . Op opcode

simpleOp0 :: String -> Int -> IeleOpcode0P -> Parser Instruction
simpleOp0 name arity opcode = simpleOp name arity (VoidOp opcode)

loadInst :: LValue -> Parser Instruction
loadInst result =
  try (simpleOp "load" 3 (Op MLOADN result))
   <|> simpleOp "load" 1 (Op MLOAD result)

storeInst :: Parser Instruction
storeInst =
  try (simpleOp0 "store" 4 MSTOREN)
  <|>  simpleOp0 "store" 2 MSTORE

sloadInst :: LValue -> Parser Instruction
sloadInst = simpleOp1 "sload" 1 SLOAD

sstoreInst :: Parser Instruction
sstoreInst = simpleOp0 "sstore" 2 SSTORE

jumpInst :: Parser Instruction
jumpInst = do
  skipKeyword "br"
  condition <- Just <$> lValue <* comma <|> pure Nothing
  lbl <- ieleNameToken
  pure $ case condition of
    Nothing -> VoidOp (JUMP lbl) []
    Just reg -> VoidOp (JUMPI lbl) [reg]

isZeroInst :: LValue -> Parser Instruction
isZeroInst = simpleOp1 "iszero" 1 ISZERO

notInst :: LValue -> Parser Instruction
notInst = simpleOp1 "not" 1 NOT

shaInst :: LValue -> Parser Instruction
shaInst = simpleOp1 "sha3" 1 SHA3

enumParser :: [(String,a)] -> Parser a
enumParser options =
  choice [val <$ skipKeyword name | (name,val) <- options]

binaryOperations :: [(String,IeleOpcode1P)]
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
  ,("sext",SIGNEXTEND)
  ,("twos",TWOS)
  ]

binaryInst :: LValue -> Parser Instruction
binaryInst result =
  choice [simpleOp1 name 2 opcode result | (name,opcode) <- binaryOperations]

ternaryOperators :: [(String, IeleOpcode1P)]
ternaryOperators = [("addmod",ADDMOD)
                   ,("mulmod",MULMOD)
                   ,("expmod",EXPMOD)
                   ]

ternaryInst :: LValue -> Parser Instruction
ternaryInst result =
  flip Op result <$> enumParser ternaryOperators <*> simpleArgs 3

predicateFlags :: [(String, IeleOpcode1P)]
predicateFlags = [("lt",LT)
                 ,("le",LE)
                 ,("gt",GT)
                 ,("ge",GE)
                 ,("eq",EQ)
                 ,("ne",NE)
                 ]

predicateInst :: LValue -> Parser Instruction
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

localCallInst :: Parser Instruction
localCallInst = do
  results <- callResult
  name <- skipKeyword "call" *> globalNameInclReserved
  args <- parens lValues
  let call = CallOp (LOCALCALL name (argsLength args) (retsLength results)) results args
  case Map.lookup name builtins of
    Nothing | name /= GlobalName (IeleNameText "iele.invalid") -> pure call
            | not (null results) -> fail $ "builtin iele.invalid returns no results"
            | not (null args) -> fail $ "builtin iele.invalid takes no arguments"
            | otherwise -> pure $ VoidOp INVALID []
    Just (op,arity)
      | length results /= 1 -> fail $ "builtin "++show name++" returns exactly one result"
      | arity == length args -> pure $ Op (IeleOpcodesQuery op) (head results) args
      | otherwise -> fail $ "builtin "++show name++" expects "++show arity++" arguments"


accountCallInst :: Parser Instruction
accountCallInst = do
  results <- nonEmptyLValues <* equal
  name <- skipKeyword "call" *> globalNameInclReserved
  tgt <- skipKeyword "at" *> lValue
  args <- parens lValues
  value <- skipKeyword "send" *> lValue
  gas <- comma *> skipKeyword "gaslimit" *> lValue
  let op = CALL name (argsLength args) (fmap (subtract 1) (retsLength results))
  pure (CallOp op results ([gas,tgt,value]++args))

staticCallInst :: Parser Instruction
staticCallInst = do
  results <- nonEmptyLValues <* equal
  name <- skipKeyword "staticcall" *> globalNameInclReserved
  tgt <- skipKeyword "at" *> lValue
  args <- parens lValues
  value <- skipKeyword "send" *> lValue
  gas <- comma *> skipKeyword "gaslimit" *> lValue
  let op = STATICCALL name (argsLength args) (fmap (subtract 1) (retsLength results))
  pure (CallOp op results ([gas,tgt,value]++args))


argumentsOrVoid :: Parser [LValue]
argumentsOrVoid = nonEmptyLValues <|> [] <$ skipKeyword "void"

returnInst :: Parser Instruction
returnInst = skipKeyword "ret" *>
  fmap (\args -> VoidOp (RETURN (retsLength args)) args) argumentsOrVoid

revertInst :: Parser Instruction
revertInst = skipKeyword "revert" *>
  fmap (\args -> VoidOp (REVERT (retsLength args)) args) argumentsOrVoid

selfDestructInst :: Parser Instruction
selfDestructInst = simpleOp0 "selfdestruct" 1 SELFDESTRUCT

upto :: Int -> Parser a -> Parser [a]
upto 0 _ = pure []
upto n p = ((:) <$> p <*> upto (n-1) p) <|> pure []

logInst :: Parser Instruction
logInst = do
  skipKeyword "log"
  arg1 <- lValue
  args <- upto 4 (comma *> lValue)
  pure $ VoidOp (LOG (fromIntegral (length args))) (arg1:args)

createInst :: LValue -> Parser Instruction
createInst result =
   build <$ skipKeyword "create" <*> ieleNameToken <*> parens lValues
         <* skipKeyword "send" <*> lValue
 where
   build name args val = Op (CREATE name (argsLength args)) result (val:args)

copycreateInst :: LValue -> Parser Instruction
copycreateInst result =
   build <$ skipKeyword "copycreate" <*> lValue <*> parens lValues
         <* skipKeyword "send" <*> lValue
 where
   build from args val = Op (COPYCREATE (argsLength args)) result (val:from:args)


instruction :: Parser Instruction
instruction = ieleOp <?> "instruction"
-- The try at the end, which I usually leave out, is needed here because we may
-- try to parse a label as an instruction and we must be able to recover from
-- that.

blockLabel :: Parser IeleName
blockLabel = try $ ieleNameToken <* void colon

instructions :: Parser [Instruction]
instructions = many instruction

labeledBlock :: Parser LabeledBlock
labeledBlock =
  LabeledBlock <$> blockLabel <*> instructions

labeledBlocks :: Parser [LabeledBlock]
labeledBlocks = many labeledBlock

functionParameters :: Parser [LocalName]
functionParameters = parens (commaSep0 localName)
  <?> "function parameters"

functionDefinition :: Parser FunctionDefinition
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
      TopLevelDefinitionContract <$ skipKeyword "contract" <*> globalName
  <|> TopLevelDefinitionFunction <$> functionDefinition

contract :: Parser Contract
contract = Contract <$ skipKeyword "contract" <*> ieleNameToken
                    <*> (Just <$ char '!' <*> positiveInt <|> pure Nothing)
                    <*> braces (many topLevelDefinition)
                    <?> "contract"

ieleParser1 :: Parser Contract
ieleParser1 = whitespace *> contract <* eof

ieleParser :: Parser [Contract]
ieleParser = whitespace *> many contract <* eof
