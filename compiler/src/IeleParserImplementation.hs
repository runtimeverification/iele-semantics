module IeleParserImplementation
    ( AccountCallInst
      ( AccountCallInst, accountCallLValues, accountCallName
      , accountCallAddress, accountCallOperands, accountCallSend
      , accountCallGasLimit)
    , accountCallInst
    , AssignInst (AssignInst, assignLeft, assignRight)
    , assignInst
    , at
    , BinaryOperationInst
      ( BinaryOperationInst, binaryOperationType, binaryOperationLeft
      , binaryOperationFirst, binaryOperationSecond)
    , binaryOperationInst
    , BinaryOperationType
      ( AddInst, MulInst, SubInst, DivInst, ExpInst, ModInst, AndInst
      , OrInst, XorInst, Sha3Inst, ByteInst, SExtInst, TwosInst)
    , closedCurlyBrace
    , closedParenthesis
    , colon
    , comma
    , CondJumpInst (CondJumpInst, condJumpOperand, condJumpLabel)
    , condJumpInst
    , contract
    , Contract
    , CreateInst
      ( CreateInst, createLValue, createContractName, createOperands
      , createSendValue)
    , createInst
    , equal
    , FunctionDefinition
      ( FunctionDefinition, functionDefinitionSignature
      , functionDefinitionBlocks)
    , functionDefinition
    , FunctionParameters (FunctionParameters)
    , functionParameters
    , FunctionSignature
      (FunctionSignature, functionSignatureName, functionSignatureParameters)
    , functionSignature
    , GlobalName (GlobalName)
    , globalName
    , GlobalVariableDefinition
      (GlobalVariableDefinition, globalVariableName, globalVariableValue)
    , globalVariableDefinition
    , ieleNameFirstChar
    , ieleNameNonFirstChar
    , IeleName(IeleName)
    , ieleNameToken
    , ieleParser
    , Instruction
      ( InstructionAssign, InstructionLoad, InstructionStore, InstructionSLoad
      , InstructionSStore, InstructionIsZero, InstructionNot
      , InstructionBinaryOperation, InstructionTernaryOperation
      , InstructionPredicateOperation, InstructionJump, InstructionCondJump
      , InstructionLocalCall, InstructionAccountCall, InstructionSend
      , InstructionReturn, InstructionRevert, InstructionStop, InstructionLog
      , InstructionCreate, InstructionSelfdestruct)
    , instruction
    , Instructions (Instructions)
    , instructions
    , IntToken (IntToken)
    , intToken
    , IsZeroInst (IsZeroInst, isZeroLeft, isZeroRight)
    , isZeroInst
    , JumpInst (JumpInst)
    , jumpInst
    , LabeledBlock (LabeledBlock, labeledBlockLabel, labeledBlockInstructions)
    , labeledBlock
    , LabeledBlocks (LabeledBlocks)
    , labeledBlocks
    , lexeme
    , LoadInst (LoadInst, loadLeft, loadIndex, loadSizeInBytes)
    , loadInst
    , LocalCallInst
      (LocalCallInst, localCallLValues, localCallName, localCallOperands)
    , localCallInst
    , LocalName (LocalName)
    , localName
    , LocalNames (LocalNames)
    , localNames
    , LogInst (LogInst, logIndex, logSizeInBytes, logContent)
    , logInst
    , LValue (LValueGlobalName, LValueLocalName)
    , lValue
    , minus
    , LValues (LValues)
    , nonEmptyLValues
    , nonEmptyOperands
    , NotInst (NotInst, notLeft, notRight)
    , notInst
    , openCurlyBrace
    , openParenthesis
    , Operand (OperandInt, OperandLValue)
    , operand
    , Operands (Operands)
    , operands
    , percent
    , PredicateOperationInst
      ( PredicateOperationInst, predicateOperationType, predicateOperationLeft
      , predicateOperationFirst, predicateOperationSecond)
    , predicateOperationInst
    , PredicateOperationType
      ( LtPredicateInst, LePredicateInst, GtPredicateInst, GePredicateInst
      , EqPredicateInst, NePredicateInst)
    , ReturnInst (ReturnInst)
    , returnInst
    , RevertInst (RevertInst)
    , revertInst
    , SelfdestructInst (SelfdestructInst, selfdestructAccountToSendBalance)
    , selfDestructInst
    , SendInst (SendInst, sendValue, sendDestinationAccount)
    , sendInst
    , skipKeyword
    , SLoadInst (SLoadInst, sloadLeft, sloadIndex, sloadSizeInBytes)
    , sloadInst
    , SStoreInst (SStoreInst, sstoreValue, sstoreIndex, sstoreSizeInBytes)
    , sstoreInst
    , StopInst (StopInst)
    , stopInst
    , StoreInst (StoreInst, storeValue, storeIndex, storeSizeInBytes)
    , storeInst
    , TernaryOperationInst
      ( TernaryOperationInst, ternaryOperationType, ternaryOperationLeft
      , ternaryOperationFirst, ternaryOperationSecond, ternaryOperationDivisor)
    , ternaryOperationInst
    , TernaryOperationType (AddModInst, MulModInst, ExpModInst)
    , TopLevelDefinition
      (TopLevelDefinitionGlobalVariable, TopLevelDefinitionFunction)
    , topLevelDefinition
    , TopLevelDefinitions (TopLevelDefinitions)
    , topLevelDefinitions
    , whitespace
    ) where

import Control.Applicative ((<|>), (<*), many)
import Control.Monad (void)
import Text.Parsec (try, (<?>))
import Text.Parsec.Char (char, digit, letter, oneOf, string)
import Text.Parsec.Combinator (eof, many1, choice, notFollowedBy, sepBy, sepBy1)
import Text.Parsec.String (Parser)

whitespace :: Parser ()
whitespace = void (many (oneOf " \n\t"))

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

closedParenthesis :: Parser Char
closedParenthesis = lexeme (char ')')

parens :: Parser a -> Parser a
parens p = openParenthesis *> p <* closedParenthesis

openCurlyBrace :: Parser Char
openCurlyBrace = lexeme (char '{')

closedCurlyBrace :: Parser Char
closedCurlyBrace = lexeme (char '}')

braces :: Parser a -> Parser a
braces p = openCurlyBrace *> p <* closedCurlyBrace

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
skipKeyword s = try $ lexeme $ string s *> notFollowedBy ieleNameNonFirstChar

whitespaceColon :: Parser ()
whitespaceColon = whitespace <* colon

---------------------------------------------

newtype IeleName = IeleName String
  deriving (Show, Eq)

ieleNameTokenNotNumber :: Parser IeleName
ieleNameTokenNotNumber =
  lexeme $ IeleName <$> ((:) <$> ieleNameFirstChar <*> many ieleNameNonFirstChar)

ieleNameTokenNumber :: Parser IeleName
ieleNameTokenNumber =
    lexeme $ IeleName <$> many1 digit <* notFollowedBy ieleNameNonFirstChar

ieleNameToken :: Parser IeleName
ieleNameToken = ieleNameTokenNotNumber <|> ieleNameTokenNumber

newtype IntToken = IntToken Integer deriving (Show, Eq)

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

newtype GlobalName = GlobalName IeleName  -- TODO: A string may be more efficient than the full AST
  deriving (Show, Eq)
globalName :: Parser GlobalName
globalName = GlobalName <$ at <*> ieleNameToken

newtype LocalName = LocalName IeleName  -- TODO: A string may be more efficient than the full AST
  deriving (Show, Eq)
localName :: Parser LocalName
localName = LocalName <$ percent <*> ieleNameToken

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype LocalNames = LocalNames [LocalName] deriving (Show, Eq)
localNames :: Parser LocalNames
localNames = LocalNames <$> commaSep0 localName

data LValue =
    LValueGlobalName GlobalName
  | LValueLocalName LocalName
  deriving (Show, Eq)

lValue :: Parser LValue
lValue =
      LValueGlobalName <$> globalName
  <|> LValueLocalName <$> localName

data Operand =
    OperandLValue LValue
  | OperandInt IntToken
  deriving (Show, Eq)

operand :: Parser Operand
operand =
      OperandLValue <$> lValue
  <|> OperandInt <$> intToken

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype Operands = Operands [Operand] deriving (Show, Eq)
operands :: Parser Operands
operands = Operands <$> commaSep0 operand

data AssignInst = AssignInst
  { assignLeft :: LValue
  , assignRight :: Operand
  } deriving (Show, Eq)
assignInst :: Parser AssignInst
assignInst =
  AssignInst <$> lValue <* equal <*> operand

data LoadInst = LoadInst
  { loadLeft :: LValue
  , loadIndex :: Operand
  , loadSizeInBytes :: Operand
  } deriving (Show, Eq)
loadInst :: Parser LoadInst
loadInst =
  LoadInst <$> lValue <* equal <* skipKeyword "load" <*> operand <* comma <*> operand

data StoreInst = StoreInst
  { storeValue :: Operand
  , storeIndex :: Operand
  , storeSizeInBytes :: Operand
  } deriving (Show, Eq)
storeInst :: Parser StoreInst
storeInst =
  StoreInst <$ skipKeyword "store" <*> operand <* comma <*> operand <* comma <*> operand

data SLoadInst = SLoadInst
  { sloadLeft :: LValue
  , sloadIndex :: Operand
  , sloadSizeInBytes :: Operand
  } deriving (Show, Eq)
sloadInst :: Parser SLoadInst
sloadInst =
  SLoadInst <$> lValue <* equal <* skipKeyword "sload" <*> operand <* comma <*> operand

data SStoreInst = SStoreInst
  { sstoreValue :: Operand
  , sstoreIndex :: Operand
  , sstoreSizeInBytes :: Operand
  } deriving (Show, Eq)
sstoreInst :: Parser SStoreInst
sstoreInst =
  SStoreInst <$ skipKeyword "sstore" <*> operand <* comma <*> operand <* comma <*> operand

data IsZeroInst = IsZeroInst
  { isZeroLeft :: LValue
  , isZeroRight :: Operand
  } deriving (Show, Eq)
isZeroInst :: Parser IsZeroInst
isZeroInst = IsZeroInst <$> lValue <* equal <* skipKeyword "iszero" <*> operand

data NotInst = NotInst
  { notLeft :: LValue
  , notRight :: Operand
  } deriving (Show, Eq)
notInst :: Parser NotInst
notInst = NotInst <$> lValue <* equal <* skipKeyword "not" <*> operand

data BinaryOperationType =
    AddInst
  | MulInst
  | SubInst
  | DivInst
  | ExpInst
  | ModInst
  | AndInst
  | OrInst
  | XorInst
  | Sha3Inst
  | ByteInst
  | SExtInst
  | TwosInst
  deriving (Show, Eq)

enumParser :: [(String,a)] -> Parser a
enumParser options =
  choice [val <$ skipKeyword name | (name,val) <- options]

binaryOperations :: [(String,BinaryOperationType)]
binaryOperations =
  [("add",AddInst)
  ,("mul",MulInst)
  ,("sub",SubInst)
  ,("div",DivInst)
  ,("exp",ExpInst)
  ,("mod",ModInst)
  ,("and",AndInst)
  ,("or", OrInst)
  ,("xor",XorInst)
  ,("sha3",Sha3Inst)
  ,("byte",ByteInst)
  ,("sext",SExtInst)
  ,("twos",TwosInst)
  ]

data BinaryOperationInst = BinaryOperationInst
  { binaryOperationType :: BinaryOperationType
  , binaryOperationLeft :: LValue
  , binaryOperationFirst :: Operand
  , binaryOperationSecond :: Operand
  } deriving (Show, Eq)

binaryOperationInst :: Parser BinaryOperationInst
binaryOperationInst =
  flip BinaryOperationInst <$> lValue <* equal <*> enumParser binaryOperations
    <*> operand <* comma <*> operand

data TernaryOperationType =
    AddModInst
  | MulModInst
  | ExpModInst
  deriving (Show, Eq)

ternaryOperators :: [(String, TernaryOperationType)]
ternaryOperators = [("addmod",AddModInst)
                   ,("mulmod",MulModInst)
                   ,("expmod",ExpModInst)
                   ]

data TernaryOperationInst = TernaryOperationInst
  { ternaryOperationType :: TernaryOperationType
  , ternaryOperationLeft :: LValue
  , ternaryOperationFirst :: Operand
  , ternaryOperationSecond :: Operand
  , ternaryOperationDivisor :: Operand
  } deriving (Show, Eq)

ternaryOperationInst :: Parser TernaryOperationInst
ternaryOperationInst =
  flip TernaryOperationInst <$> lValue <* equal <*> enumParser ternaryOperators
    <*> operand <* comma <*> operand <* comma <*> operand

data PredicateOperationType =
    LtPredicateInst
  | LePredicateInst
  | GtPredicateInst
  | GePredicateInst
  | EqPredicateInst
  | NePredicateInst
  deriving (Show, Eq)

predicateOperationTypes :: Parser PredicateOperationType
predicateOperationTypes = enumParser
  [("lt",LtPredicateInst)
  ,("le",LePredicateInst)
  ,("gt",GtPredicateInst)
  ,("ge",GePredicateInst)
  ,("eq",EqPredicateInst)
  ,("ne",NePredicateInst)
  ]

data PredicateOperationInst = PredicateOperationInst
    { predicateOperationType :: PredicateOperationType
    , predicateOperationLeft :: LValue
    , predicateOperationFirst :: Operand
    , predicateOperationSecond :: Operand
    } deriving (Show, Eq)

predicateOperationInst :: Parser PredicateOperationInst
predicateOperationInst =
  flip PredicateOperationInst <$> lValue <* equal <* skipKeyword "cmp" <*> predicateOperationTypes
    <*> operand <* comma <*> operand

newtype JumpInst = JumpInst IeleName deriving (Show, Eq)
jumpInst :: Parser JumpInst
jumpInst = JumpInst <$ skipKeyword "br" <*> ieleNameToken

data CondJumpInst = CondJumpInst
  { condJumpOperand :: Operand
  , condJumpLabel :: IeleName
  } deriving (Show, Eq)
condJumpInst :: Parser CondJumpInst
condJumpInst =
  CondJumpInst <$ skipKeyword "br" <*> operand <* comma <*> ieleNameToken

newtype LValues = LValues [LValue] deriving (Show, Eq)
nonEmptyLValues :: Parser LValues
nonEmptyLValues = LValues <$> commaSep1 lValue

callResult :: Parser LValues
callResult =  nonEmptyLValues <* equal <|> return (LValues [])

data LocalCallInst = LocalCallInst
  { localCallLValues :: LValues
  , localCallName :: GlobalName
  , localCallOperands :: Operands
  } deriving (Show, Eq)
localCallInst :: Parser LocalCallInst
localCallInst =
  LocalCallInst <$> callResult <* skipKeyword "call" <*> globalName <*> parens operands

data AccountCallInst = AccountCallInst
  { accountCallLValues :: LValues
  , accountCallName :: GlobalName
  , accountCallAddress :: Operand
  , accountCallOperands :: Operands
  , accountCallSend :: Operand
  , accountCallGasLimit :: Operand
  } deriving (Show, Eq)
accountCallInst :: Parser AccountCallInst
accountCallInst =
  AccountCallInst <$> callResult <* skipKeyword "call"
    <*> globalName <* skipKeyword "at" <*> operand <*> parens operands
    <* skipKeyword "send" <*> operand
    <* comma <* skipKeyword "gaslimit" <*> operand

data SendInst = SendInst
  { sendValue :: Operand
  , sendDestinationAccount :: Operand
  } deriving (Show, Eq)
sendInst :: Parser SendInst
sendInst = SendInst <$ skipKeyword "send" <*> operand <* skipKeyword "to" <*> operand

nonEmptyOperands :: Parser Operands
nonEmptyOperands = Operands <$> commaSep1 operand

argumentsOrVoid :: Parser Operands
argumentsOrVoid = parens nonEmptyOperands <|> Operands [] <$ skipKeyword "void"

newtype ReturnInst = ReturnInst Operands deriving (Show, Eq)
returnInst :: Parser ReturnInst
returnInst = ReturnInst <$ skipKeyword "ret" <*> argumentsOrVoid

newtype RevertInst = RevertInst Operands deriving (Show, Eq)
revertInst :: Parser RevertInst
revertInst = RevertInst <$ skipKeyword "revert" <*> argumentsOrVoid

data StopInst = StopInst deriving (Show, Eq)
stopInst :: Parser StopInst
stopInst = StopInst <$ skipKeyword "stop"

data LogInst = LogInst
  { logIndex :: Operand
  , logSizeInBytes :: Operand
  , logContent :: Operands
  } deriving (Show, Eq)
logInst :: Parser LogInst
logInst = LogInst <$ skipKeyword "log" <*> operand <* comma <*> operand <*> logArgs
  where logArgs = Operands <$> many (comma *> operand)

data CreateInst = CreateInst
  { createLValue :: LValue
  , createContractName :: IeleName
  , createOperands :: Operands
  , createSendValue :: Operand
  } deriving (Show, Eq)
createInst :: Parser CreateInst
createInst = CreateInst <$> lValue <* equal <* skipKeyword "create"
  <*> ieleNameToken <*> parens operands <* skipKeyword "send" <*> operand

newtype SelfdestructInst = SelfdestructInst
  { selfdestructAccountToSendBalance :: Operand} deriving (Show, Eq)
selfDestructInst :: Parser SelfdestructInst
selfDestructInst = SelfdestructInst <$ skipKeyword "selfdestruct" <*> operand

data Instruction =
    InstructionAssign AssignInst
  | InstructionLoad LoadInst
  | InstructionStore StoreInst
  | InstructionSLoad SLoadInst
  | InstructionSStore SStoreInst
  | InstructionIsZero IsZeroInst
  | InstructionNot NotInst
  | InstructionBinaryOperation BinaryOperationInst
  | InstructionTernaryOperation TernaryOperationInst
  | InstructionPredicateOperation PredicateOperationInst
  | InstructionJump JumpInst
  | InstructionCondJump CondJumpInst
  | InstructionLocalCall LocalCallInst
  | InstructionAccountCall AccountCallInst
  | InstructionSend SendInst
  | InstructionReturn ReturnInst
  | InstructionRevert RevertInst
  | InstructionStop StopInst
  | InstructionLog LogInst
  | InstructionCreate CreateInst
  | InstructionSelfdestruct SelfdestructInst
  deriving (Show, Eq)

instruction :: Parser Instruction
instruction =
      try (InstructionAssign <$> assignInst)
  <|> try (InstructionLoad <$> loadInst)
  <|> try (InstructionStore <$> storeInst)
  <|> try (InstructionSLoad <$> sloadInst)
  <|> try (InstructionSStore <$> sstoreInst)
  <|> try (InstructionIsZero <$> isZeroInst)
  <|> try (InstructionNot <$> notInst)
  <|> try (InstructionBinaryOperation <$> binaryOperationInst)
  <|> try (InstructionTernaryOperation <$> ternaryOperationInst)
  <|> try (InstructionPredicateOperation <$> predicateOperationInst)
  -- condJumpInst must be before jumpInst otherwise it's harder to parse things,
  -- i.e. 'br 10, a' gets parsed as 'br 10' with a leftover of ', a'.
  <|> try (InstructionCondJump <$> condJumpInst)
  <|> try (InstructionJump <$> jumpInst)
  <|> try (InstructionLocalCall <$> localCallInst)
  <|> try (InstructionAccountCall <$> accountCallInst)
  <|> try (InstructionSend <$> sendInst)
  <|> try (InstructionReturn <$> returnInst)
  <|> try (InstructionRevert <$> revertInst)
  <|> try (InstructionStop <$> stopInst)
  <|> try (InstructionLog <$> logInst)
  <|> try (InstructionCreate <$> createInst)
  <|> try (InstructionSelfdestruct <$> selfDestructInst)
  <?> "instruction"
-- The try at the end, which I usually leave out, is needed here because we may
-- try to parse a label as an instruction and we must be able to recover from
-- that.

blockLabel :: Parser IeleName
blockLabel = try $ ieleNameToken <* void colon

newtype Instructions = Instructions [Instruction] deriving (Show, Eq)
instructions :: Parser Instructions
instructions = Instructions <$> many (try (instruction <* notFollowedBy colon))

data LabeledBlock = LabeledBlock
  { labeledBlockLabel :: IeleName
  , labeledBlockInstructions :: Instructions
  } deriving (Show, Eq)
labeledBlock :: Parser LabeledBlock
labeledBlock =
  LabeledBlock <$> blockLabel <*> instructions

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype LabeledBlocks = LabeledBlocks [LabeledBlock] deriving (Show, Eq)
labeledBlocks :: Parser LabeledBlocks
labeledBlocks =
  LabeledBlocks <$> many labeledBlock

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype FunctionParameters = FunctionParameters LocalNames deriving (Show, Eq)
functionParameters :: Parser FunctionParameters
functionParameters =
  FunctionParameters <$> localNames

data FunctionSignature = FunctionSignature
  { functionSignatureName :: GlobalName
  , functionSignatureParameters :: FunctionParameters
  } deriving (Show, Eq)
functionSignature :: Parser FunctionSignature
functionSignature =
  FunctionSignature <$> globalName <*> parens functionParameters

data FunctionDefinition = FunctionDefinition
  { functionDefinitionSignature :: FunctionSignature
  , functionDefinitionBlocks :: LabeledBlocks
  } deriving (Show, Eq)
functionDefinition :: Parser FunctionDefinition
functionDefinition =
  FunctionDefinition <$ skipKeyword "define" <*> functionSignature <*> braces labeledBlocks

data GlobalVariableDefinition = GlobalVariableDefinition
  { globalVariableName :: GlobalName
  , globalVariableValue :: IntToken
  } deriving (Show, Eq)
globalVariableDefinition :: Parser GlobalVariableDefinition
globalVariableDefinition = GlobalVariableDefinition <$> globalName <* equal <*> intToken

data TopLevelDefinition =
    TopLevelDefinitionGlobalVariable GlobalVariableDefinition
  | TopLevelDefinitionFunction FunctionDefinition
  deriving (Show, Eq)
topLevelDefinition :: Parser TopLevelDefinition
topLevelDefinition =
      TopLevelDefinitionGlobalVariable <$> globalVariableDefinition
  <|> TopLevelDefinitionFunction <$> functionDefinition

newtype TopLevelDefinitions =
  TopLevelDefinitions [TopLevelDefinition] deriving (Show, Eq)
topLevelDefinitions :: Parser TopLevelDefinitions
topLevelDefinitions =
    TopLevelDefinitions <$> many topLevelDefinition

newtype Contract = Contract TopLevelDefinitions deriving (Show, Eq)
contract :: Parser Contract
contract = Contract <$> topLevelDefinitions

ieleParser :: Parser Contract
ieleParser = whitespace *> contract <* eof
