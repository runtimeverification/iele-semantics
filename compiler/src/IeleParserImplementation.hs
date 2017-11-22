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
import Text.Parsec (try)
import Text.Parsec.Char (char, digit, letter, oneOf, string)
import Text.Parsec.Combinator (eof, many1, notFollowedBy, sepBy, sepBy1)
import Text.Parsec.String (Parser)

whitespace :: Parser ()
whitespace = void (many (oneOf " \n\t"))

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

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

colons :: Parser String
colons = many colon

openParenthesis :: Parser Char
openParenthesis = lexeme (char '(')

closedParenthesis :: Parser Char
closedParenthesis = lexeme (char ')')

openCurlyBrace :: Parser Char
openCurlyBrace = lexeme (char '{')

closedCurlyBrace :: Parser Char
closedCurlyBrace = lexeme (char '}')

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
skipKeyword s = do
    void (string s)
    notFollowedBy ieleNameNonFirstChar
    whitespace

wrapperParser :: Parser b -> (b -> a) -> Parser a
wrapperParser parse constructor = do
  c <- parse
  return (constructor c)

whitespaceColon :: Parser ()
whitespaceColon = do
    whitespace
    void colon

-- Needed to avoid backtracking when parsing "stop:", which can be parsed as
-- the instruction "stop" followed by ":" or as the label "stop"
instructionWrapper :: Parser b -> (b -> a) -> Parser a
instructionWrapper parse constructor = do
    c <- parse
    notFollowedBy whitespaceColon
    return (constructor c)

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
commaSeparatedList0 :: Parser a -> ([a] -> b) -> Parser b
commaSeparatedList0 elementParser listConstructor =
    wrapperParser (commaSep0 elementParser) listConstructor

commaSeparatedList1 :: Parser a -> ([a] -> b) -> Parser b
commaSeparatedList1 elementParser listConstructor =
    wrapperParser (commaSep1 elementParser) listConstructor

---------------------------------------------

newtype IeleName = IeleName String
  deriving (Show, Eq)

ieleNameTokenNotNumber :: Parser IeleName
ieleNameTokenNotNumber = do
    fc <- ieleNameFirstChar
    rest <- many ieleNameNonFirstChar
    whitespace
    return (IeleName (fc:rest))

ieleNameTokenNumber :: Parser IeleName
ieleNameTokenNumber = do
    t <- many1 digit
    notFollowedBy ieleNameNonFirstChar
    whitespace
    return (IeleName t)

ieleNameToken :: Parser IeleName
ieleNameToken = ieleNameTokenNotNumber <|> ieleNameTokenNumber

newtype IntToken = IntToken Integer deriving (Show, Eq)

positiveIntToken :: Parser IntToken
positiveIntToken = do
    t <- many1 digit
    notFollowedBy ieleNameNonFirstChar
    whitespace
    return (IntToken (read t))

negativeIntToken :: Parser IntToken
negativeIntToken = do
    h <- minus
    t <- many1 digit
    notFollowedBy ieleNameNonFirstChar
    whitespace
    return (IntToken (read (h:t)))

trueToken :: Parser IntToken
trueToken = do
    skipKeyword "true"
    return (IntToken 1)

falseToken :: Parser IntToken
falseToken = do
    skipKeyword "false"
    return (IntToken 0)

intToken :: Parser IntToken
intToken = positiveIntToken <|> negativeIntToken <|> trueToken <|> falseToken

newtype GlobalName = GlobalName IeleName  -- TODO: A string may be more efficient than the full AST
  deriving (Show, Eq)
globalName :: Parser GlobalName
globalName = do
    void at
    t <- ieleNameToken
    return (GlobalName t)

newtype LocalName = LocalName IeleName  -- TODO: A string may be more efficient than the full AST
  deriving (Show, Eq)
localName :: Parser LocalName
localName = do
    void percent
    t <- ieleNameToken
    return (LocalName t)

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype LocalNames = LocalNames [LocalName] deriving (Show, Eq)
localNames :: Parser LocalNames
localNames = commaSeparatedList0 localName LocalNames

data LValue =
    LValueGlobalName GlobalName
  | LValueLocalName LocalName
  deriving (Show, Eq)

lValue :: Parser LValue
lValue =
      wrapperParser globalName LValueGlobalName
  <|> wrapperParser localName LValueLocalName

newtype LValues = LValues [LValue] deriving (Show, Eq)
nonEmptyLValues :: Parser LValues
nonEmptyLValues = commaSeparatedList1 lValue LValues

data Operand =
    OperandLValue LValue
  | OperandInt IntToken
  deriving (Show, Eq)

operand :: Parser Operand
operand =
      wrapperParser lValue OperandLValue
  <|> wrapperParser intToken OperandInt

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype Operands = Operands [Operand] deriving (Show, Eq)
operands :: Parser Operands
operands = commaSeparatedList0 operand Operands

nonEmptyOperands :: Parser Operands
nonEmptyOperands = commaSeparatedList1 operand Operands

data AssignInst = AssignInst
  { assignLeft :: LValue
  , assignRight :: Operand
  } deriving (Show, Eq)
assignInst :: Parser AssignInst
assignInst = do
    left <- lValue
    void equal
    right <- operand
    return AssignInst {assignLeft = left, assignRight = right}

data LoadInst = LoadInst
  { loadLeft :: LValue
  , loadIndex :: Operand
  , loadSizeInBytes :: Operand
  } deriving (Show, Eq)
loadInst :: Parser LoadInst
loadInst = do
    left <- lValue
    void equal
    skipKeyword "load"
    index <- operand
    void comma
    sizeInBytes <- operand
    return LoadInst {loadLeft = left, loadIndex = index, loadSizeInBytes = sizeInBytes}

data StoreInst = StoreInst
  { storeValue :: Operand
  , storeIndex :: Operand
  , storeSizeInBytes :: Operand
  } deriving (Show, Eq)
storeInst :: Parser StoreInst
storeInst = do
    skipKeyword "store"
    value <- operand
    void comma
    index <- operand
    void comma
    sizeInBytes <- operand
    return StoreInst {storeValue = value, storeIndex = index, storeSizeInBytes = sizeInBytes}

data SLoadInst = SLoadInst
  { sloadLeft :: LValue
  , sloadIndex :: Operand
  , sloadSizeInBytes :: Operand
  } deriving (Show, Eq)
sloadInst :: Parser SLoadInst
sloadInst = do
    left <- lValue
    void equal
    skipKeyword "sload"
    index <- operand
    void comma
    sizeInBytes <- operand
    return SLoadInst {sloadLeft = left, sloadIndex = index, sloadSizeInBytes = sizeInBytes}

data SStoreInst = SStoreInst
  { sstoreValue :: Operand
  , sstoreIndex :: Operand
  , sstoreSizeInBytes :: Operand
  } deriving (Show, Eq)
sstoreInst :: Parser SStoreInst
sstoreInst = do
    skipKeyword "sstore"
    value <- operand
    void comma
    index <- operand
    void comma
    sizeInBytes <- operand
    return SStoreInst {sstoreValue = value, sstoreIndex = index, sstoreSizeInBytes = sizeInBytes}

data IsZeroInst = IsZeroInst
  { isZeroLeft :: LValue
  , isZeroRight :: Operand
  } deriving (Show, Eq)
isZeroInst :: Parser IsZeroInst
isZeroInst = do
    left <- lValue
    void equal
    skipKeyword "iszero"
    right <- operand
    return IsZeroInst {isZeroLeft = left, isZeroRight = right}

data NotInst = NotInst
  { notLeft :: LValue
  , notRight :: Operand
  } deriving (Show, Eq)
notInst :: Parser NotInst
notInst = do
    left <- lValue
    void equal
    skipKeyword "not"
    right <- operand
    return NotInst {notLeft = left, notRight = right}


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

enumParser :: String -> a -> Parser a
enumParser name operationType = do
    skipKeyword name
    return operationType

binaryOperationTypes :: Parser BinaryOperationType
binaryOperationTypes =
      try (enumParser "add" AddInst)
  <|> try (enumParser "mul" MulInst)
  <|> try (enumParser "sub" SubInst)
  <|> try (enumParser "div" DivInst)
  <|> try (enumParser "exp" ExpInst)
  <|> try (enumParser "mod" ModInst)
  <|> try (enumParser "and" AndInst)
  <|> try (enumParser "or" OrInst)
  <|> try (enumParser "xor" XorInst)
  <|> try (enumParser "sha3" Sha3Inst)
  <|> try (enumParser "byte" ByteInst)
  <|> try (enumParser "sext" SExtInst)
  <|> enumParser "twos" TwosInst


data BinaryOperationInst = BinaryOperationInst
  { binaryOperationType :: BinaryOperationType
  , binaryOperationLeft :: LValue
  , binaryOperationFirst :: Operand
  , binaryOperationSecond :: Operand
  } deriving (Show, Eq)

binaryOperationInst :: Parser BinaryOperationInst
binaryOperationInst = do
    left <- lValue
    void equal
    operationType <- binaryOperationTypes
    first <- operand
    void comma
    second <- operand
    return (BinaryOperationInst operationType left first second)

data TernaryOperationType =
    AddModInst
  | MulModInst
  | ExpModInst
  deriving (Show, Eq)

ternaryOperationTypes :: Parser TernaryOperationType
ternaryOperationTypes =
      try (enumParser "addmod" AddModInst)
  <|> try (enumParser "mulmod" MulModInst)
  <|> enumParser "expmod" ExpModInst

data TernaryOperationInst = TernaryOperationInst
  { ternaryOperationType :: TernaryOperationType
  , ternaryOperationLeft :: LValue
  , ternaryOperationFirst :: Operand
  , ternaryOperationSecond :: Operand
  , ternaryOperationDivisor :: Operand
  } deriving (Show, Eq)

ternaryOperationInst :: Parser TernaryOperationInst
ternaryOperationInst = do
    left <- lValue
    void equal
    operationType <- ternaryOperationTypes
    first <- operand
    void comma
    second <- operand
    void comma
    third <- operand
    return (TernaryOperationInst operationType left first second third)

data PredicateOperationType =
    LtPredicateInst
  | LePredicateInst
  | GtPredicateInst
  | GePredicateInst
  | EqPredicateInst
  | NePredicateInst
  deriving (Show, Eq)

predicateOperationTypes :: Parser PredicateOperationType
predicateOperationTypes =
      try (enumParser "lt" LtPredicateInst)
  <|> try (enumParser "le" LePredicateInst)
  <|> try (enumParser "gt" GtPredicateInst)
  <|> try (enumParser "ge" GePredicateInst)
  <|> try (enumParser "eq" EqPredicateInst)
  <|> enumParser "ne" NePredicateInst

data PredicateOperationInst = PredicateOperationInst
    { predicateOperationType :: PredicateOperationType
    , predicateOperationLeft :: LValue
    , predicateOperationFirst :: Operand
    , predicateOperationSecond :: Operand
    } deriving (Show, Eq)

predicateOperationInst :: Parser PredicateOperationInst
predicateOperationInst = do
    left <- lValue
    void equal
    skipKeyword "cmp"
    operationType <- predicateOperationTypes
    first <- operand
    void comma
    second <- operand
    return (PredicateOperationInst operationType left first second)

newtype JumpInst = JumpInst IeleName deriving (Show, Eq)
jumpInst :: Parser JumpInst
jumpInst = do
    skipKeyword "br"
    l <- ieleNameToken
    return (JumpInst l)

data CondJumpInst = CondJumpInst
  { condJumpOperand :: Operand
  , condJumpLabel :: IeleName
  } deriving (Show, Eq)
condJumpInst :: Parser CondJumpInst
condJumpInst = do
    skipKeyword "br"
    o <- operand
    void comma
    l <- ieleNameToken
    return CondJumpInst {condJumpOperand = o, condJumpLabel = l}

data LocalCallInst = LocalCallInst
  { localCallLValues :: LValues
  , localCallName :: GlobalName
  , localCallOperands :: Operands
  } deriving (Show, Eq)
localCallInstLValues :: Parser LocalCallInst
localCallInstLValues = do
    lv <- nonEmptyLValues
    void equal
    skipKeyword "call"
    n <- globalName
    void openParenthesis
    o <- operands
    void closedParenthesis
    return
      LocalCallInst
        {localCallLValues = lv, localCallName = n, localCallOperands = o}
localCallInstNoLValues :: Parser LocalCallInst
localCallInstNoLValues = do
    skipKeyword "call"
    n <- globalName
    void openParenthesis
    o <- operands
    void closedParenthesis
    return
      LocalCallInst
        { localCallLValues = LValues []
        , localCallName = n
        , localCallOperands = o
        }
localCallInst :: Parser LocalCallInst
localCallInst = localCallInstLValues <|> localCallInstNoLValues

data AccountCallInst = AccountCallInst
  { accountCallLValues :: LValues
  , accountCallName :: GlobalName
  , accountCallAddress :: Operand
  , accountCallOperands :: Operands
  , accountCallSend :: Operand
  , accountCallGasLimit :: Operand
  } deriving (Show, Eq)
accountCallInstLValues :: Parser AccountCallInst
accountCallInstLValues = do
    lv <- nonEmptyLValues
    void equal
    skipKeyword "call"
    n <- globalName
    skipKeyword "at"
    a <- operand
    void openParenthesis
    o <- operands
    void closedParenthesis
    skipKeyword "send"
    s <- operand
    void comma
    skipKeyword "gaslimit"
    g <- operand
    return AccountCallInst
      { accountCallLValues = lv
      , accountCallName = n
      , accountCallAddress = a
      , accountCallOperands = o
      , accountCallSend = s
      , accountCallGasLimit = g}
accountCallInstNoLValues :: Parser AccountCallInst
accountCallInstNoLValues = do
  skipKeyword "call"
  n <- globalName
  skipKeyword "at"
  a <- operand
  void openParenthesis
  o <- operands
  void closedParenthesis
  skipKeyword "send"
  s <- operand
  void comma
  skipKeyword "gaslimit"
  g <- operand
  return AccountCallInst
    { accountCallLValues = LValues []
    , accountCallName = n
    , accountCallAddress = a
    , accountCallOperands = o
    , accountCallSend = s
    , accountCallGasLimit = g}
accountCallInst :: Parser AccountCallInst
accountCallInst = accountCallInstLValues <|> accountCallInstNoLValues


data SendInst = SendInst
  { sendValue :: Operand
  , sendDestinationAccount :: Operand
  } deriving (Show, Eq)
sendInst :: Parser SendInst
sendInst = do
    skipKeyword "send"
    v <- operand
    skipKeyword "to"
    a <- operand
    return SendInst {sendValue = v, sendDestinationAccount = a}

newtype ReturnInst = ReturnInst Operands deriving (Show, Eq)
returnInstWithOperands :: Parser ReturnInst
returnInstWithOperands = do
    skipKeyword "ret"
    void openParenthesis
    t <- nonEmptyOperands
    void closedParenthesis
    return (ReturnInst t)
returnInstVoid :: Parser ReturnInst
returnInstVoid = do
    skipKeyword "ret"
    skipKeyword "void"
    return (ReturnInst (Operands []))
returnInst :: Parser ReturnInst
returnInst = try returnInstVoid <|> returnInstWithOperands

newtype RevertInst = RevertInst Operands deriving (Show, Eq)
revertInstWithOperands :: Parser RevertInst
revertInstWithOperands = do
    skipKeyword "revert"
    void openParenthesis
    t <- nonEmptyOperands
    void closedParenthesis
    return (RevertInst t)
revertInstVoid :: Parser RevertInst
revertInstVoid = do
    skipKeyword "revert"
    skipKeyword "void"
    return (RevertInst (Operands[]))
revertInst :: Parser RevertInst
revertInst = try revertInstVoid <|> revertInstWithOperands

data StopInst = StopInst deriving (Show, Eq)
stopInst :: Parser StopInst
stopInst = do
    skipKeyword "stop"
    return StopInst

data LogInst = LogInst
  { logIndex :: Operand
  , logSizeInBytes :: Operand
  , logContent :: Operands
  } deriving (Show, Eq)
logInstWithContent :: Parser LogInst
logInstWithContent = do
    skipKeyword "log"
    i <- operand
    void comma
    s <- operand
    void comma
    c <- nonEmptyOperands
    return LogInst {logIndex = i, logSizeInBytes = s, logContent = c}
logInstNoContent :: Parser LogInst
logInstNoContent = do
    skipKeyword "log"
    i <- operand
    void comma
    s <- operand
    return LogInst {logIndex = i, logSizeInBytes = s, logContent = Operands []}
logInst :: Parser LogInst
logInst = try logInstWithContent <|> logInstNoContent

data CreateInst = CreateInst
  { createLValue :: LValue
  , createContractName :: IeleName
  , createOperands :: Operands
  , createSendValue :: Operand
  } deriving (Show, Eq)
createInst :: Parser CreateInst
createInst = do
    l <- lValue
    void equal
    skipKeyword "create"
    n <- ieleNameToken
    void openParenthesis
    o <- operands
    void closedParenthesis
    skipKeyword "send"
    v <- operand
    return
      CreateInst
        { createLValue = l
        , createContractName = n
        , createOperands = o
        , createSendValue = v
        }

newtype SelfdestructInst = SelfdestructInst
  { selfdestructAccountToSendBalance :: Operand} deriving (Show, Eq)
selfDestructInst :: Parser SelfdestructInst
selfDestructInst = do
  skipKeyword "selfdestruct"
  a <- operand
  return SelfdestructInst {selfdestructAccountToSendBalance = a}

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
      try (instructionWrapper assignInst InstructionAssign)
  <|> try (instructionWrapper loadInst InstructionLoad)
  <|> try (instructionWrapper storeInst InstructionStore)
  <|> try (instructionWrapper sloadInst InstructionSLoad)
  <|> try (instructionWrapper sstoreInst InstructionSStore)
  <|> try (instructionWrapper isZeroInst InstructionIsZero)
  <|> try (instructionWrapper notInst InstructionNot)
  <|> try (instructionWrapper binaryOperationInst InstructionBinaryOperation)
  <|> try (instructionWrapper ternaryOperationInst InstructionTernaryOperation)
  <|> try (instructionWrapper predicateOperationInst InstructionPredicateOperation)
  -- condJumpInst must be before jumpInst otherwise it's harder to parse things,
  -- i.e. 'br 10, a' gets parsed as 'br 10' with a leftover of ', a'.
  <|> try (instructionWrapper condJumpInst InstructionCondJump)
  <|> try (instructionWrapper jumpInst InstructionJump)
  <|> try (instructionWrapper localCallInst InstructionLocalCall)
  <|> try (instructionWrapper accountCallInst InstructionAccountCall)
  <|> try (instructionWrapper sendInst InstructionSend)
  <|> try (instructionWrapper returnInst InstructionReturn)
  <|> try (instructionWrapper revertInst InstructionRevert)
  <|> try (instructionWrapper stopInst InstructionStop)
  <|> try (instructionWrapper logInst InstructionLog)
  <|> try (instructionWrapper createInst InstructionCreate)
  <|> try (instructionWrapper selfDestructInst InstructionSelfdestruct)
-- The try at the end, which I usually leave out, is needed here because we may
-- try to parse a label as an instruction and we must be able to recover from
-- that.

newtype Instructions = Instructions [Instruction] deriving (Show, Eq)
instructions :: Parser Instructions
instructions = wrapperParser (many instruction) Instructions

data LabeledBlock = LabeledBlock
  { labeledBlockLabel :: IeleName
  , labeledBlockInstructions :: Instructions
  } deriving (Show, Eq)
labeledBlock :: Parser LabeledBlock
labeledBlock = do
    l <- ieleNameToken
    void colon
    i <- instructions
    return LabeledBlock {labeledBlockLabel = l, labeledBlockInstructions = i}

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype LabeledBlocks = LabeledBlocks [LabeledBlock] deriving (Show, Eq)
labeledBlocks :: Parser LabeledBlocks
labeledBlocks = wrapperParser (many labeledBlock) LabeledBlocks

-- Matches the empty string, so it may enter an infinite loop when used with,
-- say, 'many'.
newtype FunctionParameters = FunctionParameters LocalNames deriving (Show, Eq)
functionParameters :: Parser FunctionParameters
functionParameters = wrapperParser localNames FunctionParameters

data FunctionSignature = FunctionSignature
  { functionSignatureName :: GlobalName
  , functionSignatureParameters :: FunctionParameters
  } deriving (Show, Eq)
functionSignature :: Parser FunctionSignature
functionSignature = do
    g <- globalName
    void openParenthesis
    p <- functionParameters
    void closedParenthesis
    return
      FunctionSignature
        {functionSignatureName = g, functionSignatureParameters = p}

data FunctionDefinition = FunctionDefinition
  { functionDefinitionSignature :: FunctionSignature
  , functionDefinitionBlocks :: LabeledBlocks
  } deriving (Show, Eq)
functionDefinition :: Parser FunctionDefinition
functionDefinition = do
    skipKeyword "define"
    s <- functionSignature
    void openCurlyBrace
    b <- labeledBlocks
    void closedCurlyBrace
    return
      FunctionDefinition
        {functionDefinitionSignature = s, functionDefinitionBlocks = b}

data GlobalVariableDefinition = GlobalVariableDefinition
  { globalVariableName :: GlobalName
  , globalVariableValue :: IntToken
  } deriving (Show, Eq)
globalVariableDefinition :: Parser GlobalVariableDefinition
globalVariableDefinition = do
    n <- globalName
    void equal
    c <- intToken
    return
      GlobalVariableDefinition {globalVariableName = n, globalVariableValue = c}

data TopLevelDefinition =
    TopLevelDefinitionGlobalVariable GlobalVariableDefinition
  | TopLevelDefinitionFunction FunctionDefinition
  deriving (Show, Eq)
topLevelDefinition :: Parser TopLevelDefinition
topLevelDefinition =
      wrapperParser globalVariableDefinition TopLevelDefinitionGlobalVariable
  <|> wrapperParser functionDefinition TopLevelDefinitionFunction

newtype TopLevelDefinitions =
  TopLevelDefinitions [TopLevelDefinition] deriving (Show, Eq)
topLevelDefinitions :: Parser TopLevelDefinitions
topLevelDefinitions =
    wrapperParser (many topLevelDefinition) TopLevelDefinitions

newtype Contract = Contract TopLevelDefinitions deriving (Show, Eq)
contract :: Parser Contract
contract = wrapperParser topLevelDefinitions Contract

ielePartialParser :: Parser Contract
ielePartialParser = do
  whitespace
  contract  -- returned value

ieleParser :: Parser Contract
ieleParser = ielePartialParser <* eof
