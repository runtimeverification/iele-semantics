{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DeriveDataTypeable #-}
module IeleTypes
    ( ContractP (ContractP, contractName, contractSize, contractDefinitions)
    , FunctionDefinitionP
    , FunctionDefinition
      ( FunctionDefinition,
        functionDefinitionPublic, functionDefinitionName,
        functionDefinitionParameters , functionDefinitionBlocks)
    , name
    , parameters
    , blocks
    , IeleName(IeleNameNumber,IeleNameText)
    , IntToken (IntToken)
    , LabeledBlockP
    , LabeledBlock (LabeledBlock, labeledBlockLabel, labeledBlockInstructions)
    , label, instructions
    , GlobalName (GlobalName)
    , LocalName (LocalName)
    , LValue (LValueLocalName)
    , Operand (RegOperand,ImmOperand,GlobalOperand)
    , TopLevelDefinition
      (TopLevelDefinitionContract, TopLevelDefinitionFunction, TopLevelDefinitionGlobal)

    -- type synonyms for parser's instantiations of IeleInstruction's types
    , Instruction
    , IeleOpP
    , IeleOpcode0P

    , _LValueLocalName
    , _LocalName
    , _GlobalName
    , _TopLevelDefinitionFunction
    , contractBlocks
    , functionInsts

    , FunctionDefinitionD
    , ContractD
      ( ContractD, functionNames, externalContracts, functionDefinitions)
    ) where
import Data.Char
import Data.String
import Data.Maybe(maybeToList)
import Data.List(stripPrefix)
import Data.Data
import Data.Word(Word16)

import Control.Lens.TH
import Language.Haskell.TH hiding (Type)
import Data.Data.Lens(template)
import Control.Lens
import Data.Kind

import IeleTHUtil
import IeleInstructions

type IeleOpP = IeleOpG IeleName GlobalName IeleName LValue Operand
type IeleOpcode0P = IeleOpcode0G GlobalName IeleName

type Instruction = IeleOpP

data IeleName = IeleNameNumber Int | IeleNameText String
  deriving (Show, Eq, Ord, Data)

instance Read IeleName where
  readsPrec _ name@(c:cs)
    | isDigit c = [(IeleNameNumber (read (c:takeWhile isDigit cs)),
                                            dropWhile isDigit cs)]
    | okFirst c = [(IeleNameText (c:takeWhile okRest cs),
                                    dropWhile okRest cs)]
   where
    okFirst c = isLetter c || c `elem` "._-$"
    okRest c = okFirst c || isDigit c
  readsPrec _ _ = []

instance IsString IeleName where
  fromString str
    | [(name,"")] <- reads str = name
    | otherwise = error $ "Invalid IeleName "++show str

newtype IntToken = IntToken Integer deriving (Show, Eq, Ord, Data)
newtype GlobalName = GlobalName IeleName  -- TODO: A string may be more efficient than the full AST
  deriving (Show, Eq, Ord, Data)

instance IsString GlobalName where
  fromString ('@':str) = GlobalName (fromString str)
  fromString str = error $ "GlobalName must begin with @ in "++show str

newtype LocalName = LocalName IeleName  -- TODO: A string may be more efficient than the full AST
  deriving (Show, Eq, Data)

instance IsString LocalName where
  fromString ('%':str) = LocalName (fromString str)
  fromString str = error $ "LocalName must begin with % in "++show str

newtype LValue = LValueLocalName LocalName
  deriving (Show, Eq, Data)

instance IsString LValue where
  fromString str@('%':_) = LValueLocalName (fromString str)
  fromString str = error $ "LValue must begin with % or @ in "++show str

data Operand =
   RegOperand LValue
 | ImmOperand IntToken
 | GlobalOperand GlobalName
  deriving (Show, Eq, Data)

instance IsString Operand where
  fromString str@('%':_) = RegOperand (fromString str)
  fromString str@('@':_) = GlobalOperand (fromString str)
  fromString str
    | [(imm,"")] <- reads str = ImmOperand (IntToken imm)
  fromString str = error $ "not an operand: "++str

type LabeledBlockP = LabeledBlock IeleName Instruction
data LabeledBlock lblId instruction = LabeledBlock
  { labeledBlockLabel :: lblId
  , labeledBlockInstructions :: [instruction]
  } deriving (Show, Eq, Data)
label :: Lens
  (LabeledBlock lbl inst)
  (LabeledBlock lbl' inst)
  lbl
  lbl'
label f (LabeledBlock label insts) = (\l' -> LabeledBlock l' insts) <$> f label
instructions :: Lens
  (LabeledBlock lbl inst)
  (LabeledBlock lbl inst')
  [inst]
  [inst']
instructions f (LabeledBlock label insts) = LabeledBlock label <$> f insts

type FunctionDefinitionP = FunctionDefinition GlobalName IeleName LValue Instruction
data FunctionDefinition funId blockId arg instruction = FunctionDefinition
  { functionDefinitionPublic :: Bool
  , functionDefinitionName :: funId
  , functionDefinitionParameters :: [arg]
  , functionDefinitionEntry :: [instruction]
  , functionDefinitionBlocks :: [LabeledBlock blockId instruction]
  } deriving (Show, Eq, Data)

name :: Lens
  (FunctionDefinition fun lbl arg inst)
  (FunctionDefinition fun' lbl arg inst)
  fun fun'
name f (FunctionDefinition public name params entry blocks) =
  (\name' -> FunctionDefinition public name' params entry blocks) <$> f name

parameters :: Lens
  (FunctionDefinition fun lbl arg inst)
  (FunctionDefinition fun lbl arg' inst)
  [arg] [arg']
parameters f (FunctionDefinition public name params entry blocks) =
  (\params' -> FunctionDefinition public name params' entry blocks) <$> f params

blocks :: Lens
  (FunctionDefinition fun lbl arg inst)
  (FunctionDefinition fun lbl' arg inst)
  [LabeledBlock lbl inst]
  [LabeledBlock lbl' inst]
blocks f (FunctionDefinition public name params entry blocks)
 = FunctionDefinition public name params entry <$> f blocks

data TopLevelDefinition =
    TopLevelDefinitionContract IeleName
  | TopLevelDefinitionFunction FunctionDefinitionP
  | TopLevelDefinitionGlobal GlobalName Integer
  deriving (Show, Eq, Data)
data ContractP = ContractP
  { contractName :: IeleName
  , contractSize :: Maybe Int
  , contractDefinitions :: [TopLevelDefinition]
  }
  deriving (Show, Eq, Data)

type FunctionDefinitionD contract funId blkId lval op =
  FunctionDefinition funId blkId lval (IeleOpG contract funId blkId lval op)

data ContractD contractId = ContractD
  { functionNames :: [String]
  , externalContracts :: [contractId]
  , functionDefinitions :: [FunctionDefinitionD Word16 Word16 Word16 Int Int]
  }

makePrisms ''TopLevelDefinition
makePrisms ''LValue
makePrisms ''LocalName
makePrisms ''GlobalName

contractBlocks :: Traversal' ContractP [Instruction]
contractBlocks = template

functionInsts :: Traversal
  (FunctionDefinition funId blockId arg inst)
  (FunctionDefinition funId blockId arg inst')
  [inst] [inst']
functionInsts f (FunctionDefinition public name args entry blocks) =
  FunctionDefinition public name args
     <$> f entry
     <*> traverse (instructions f) blocks
