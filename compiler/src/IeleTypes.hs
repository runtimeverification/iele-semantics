{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DeriveDataTypeable #-}
module IeleTypes
    ( Contract (Contract, contractName, contractSize, contractDefinitions)
    , FunctionDefinition
      ( FunctionDefinition,
        functionDefinitionPublic, functionDefinitionName,
        functionDefinitionParameters , functionDefinitionBlocks)
    , IeleName(IeleNameNumber,IeleNameText)
    , IntToken (IntToken)
    , LabeledBlock (LabeledBlock, labeledBlockLabel, labeledBlockInstructions)
    , GlobalName (GlobalName)
    , LocalName (LocalName)
    , LValue (LValueLocalName)
    , TopLevelDefinition
      (TopLevelDefinitionContract, TopLevelDefinitionFunction, TopLevelDefinitionGlobal)

    -- type synonyms for parser's instantiations of IeleInstructions' types
    , Instruction(IeleInst,SugarInst)
    , SugarInstruction(LoadGlobal)
    , IeleOpP
    , IeleOpcode0P

    , HasName(name)
    , HasSize(size)
    , HasDefinitions(definitions)

    , HasPublic(public)
    , HasBlocks(blocks)
    , HasParameters(parameters)
    , HasLabel(label)
    , HasInstructions(instructions)
    , _LValueLocalName
    , _LocalName
    , _GlobalName
    , _TopLevelDefinitionFunction
    , instructionRegisters
    , instructionJumpDest
    , instructionCallName
    , instructionContractName
    , functionInsts
    ) where
import Data.Char
import Data.String
import Data.Data

import Control.Lens.TH
import Data.Data.Lens(template)
import Control.Lens

import IeleInstructions

type IeleOpP = IeleOpG IeleName GlobalName IeleName LValue
type IeleOpcode0P = IeleOpcode0G GlobalName IeleName

data Instruction =
   IeleInst IeleOpP
 | SugarInst SugarInstruction
  deriving (Show, Eq, Data)

data SugarInstruction =
  LoadGlobal LValue GlobalName
  deriving (Show, Eq, Data)

data IeleName = IeleNameNumber Int | IeleNameText String
  deriving (Show, Eq, Ord, Data)

instance Read IeleName where
  readsPrec _ name@(c:cs)
    | all isDigit name = [(IeleNameNumber (read name),"")]
    | okFirst c, all okRest cs = [(IeleNameText name,"")]
    | otherwise = []
   where
    okFirst c = isLetter c || c `elem` "._-$"
    okRest c = okFirst c || isDigit c

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

data LabeledBlock = LabeledBlock
  { labeledBlockLabel :: IeleName
  , labeledBlockInstructions :: [Instruction]
  } deriving (Show, Eq, Data)

data FunctionDefinition = FunctionDefinition
  { functionDefinitionPublic :: Bool
  , functionDefinitionName :: GlobalName
  , functionDefinitionParameters :: [LocalName]
  , functionDefinitionEntry :: [Instruction]
  , functionDefinitionBlocks :: [LabeledBlock]
  } deriving (Show, Eq, Data)
data TopLevelDefinition =
    TopLevelDefinitionContract IeleName
  | TopLevelDefinitionFunction FunctionDefinition
  | TopLevelDefinitionGlobal GlobalName Integer
  deriving (Show, Eq, Data)
data Contract = Contract
  { contractName :: IeleName
  , contractSize :: Maybe Int
  , contractDefinitions :: [TopLevelDefinition]
  }
  deriving (Show, Eq, Data)

instructionRegisters :: Traversal' Instruction LValue
instructionRegisters = template

instructionJumpDest :: Traversal' Instruction IeleName
instructionJumpDest f (IeleInst (VoidOp (JUMP tgt) []))
  = fmap (\tgt -> IeleInst (VoidOp (JUMP tgt) [])) (f tgt)
instructionJumpDest f (IeleInst (VoidOp (JUMPI tgt) [arg]))
  = fmap (\tgt -> IeleInst (VoidOp (JUMPI tgt) [arg])) (f tgt)
instructionJumpDest _ i = pure i

instructionCallName :: Traversal' IeleOpP GlobalName
instructionCallName f (CallOp callOp results args) =
  (\o' -> CallOp o' results args) <$> traverse f callOp
instructionCallName _ inst = pure inst

instructionContractName :: Traversal' IeleOpP IeleName
instructionContractName f (CallOp (CREATE c nargs) results args) =
  fmap (\c' -> CallOp (CREATE c' nargs) results args) (f c)
instructionContractName _ inst = pure inst

functionInsts :: Traversal' FunctionDefinition Instruction
functionInsts = template

makeLensesWith camelCaseFields ''FunctionDefinition
makeLensesWith camelCaseFields ''LabeledBlock
makeLensesWith camelCaseFields ''Contract
makePrisms ''TopLevelDefinition
makePrisms ''LValue
makePrisms ''LocalName
makePrisms ''GlobalName
