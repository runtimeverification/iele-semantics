{-# LANGUAGE ScopedTypeVariables #-}
module IeleDesugar where
import IeleTypes
import IeleInstructions

import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Map.Strict(Map,(!))
import qualified Data.Map.Strict as Map

import Data.Bifunctor

import Data.Word
import Data.Data

import Control.Lens
import Data.Data.Lens

type IeleNameNum = Int

{- | Assigns numbers to the given set, avoiding the already-used numbers -}
assignNumbers :: (Ord a) => IntSet -> [a] -> Map a Int
assignNumbers usedIds keys = extendAssignment Map.empty usedIds keys

{- | Extend an existing assignment. -}
extendAssignment :: (Ord a) => Map a Int -> IntSet -> [a] -> Map a Int
extendAssignment preAssigned usedIds keys =
  let unassigned = foldr (\a b used ->
                            if Set.member a used then b used else a : b (Set.insert a used))
                   (const []) keys (Map.keysSet preAssigned)
      usedIds' = IntSet.union (IntSet.fromList (Map.elems preAssigned)) usedIds
      freeIds  = [n | n <- [0..], not (IntSet.member n usedIds')]
  in Map.union preAssigned (Map.fromList (zip unassigned freeIds))

numberIeleNames :: [IeleName] -> Map String Int
numberIeleNames names =
  assignNumbers (IntSet.fromList [x | IeleNameNumber x <- names])
                [s | IeleNameText s <- names]

applyIeleNameMap :: Map String Int -> IeleName -> IeleName
applyIeleNameMap _ n@(IeleNameNumber _) = n
applyIeleNameMap mapping (IeleNameText t)
  | Just ix <- Map.lookup t mapping = IeleNameNumber ix

numberBlocks :: FunctionDefinition -> FunctionDefinition
numberBlocks funDef =
  let rename = applyIeleNameMap (numberIeleNames (funDef ^.. blocks . traverse . label))
  in funDef & blocks . traverse . label %~ rename
            & functionInsts . instructionJumpDest %~ rename

assignLocals :: [IeleName] -> [IeleName] -> Map String Int
assignLocals args bodyRefs =
  let usedNums = IntSet.fromList [x | IeleNameNumber x <- bodyRefs]
      usedNames = [s | IeleNameText s <- bodyRefs]
      namedArgs = [(t,ix) | (IeleNameText t,ix) <- zip args [0..]]
  in extendAssignment (Map.fromList namedArgs) usedNums usedNames

numberLocals :: FunctionDefinition -> FunctionDefinition
numberLocals funDef =
  let funLocalRegs, funParams :: Traversal' FunctionDefinition IeleName
      funLocalRegs = template . instructionRegisters . _LValueLocalName . _LocalName
      funParams = parameters . traverse . _LocalName
      rename = applyIeleNameMap (assignLocals (funDef ^.. funParams)
                                              (funDef ^.. funLocalRegs))
  in funDef & funParams %~ rename
            & funLocalRegs %~ rename

numberGlobals :: Contract -> Contract
numberGlobals contract =
  let globalRegs :: Traversal' Contract IeleName
      globalRegs = template . _LValueGlobalName . _GlobalName
      rename = applyIeleNameMap (numberIeleNames (contract ^.. globalRegs))
  in contract & template . _LValueGlobalName . _GlobalName %~ rename

functionDefinitions :: Traversal' Contract FunctionDefinition
functionDefinitions = template

calledFunctions :: Traversal' Contract IeleName
calledFunctions = template . instructionCallName . _GlobalName

declaredFunctions :: Traversal' Contract IeleName
declaredFunctions = functionDefinitions . name . _GlobalName

numberFunctions :: Contract -> ([IeleName],Contract)
numberFunctions contract = let
  funNames = contract ^.. declaredFunctions ++ contract ^.. calledFunctions
  funDecls = Set.delete (IeleNameText "init") (Set.fromList funNames)
  functionTable = Set.toList funDecls
  functionMapping = Map.insert (IeleNameText "init") (IeleNameNumber 0)
      (Map.fromList (zip functionTable (map IeleNameNumber [1..])))
 in (functionTable,
      contract & calledFunctions %~ (functionMapping !)
               & declaredFunctions %~ (functionMapping !))

processContract :: Contract -> ([IeleName],Contract)
processContract contract =
  let locallyNumbered = contract & functionDefinitions  %~ numberLocals . numberBlocks
  in numberFunctions (numberGlobals locallyNumbered)

type IeleOp' = IeleOpG Word16 Word16 Word16 LValue

flattenFundef :: FunctionDefinition -> [IeleOp']
flattenFundef (FunctionDefinition isPublic name args entry blocks) =
  let GlobalName (IeleNameNumber funNum) = name
      funNum16 = fromIntegral funNum
  in [VoidOp ((if isPublic then EXTCALLDEST else CALLDEST) funNum16 (argsLength args)) []]
  ++ flattenInsts entry
  ++ concatMap flattenBlock blocks

flattenBlock :: LabeledBlock -> [IeleOp']
flattenBlock (LabeledBlock (IeleNameNumber lbl) insts) =
  [VoidOp (JUMPDEST (fromIntegral lbl)) []]
  ++ flattenInsts insts

flattenInsts :: [Instruction] -> [IeleOp']
flattenInsts = map flattenInst

flattenInst :: Instruction -> IeleOp'
flattenInst Nop = Nop
flattenInst (Op op1 result args) = Op op1 result args
flattenInst (VoidOp op0 args) =
  VoidOp (bimap flattenGlobalName flattenName op0) args
flattenInst (CallOp callOp results args) =
  CallOp (bimap flattenName flattenGlobalName callOp) results args
flattenInst (LiOp op result val) = LiOp op result val

flattenName :: IeleName -> Word16
flattenName (IeleNameNumber i) = fromIntegral i
flattenName (IeleNameText t) = error $ "residual text name "++t

flattenGlobalName :: GlobalName -> Word16
flattenGlobalName (GlobalName i) = flattenName i

countRegisters :: [IeleOp'] -> [IeleOp]
countRegisters ops' =
  let maxRegs = maximumOf (traverse . traverse . to regNum) ops'
      nbits = case maxRegs of
        Nothing -> 0
        Just maxNum -> 1 + ceiling (logBase 2 (fromIntegral maxNum))
      globalOffset = 2^(nbits-1)
      regNum (LValueLocalName (LocalName (IeleNameNumber i))) = i
      regNum (LValueGlobalName (GlobalName (IeleNameNumber i))) = i
      regNum l = error $ "Residual textual name "++show l
      valToInt (LValueLocalName (LocalName (IeleNameNumber i))) = i
      valToInt (LValueGlobalName (GlobalName (IeleNameNumber i))) = globalOffset + i
  in [VoidOp (REGISTERS (fromIntegral nbits)) []]
     ++ over (traverse . traverse) valToInt ops'

compileContract :: Contract -> [IeleOp]
compileContract contract =
  let (functions,Contract _ _ defs) = processContract contract
      ops' = [VoidOp (FUNCTION t) [] | IeleNameText t <- functions]
             ++ concatMapOf (traverse . _TopLevelDefinitionFunction) flattenFundef defs
  in countRegisters ops'
