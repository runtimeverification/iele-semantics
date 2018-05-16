{-# LANGUAGE ScopedTypeVariables #-}
module IeleDesugar where
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Map.Strict(Map,(!))
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B

import Data.Bifunctor

import Data.Word
import Data.Data

import Control.Lens
import Data.Data.Lens

import IeleInstructions
import IeleAssembler
import IeleTypes
import IelePrint(prettyInst,prettyName)

type IeleNameNum = Int

lookupGlobal :: Map GlobalName Integer
             -> Operand
             -> Either Integer LValue
lookupGlobal definitions op = case op of
  RegOperand r -> Right r
  ImmOperand (IntToken i) -> Left i
  GlobalOperand g
    | Just val <- Map.lookup g definitions -> Left val
    | otherwise -> error $ "Undefined global "++show (prettyName g)++" in operand"

expandImmediates :: IeleOpG con fun lbl LValue (Either Integer LValue)
                 -> [IeleOpG con fun lbl LValue LValue]
expandImmediates (Op MOVE tgt [Left imm]) = [loadImm tgt imm]
expandImmediates inst =
  let processArg (loads,nextTmp) (Left imm) =
         let tmpReg = LValueLocalName (LocalName (IeleNameText ("iele.tmp"++show nextTmp)))
         in (((loadImm tmpReg imm:loads),1+nextTmp),tmpReg)
      processArg acc (Right reg) = (acc,reg)
      ((loads,_),inst') = mapAccumLOf ieleOpArg processArg ([],0) inst
  in reverse loads++[inst']

desugarFundef :: Map GlobalName Integer
              -> FunctionDefinitionD IeleName Word16 IeleName LValue Operand
              -> FunctionDefinitionD IeleName Word16 IeleName LValue LValue
desugarFundef globals fundef =
  fundef & functionInsts %~ concatMap expandImmediates
                          . (traverse . ieleOpArg %~ lookupGlobal globals)

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

applyIeleNameMap :: Map String Int -> IeleName -> Int
applyIeleNameMap _ (IeleNameNumber n) = n
applyIeleNameMap mapping (IeleNameText t)
  | Just ix <- Map.lookup t mapping = ix
  | otherwise = error $ "Undefined name "++t

numberBlocks :: FunctionDefinitionD conId funId IeleName reg reg
             -> FunctionDefinitionD conId funId Word16 reg reg
numberBlocks funDef =
  let rename = fromIntegral . applyIeleNameMap (numberIeleNames (funDef ^.. blocks . traverse . label))
  in funDef & blocks . traverse . label %~ rename
            & functionInsts . traverse . ieleOpJumpDest %~ rename

assignLocals :: [IeleName] -> [IeleName] -> Map String Int
assignLocals args bodyRefs =
  let usedNums = IntSet.fromList [x | IeleNameNumber x <- bodyRefs]
      usedNames = [s | IeleNameText s <- bodyRefs]
      namedArgs = [(t,ix) | (IeleNameText t,ix) <- zip args [0..]]
  in extendAssignment (Map.fromList namedArgs) usedNums usedNames

numberLocals :: FunctionDefinitionD conId funId blkId LValue LValue
             -> FunctionDefinitionD conId funId blkId Int Int
numberLocals funDef =
  let funLocalRegs f = (functionInsts . traverse . ieleOpRegs') f
      funParams f = (parameters . traverse) f
      assignment = assignLocals (funDef ^.. funParams . _LValueLocalName . _LocalName)
                                (funDef ^.. funLocalRegs . _LValueLocalName . _LocalName)
      rename (LValueLocalName (LocalName name)) = applyIeleNameMap assignment name
  in funDef & parameters . traverse  %~ rename
            & funLocalRegs %~ rename

setNub :: (Ord a) => [a] -> [a]
setNub l = go Set.empty l
  where go _ [] = []
        go acc (x:xs) = if Set.member x acc
                        then go acc xs
                        else x:go (Set.insert x acc) xs

numberFunctions :: [FunctionDefinitionP]
                -> ([String],(Map IeleName Word16),[FunctionDefinitionD IeleName Word16 IeleName LValue Operand])
numberFunctions fundefs =
  let declaredFuns = fundefs ^.. traverse . name . _GlobalName
      calledFuns = fundefs ^.. traverse . functionInsts . traverse . ieleOpFunId . _GlobalName
      functionNames = setNub (IeleNameText "init":declaredFuns++calledFuns)
      nextId = length functionNames
      functionMapping = Map.fromList (zip functionNames [fromIntegral 0..])
      renameFunction (GlobalName g) = functionMapping Map.! g
  in ([n | IeleNameText n <- tail functionNames],
      functionMapping,
      fundefs & traverse . name %~ renameFunction
              & traverse . functionInsts . traverse . ieleOpFunId %~ renameFunction)

numberContracts :: [IeleName]
                -> [String]
                -> [FunctionDefinitionD IeleName Word16 blk reg reg]
                -> [FunctionDefinitionD Word16 Word16 blk reg reg]
numberContracts declaredContracts functionNames fundefs =
  let nextId = length functionNames + 1
      contractMapping = Map.fromList (zip declaredContracts [fromIntegral nextId..])
      renameContract c = contractMapping Map.! c
  in fundefs & traverse . functionInsts . traverse . ieleOpContract %~ renameContract

processContract :: ContractP -> (IeleName, ContractD IeleName)
processContract (ContractP name _ definitions) =
  let externalContracts = [con | TopLevelDefinitionContract con <- definitions]
      globalDefs = Map.fromList [(g,v) | TopLevelDefinitionGlobal g v <- definitions]
      funDefs = [f | TopLevelDefinitionFunction f <- definitions]
      (functionNames,functionMapping,funDefsI) = numberFunctions funDefs
      funDefIds = Map.fromList [(GlobalName name, toInteger id) | (name, id) <- Map.toList functionMapping]
      allDefs = Map.union globalDefs funDefIds
      funDefs' = map (numberLocals . numberBlocks . desugarFundef allDefs) funDefsI
      funDefsD = numberContracts externalContracts functionNames funDefs'
  in (name, ContractD functionNames externalContracts funDefsD)

flattenFundef :: FunctionDefinitionD Word16 Word16 Word16 Int Int -> [IeleOp]
flattenFundef (FunctionDefinition isPublic name args entry blocks) =
  VoidOp ((if isPublic then EXTCALLDEST else CALLDEST) name (argsLength args)) []
  :entry++concatMap flattenBlock blocks

flattenBlock :: LabeledBlock Word16 IeleOp  -> [IeleOp]
flattenBlock (LabeledBlock lbl insts) =
  VoidOp (JUMPDEST lbl) []:insts

neededBits :: Integral a => a -> Int
neededBits max = ceiling (logBase 2 (fromIntegral max+1))

addRegisterCount insts =
  let nbits = case maximumOf (traverse . ieleOpRegs') insts of
            Nothing -> 0
            Just maxReg -> neededBits maxReg
  in VoidOp (REGISTERS (fromIntegral nbits)) []:insts

compileContract :: Map IeleName B.ByteString -> ContractP -> [IeleOp]
compileContract childContracts contract =
  let (_, ContractD funNames externalContracts funDefs) = processContract contract
      ops' = [VoidOp (FUNCTION t) [] | t <- funNames]
             ++ [VoidOp (CONTRACT (childContracts ! c)) [] | c <- externalContracts]
             ++ concatMap flattenFundef funDefs
  in addRegisterCount ops'

compileContracts :: [ContractP] -> [IeleOp]
compileContracts [] = []
compileContracts (c:cs) = go Map.empty c cs
  where go childContracts main [] = compileContract childContracts main
        go childContracts c (c':cs) =
          go (Map.insert (contractName c)
                         (assemble (compileContract childContracts c))
                         childContracts)
             c' cs
