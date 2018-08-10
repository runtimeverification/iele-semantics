module IelePrint
  ( prettyContractsP
  , prettyContractP
  , prettyInst
  , prettyContractD
  , prettyContractsD
  , PrettyNames(prettyName)
  ) where
import Prelude hiding (LT,EQ,GT)

import Text.PrettyPrint
import Data.List(intersperse)
import Data.Word(Word16)

import Control.Lens

import Codec.Binary.Base16(b16Enc)

import IeleInstructions
import IeleTypes

prettyContractsP :: [ContractP] -> Doc
prettyContractsP contracts = vcat (map prettyContractP contracts) $+$ text ""

prettyContractsD :: [(IeleName,ContractD IeleName)] -> Doc
prettyContractsD contracts = vcat (map prettyContractD contracts) $+$ text ""

class PrettyNames n where
  prettyName :: n -> Doc
instance PrettyNames IeleName where
  prettyName (IeleNameText t) = text t
  prettyName (IeleNameNumber n) = int n
instance PrettyNames LocalName where
  prettyName (LocalName n) = char '%' <> prettyName n
instance PrettyNames GlobalName where
  prettyName (GlobalName n) = char '@' <> prettyName n
instance PrettyNames LValue where
  prettyName (LValueLocalName l) = prettyName l

braceGroup :: Doc -> [Doc] -> Doc
braceGroup head body = head <+> char '{' $$ nest 2 (vcat body) $$ char '}'

braceGroupFlat :: Doc -> [Doc] -> Doc
braceGroupFlat head body = head <+> char '{' $$ vcat body $$ char '}'

blank :: Doc
blank = text ""

prettyContractP :: ContractP -> Doc
prettyContractP (ContractP name size defs) =
  braceGroup (text "contract" <+> prettyName name
                <+> maybe empty (\sz -> char '!' <> int sz) size)
    (empty:map prettyDef defs++[empty])

prettyContractD :: (IeleName,ContractD IeleName) -> Doc
prettyContractD (cname, ContractD functionNames externalContracts functionDefinitions) =
  braceGroup (text "contract" <+> prettyName cname) $
    (if null functionNames then []
     else [vcat [text "!function "<+> text (show f) | f <- functionNames]
          ,blank])
  ++(if null externalContracts then []
     else [vcat [text "external contract"<+>prettyName c | c <- externalContracts]
          ,blank])
  ++intersperse blank (map (prettyFunDef . formatDef) functionDefinitions)
 where
  numToLValue n = LValueLocalName (LocalName (IeleNameNumber n))
  formatDef :: FunctionDefinitionD Word16 Word16 Word16 Int Int
            -> FunctionDefinition GlobalName IeleName LValue Doc
  formatDef def = def & name %~ GlobalName . IeleNameNumber . fromIntegral
                      & parameters . traverse %~ numToLValue
                      & blocks . traverse . label %~ IeleNameNumber . fromIntegral
                      & functionInsts . traverse %~ prettyOp
  formatOp :: IeleOp -> IeleOpG IeleName GlobalName IeleName LValue Doc
  formatOp op = runIdentity $ retypeIeleOp
    (pure . IeleNameNumber . fromIntegral )
    (pure . GlobalName . IeleNameNumber . fromIntegral)
    (pure . IeleNameNumber . fromIntegral)
    (pure . LValueLocalName . LocalName . IeleNameNumber . fromIntegral)
    (pure . prettyName . LValueLocalName . LocalName . IeleNameNumber)
    op

  prettyOp :: IeleOp -> Doc
  prettyOp op = prettyIeleInst (formatOp op)

prettyDef :: TopLevelDefinition -> Doc
prettyDef (TopLevelDefinitionFunction funDef) = prettyFunDef
  (funDef & functionInsts . traverse %~ prettyInst)
prettyDef (TopLevelDefinitionContract conRef) =
  text "external" <+> text "contract" <+> prettyName conRef
prettyDef (TopLevelDefinitionGlobal g val) =
  prettyName g <+> char '=' <+> integer val

prettyFunDef :: (PrettyNames contractId, PrettyNames funId)
             => FunctionDefinition contractId funId LValue Doc -> Doc
prettyFunDef (FunctionDefinition public name args entry blocks) =
  braceGroupFlat
    (text "define" <+> (if public then text "public" else empty)
      <+> prettyName name <> char '(' <> commaList (map prettyName args) <> char ')')

    (nest 2 (vcat entry)
    :map prettyBlock blocks)

commaList :: [Doc] -> Doc
commaList docs = hsep (punctuate comma docs)

prettyBlock (LabeledBlock name insts) =
  prettyName name <> char ':' $+$
  nest 2 (vcat insts)

prettyInst :: Instruction -> Doc
prettyInst i = prettyIeleInst (over ieleOpArg prettyOperand i)

prettyOperand (RegOperand (LValueLocalName l)) = prettyName l
prettyOperand (GlobalOperand g) = prettyName g
prettyOperand (ImmOperand (IntToken i)) = integer i

type IeleOpPretty = IeleOpG IeleName GlobalName IeleName LValue Doc
prettyIeleInst Nop = text "nop"
prettyIeleInst (Op op1 tgt args) = prettyName tgt <+> char '=' <+> prettyOp1 op1 args
prettyIeleInst (VoidOp op args) = prettyVoidOp op args
prettyIeleInst (LiOp LOADPOS tgt i) = inst [tgt] empty [integer i]
prettyIeleInst (LiOp LOADNEG tgt i) = inst [tgt] empty [integer (negate i)]
prettyIeleInst (CallOp (LOCALCALL name _ _) results args) =
  prettyResults results <+> text "call" <+> prettyName name
    <> char '(' <> commaList args <> char ')'
prettyIeleInst (CallOp (LOCALCALLDYN  _ _) results (name : args)) =
  prettyResults results <+> text "call" <+> name
    <> char '(' <> commaList args <> char ')'
prettyIeleInst (CallOp (CALL name _ _) results allArgs) = case results of
  [] -> error "external call instruction must have at least one result"
  _ -> case allArgs of
         (gas:acct:val:args) ->
           prettyResults results <+> text "call" <+> prettyName name
             <+> text "at" <+> acct
             <> char '(' <> commaList args <> char ')'
             <+> text "send" <+> val <> comma <+> text "gaslimit" <+> gas
         _ -> error "external call instruction must encode at least target, gaslimit, and value arguments"
prettyIeleInst (CallOp (CALLDYN _ _) results allArgs) = case results of
  [] -> error "external call instruction must have at least one result"
  _ -> case allArgs of
         (name:gas:acct:val:args) ->
           prettyResults results <+> text "call" <+> name
             <+> text "at" <+> acct
             <> char '(' <> commaList args <> char ')'
             <+> text "send" <+> val <> comma <+> text "gaslimit" <+> gas
         _ -> error "external call instruction must encode at least target, gaslimit, and value arguments"
prettyIeleInst (CallOp (STATICCALL name _ _) results allArgs) = case results of
  [] -> error "external call instruction must have at least one result"
  _ -> case allArgs of
         (gas:acct:args) ->
           prettyResults results <+> text "staticcall" <+> prettyName name
             <+> text "at" <+> acct
             <> char '(' <> commaList args <> char ')'
             <+> text "gaslimit" <+> gas
         _ -> error "external staticcall instruction must encode at least target and gaslimit arguments"
prettyIeleInst (CallOp (STATICCALLDYN _ _) results allArgs) = case results of
  [] -> error "external call instruction must have at least one result"
  _ -> case allArgs of
         (name:gas:acct:args) ->
           prettyResults results <+> text "staticcall" <+> name
             <+> text "at" <+> acct
             <> char '(' <> commaList args <> char ')'
             <+> text "gaslimit" <+> gas
         _ -> error "external staticcall instruction must encode at least target and gaslimit arguments"
prettyIeleInst (CallOp (CALLADDRESS name) results allArgs) = case results of
  _:[] -> case allArgs of
         (acct:[]) ->
           prettyResults results <+> text "calladdress" <+> prettyName name
             <+> text "at" <+> acct
         _ -> error "external calladdress instruction must encode exactly one target argument"
  _ -> error "external calladdress instruction must have exactly one result"
prettyIeleInst (CallOp (CREATE name _) results (val:args)) = case results of
  [status,addr] ->
    prettyResults [status,addr] <+> text "create"
      <+> prettyName name <> char '(' <> commaList args <> char ')' <+> text "send" <+> val
  _ -> error "create instruction returns exactly two results"
prettyIeleInst (CallOp (CREATE _ _) _ _) =
  error "create instruction must encode at least one argument, for passed value"
prettyIeleInst (CallOp (COPYCREATE _) results (val:from:args)) = case results of
  [status,addr] ->
    prettyResults [status,addr] <+> text "create"
      <+> from <> char '(' <> commaList args <> char ')' <+> text "send" <+> val
  _ -> error "copycreate instruction returns exactly two results"
prettyIeleInst (CallOp (COPYCREATE _) _ _) =
  error "copycreate instruction must encode at least two arguments, for source account and passed value"


-- prettyIeleInst o = text ('#':show o)

prettyOp1 :: IeleOpcode1 -> [Doc] -> Doc
prettyOp1 op args = case op of
  MOVE -> commaList args
  IeleOpcodesQuery q ->
    text "call" <+> text (queryName q) <> char '(' <> commaList args <> char ')'
  ADD -> simple "add"
  MUL -> simple "mul"
  SUB -> simple "sub"
  DIV -> simple "div"
  MOD -> simple "mod"
  EXP -> simple "exp"
  ADDMOD -> simple "addmod"
  MULMOD -> simple "mulmod"
  EXPMOD -> simple "expmod"
  SIGNEXTEND -> simple "sext"
  TWOS -> simple "twos"
  BSWAP -> simple "bswap"
  LOG2 -> simple "log2"
  NE -> simple "cmp ne"
  LT -> simple "cmp lt"
  GT -> simple "cmp gt"
  LE -> simple "cmp le"
  GE -> simple "cmp ge"
  EQ -> simple "cmp eq"
  ISZERO -> simple "iszero"
  AND -> simple "and"
  OR -> simple "or"
  XOR -> simple "xor"
  NOT -> simple "not"
  BYTE -> simple "byte"
  SHIFT -> simple "shift"
  SHA3 -> simple "sha3"
  MLOADN -> simple "load"
  MLOAD -> simple "load"
  SLOAD -> simple "sload"
 where
  simple name = text name <+> commaList args

queryName q = case q of
  CALLER -> "@iele.caller"
  TIMESTAMP -> "@iele.timestamp"
  CALLVALUE -> "@iele.callvalue"
  ADDRESS -> "@iele.address"
  BALANCE -> "@iele.balance"
  ORIGIN -> "@iele.origin"
  CODESIZE -> "@iele.codesize"
  GASPRICE -> "@iele.gasprice"
  EXTCODESIZE -> "@iele.extcodesize"
  BLOCKHASH -> "@iele.blockhash"
  BENEFICIARY -> "@iele.beneficiary"
  NUMBER -> "@iele.number"
  DIFFICULTY -> "@iele.difficulty"
  GASLIMIT -> "@iele.gaslimit"
  MSIZE -> "@iele.msize"
  GAS -> "@iele.gas"

prettyVoidOp op args = case op of
  JUMP lbl -> case args of
    [] -> prettyJump lbl
    _ -> error "unconditional jump takes no register arguments"
  JUMPI lbl -> case args of
    [reg] -> prettyCondJump reg lbl
    _ -> error "conditional jump takes exactly one register argument"
  RETURN _ -> prettyReturn args
  REVERT -> simple "revert"
  INVALID -> text "call @iele.invalid(" <> commaList args <> char ')'
  SSTORE -> simple "sstore"
  MSTORE -> simple "store"
  MSTOREN -> simple "store"
  (REGISTERS n) -> text "!registers" <+> int (fromIntegral n)
  (JUMPDEST lbl) -> prettyName lbl <> char ':'
  (CALLDEST name arity) ->
    text "!calldest"<+> prettyName name
      <> char '(' <> int (fromIntegral (argsCount arity)) <> char ')'
  (EXTCALLDEST name arity) ->
    text "!extcalldest"<+> prettyName name
      <> char '(' <> int (fromIntegral (argsCount arity)) <> char ')'
  (FUNCTION f) -> text "!function" <+> text (show f)
  (CONTRACT n) -> text "!contract" <+> text (show (b16Enc n))
  SELFDESTRUCT -> simple "selfdestruct"
  LOG _ -> simple "log"
 where
  simple name = text name <+> commaList args

prettyVal (LValueLocalName n) = prettyName n

prettyJump :: IeleName -> Doc
prettyJump dest =        text "br" <+> prettyName dest
prettyCondJump :: Doc -> IeleName -> Doc
prettyCondJump op dest = text "br" <+> op <> comma <+> prettyName dest

prettyReturn [] = text "ret void"
prettyReturn args = instVals [] "ret" args

prettySend amount dest = text "send" <+> prettyVal amount <+> text "to" <+> prettyVal dest

prettyResults :: [LValue] -> Doc
prettyResults [] = empty
prettyResults results = commaList (map prettyVal results) <+> char '='

inst :: [LValue] -> Doc -> [Doc] -> Doc
inst results head args =
  prettyResults results <+> head <+> commaList args

instVals :: [LValue] -> String -> [Doc] -> Doc
instVals results name vals = inst results (text name) vals
