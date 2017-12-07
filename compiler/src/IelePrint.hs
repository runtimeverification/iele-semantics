module IelePrint (prettyContracts, prettyContract, prettyInst) where
import Prelude hiding (LT,EQ,GT)

import Text.PrettyPrint
import Data.List(intersperse)

import IeleInstructions
import IeleTypes

prettyContracts contracts = vcat (map prettyContract contracts) $+$ text ""

class PrettyNames n where
  prettyName :: n -> Doc
instance PrettyNames IeleName where
  prettyName (IeleNameText t) = text t
  prettyName (IeleNameNumber n) = int n
instance PrettyNames LocalName where
  prettyName (LocalName n) = char '%' <> prettyName n
instance PrettyNames GlobalName where
  prettyName (GlobalName n) = char '@' <> prettyName n

braceGroup :: Doc -> [Doc] -> Doc
braceGroup head body = head <+> char '{' $$ nest 2 (vcat body) $$ char '}'

braceGroupFlat :: Doc -> [Doc] -> Doc
braceGroupFlat head body = head <+> char '{' $$ vcat (intersperse blank body) $$ char '}'

blank :: Doc
blank = text ""

prettyContract :: Contract -> Doc
prettyContract (Contract name size defs) =
  braceGroupFlat (text "contract" <+> prettyName name
                  <+> maybe empty (\sz -> char '!' <> int sz) size)
    (empty:map prettyDef defs++[empty])

prettyDef :: TopLevelDefinition -> Doc
prettyDef (TopLevelDefinitionFunction funDef) = prettyFunDef funDef
-- prettyDef (TopLevelDefinitionContract conRef) = prettyConRef conRef

prettyFunDef (FunctionDefinition public name args entry blocks) =
  braceGroupFlat
    (text "define" <+> (if public then text "public" else empty)
      <+> prettyName name <> char '(' <> commaList (map prettyName args) <> char ')')
    (nest 4 (vcat (map prettyInst entry))
    :map prettyBlock blocks)

commaList :: [Doc] -> Doc
commaList docs = hsep (punctuate comma docs)

prettyBlock (LabeledBlock name insts) =
  prettyName name <> char ':' $+$
  nest 4 (vcat (map prettyInst insts))

prettyInst :: Instruction -> Doc
prettyInst (IeleInst i) = prettyIeleInst i
prettyInst (SugarInst s) = prettySugarInst s

prettySugarInst (LoadGlobal tgt g) =
  prettyVal tgt <+> char '=' <+> prettyName g

prettyIeleInst :: IeleOpP -> Doc
prettyIeleInst (Op MOVE tgt [src]) = inst [tgt] empty [prettyVal src]
prettyIeleInst (Op (IeleOpcodesQuery q) tgt []) =
  prettyVal tgt <+> char '=' <+> text "call" <+> text (queryName q) <> text "()"
prettyIeleInst (Op op tgt [a]) = instVals [tgt] (op1Name op) [a]
prettyIeleInst (Op op tgt [a,b]) = instVals [tgt] (op2Name op) [a,b]
prettyIeleInst (VoidOp (JUMP lbl) []) = prettyJump lbl
prettyIeleInst (VoidOp (JUMPI lbl) [reg]) = prettyCondJump reg lbl
prettyIeleInst (VoidOp (RETURN _) args) = prettyReturn args
prettyIeleInst (VoidOp INVALID []) = text "call @iele.invalid()"
prettyIeleInst (VoidOp SSTORE [val,tgt]) = instVals [] "sstore" [val,tgt]
prettyIeleInst (VoidOp MSTORE [val,tgt]) = instVals [] "mstore" [val,tgt]
prettyIeleInst (VoidOp (LOG _) args) = instVals [] "log" args
prettyIeleInst (LiOp LOADPOS tgt i) = inst [tgt] empty [integer i]
prettyIeleInst (LiOp LOADNEG tgt i) = inst [tgt] empty [integer (negate i)]
prettyIeleInst (CallOp (LOCALCALL name _ _) results args) =
  prettyResults results <+> text "call" <+> prettyName name
    <> char '(' <> commaList (map prettyVal args) <> char ')'
prettyIeleInst (CallOp (CALL name _ _) results (gas:acct:val:args)) =
  prettyResults results <+> text "call" <+> prettyName name
    <+> text "at" <+> prettyVal acct
    <> char '(' <> commaList (map prettyVal args) <> char ')'
    <+> text "send" <+> prettyVal val <> comma <+> text "gaslimit" <+> prettyVal gas
prettyIeleInst (CallOp (STATICCALL name _ _) results (gas:acct:val:args)) =
  prettyResults results <+> text "staticcall" <+> prettyName name
    <+> text "at" <+> prettyVal acct
    <> char '(' <> commaList (map prettyVal args) <> char ')'
    <+> text "send" <+> prettyVal val <> comma <+> text "gaslimit" <+> prettyVal gas

prettyIeleInst o = text ('#':show o)

queryName CALLER = "@iele.caller"
queryName q = '#':show q

op1Name o = case o of
  SLOAD -> "sload"
  _ -> '#':show o

op2Name o = case o of
  NE -> "cmp ne"
  LT -> "cmp lt"
  GT -> "cmp gt"
  LE -> "cmp le"
  GE -> "cmp ge"
  EQ -> "cmp eq"

  ADD -> "add"
  SUB -> "sub"
  MUL -> "mul"

  AND -> "and"
  OR -> "or"

  _ -> '#':show o

prettyVal (LValueLocalName n) = prettyName n

prettyJump :: IeleName -> Doc
prettyJump dest =        text "br" <+> prettyName dest
prettyCondJump :: LValue -> IeleName -> Doc
prettyCondJump op dest = text "br" <+> prettyVal op <> comma <+> prettyName dest

prettyReturn [] = text "ret void"
prettyReturn args = instVals [] "ret" args

prettySend amount dest = text "send" <+> prettyVal amount <+> text "to" <+> prettyVal dest

prettyResults :: [LValue] -> Doc
prettyResults [] = empty
prettyResults results = commaList (map prettyVal results) <+> char '='

inst :: [LValue] -> Doc -> [Doc] -> Doc
inst results head args =
  prettyResults results <+> head <+> commaList args

instVals :: [LValue] -> String -> [LValue] -> Doc
instVals results name vals = inst results (text name) (map prettyVal vals)
