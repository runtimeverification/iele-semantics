{-# LANGUAGE DeriveDataTypeable, DeriveTraversable #-}
module IeleInstructions
  (length16
  ,Args,argsCount,mkArgs
  ,argsLength
  ,Rets,retsCount,mkRets
  ,retsLength
  ,IeleOpcode1(..)
  ,IeleOpcodeQuery(..)
  ,IeleOpcode0
  ,IeleOpcode0G(..)
  ,retypeOpcode0
  ,IeleOpcodeLi(..)
  ,IeleOpcodeCall(..)
  ,retypeOpcodeCall
  ,IeleOp
  ,IeleOpG(..)
  ,retypeIeleOp
  ,ieleOpContract
  ,ieleOpFunId
  ,ieleOpJumpDest
  ,ieleOpReg
  ,ieleOpArg
  ,ieleOpRegs'
  ,loadImm
  )
where
import Data.Data
import Control.Applicative
import Control.Lens
import Data.Functor.Identity

import qualified Data.ByteString as B
import Data.Word

import Data.Bifunctor

-- a utility for getting lengths of argument lists
length16 :: [a] -> Word16
length16 l = fromIntegral (length l)

newtype Args a = Args {argsCount :: a}
  deriving (Show, Eq, Data, Functor, Foldable, Traversable)

newtype Rets a = Rets {retsCount :: a}
  deriving (Show, Eq, Data, Functor, Foldable, Traversable)

mkArgs :: a -> Args a
mkArgs = Args

mkRets :: a -> Rets a
mkRets = Rets

argsLength :: [a] -> Args Word16
argsLength l = Args (length16 l)

retsLength :: [a] -> Rets Word16
retsLength l = Rets (length16 l)

data IeleOpcode1 =
   ADD
 | MUL
 | SUB
 | DIV
 | MOD
 | EXP
 | ADDMOD
 | MULMOD
 | EXPMOD

 | SIGNEXTEND
 | TWOS
 | BSWAP

 | NE
 | LT
 | GT
 | LE
 | GE
 | EQ

 | ISZERO
 | LOG2
 | AND
 | OR
 | XOR
 | NOT

 | BYTE
 | SHIFT
 | SHA3

 | MLOADN
 | MLOAD
 | SLOAD

 | MOVE

 | IeleOpcodesQuery IeleOpcodeQuery
  deriving (Show, Eq, Data)

data IeleOpcodeQuery =
   ADDRESS
 | BALANCE
 | ORIGIN
 | CALLER
 | CALLVALUE
 | CODESIZE
 | GASPRICE
 | EXTCODESIZE
 | BLOCKHASH
 | BENEFICIARY
 | TIMESTAMP
 | NUMBER
 | DIFFICULTY
 | GASLIMIT

 | MSIZE
 | GAS
  deriving (Show, Eq, Data)

type IeleOpcode0 = IeleOpcode0G Word16 Word16
data IeleOpcode0G funId lblId =
   MSTOREN
 | MSTORE
 | SSTORE

 | REGISTERS Word8

 | JUMP lblId
 | JUMPI lblId
 | JUMPDEST lblId

 | CALLDEST funId (Args Word16)
 | EXTCALLDEST funId (Args Word16)

 | FUNCTION String
 | CONTRACT B.ByteString

 | LOG Word8

 | RETURN (Args Word16)

 | REVERT
 | INVALID
 | SELFDESTRUCT
  deriving (Show, Eq, Data)

retypeOpcode0 :: (Applicative f)
              => (fun -> f fun')
              -> (lbl -> f lbl')
              -> IeleOpcode0G fun lbl
              -> f (IeleOpcode0G fun' lbl')
retypeOpcode0 fun lbl i = case i of
   MSTOREN -> pure MSTOREN
   MSTORE -> pure MSTORE
   SSTORE -> pure SSTORE

   REGISTERS r -> pure (REGISTERS r)

   JUMP l -> JUMP <$> lbl l
   JUMPI l -> JUMPI <$> lbl l
   JUMPDEST l -> JUMPDEST <$> lbl l

   CALLDEST f arity -> CALLDEST <$> fun f <*> pure arity
   EXTCALLDEST f arity -> EXTCALLDEST <$> fun f <*> pure arity

   FUNCTION f -> pure (FUNCTION f)
   CONTRACT c -> pure (CONTRACT c)

   LOG arity -> pure (LOG arity)

   RETURN arity -> pure (RETURN arity)

   REVERT -> pure (REVERT)
   INVALID -> pure (INVALID)
   SELFDESTRUCT -> pure (SELFDESTRUCT)

data IeleOpcodeLi =
   LOADPOS
 | LOADNEG
  deriving (Show, Eq, Data)

data IeleOpcodeCall contractId funId =
   CALL funId (Args Word16) (Rets Word16)
 | CALLDYN (Args Word16) (Rets Word16)
 | STATICCALL funId (Args Word16) (Rets Word16)
 | STATICCALLDYN (Args Word16) (Rets Word16)
 | LOCALCALL funId (Args Word16) (Rets Word16)
 | LOCALCALLDYN (Args Word16) (Rets Word16)
 | CALLADDRESS funId

 | CREATE contractId (Args Word16) -- contract Id
 | COPYCREATE (Args Word16)
  deriving (Show, Eq, Data)

retypeOpcodeCall :: (Applicative f)
                 => (contractId -> f contractId')
                 -> (funId -> f funId')
                 -> IeleOpcodeCall contractId funId
                 -> f (IeleOpcodeCall contractId' funId')
retypeOpcodeCall contract fun i = case i of
    CALL f nargs nrets -> (\f -> CALL f nargs nrets) <$> fun f
    CALLDYN nargs nrets -> pure (CALLDYN nargs nrets)
    STATICCALL f nargs nrets -> (\f -> STATICCALL f nargs nrets) <$> fun f
    STATICCALLDYN nargs nrets -> pure (STATICCALLDYN nargs nrets)
    LOCALCALL f nargs nrets -> (\f -> LOCALCALL f nargs nrets) <$> fun f
    LOCALCALLDYN nargs nrets -> pure (LOCALCALLDYN nargs nrets)
    CALLADDRESS f -> CALLADDRESS <$> fun f
    CREATE c nargs -> (\c -> CREATE c nargs) <$> contract c
    COPYCREATE nargs -> pure (COPYCREATE nargs)

type IeleOp = IeleOpG Word16 Word16 Word16 Int Int
data IeleOpG contractId funId lblId regId argId =
   Nop
 | Op IeleOpcode1 regId [argId]
 | VoidOp (IeleOpcode0G funId lblId) [argId]
 | CallOp (IeleOpcodeCall contractId funId) [regId] [argId]
 | LiOp IeleOpcodeLi regId Integer
  deriving (Show, Eq, Data)

{-# INLINE retypeIeleOp #-}
retypeIeleOp :: (Applicative f)
             => (con -> f con')
             -> (fun -> f fun')
             -> (lbl -> f lbl')
             -> (reg -> f reg')
             -> (arg -> f arg')
             -> IeleOpG con fun lbl reg arg
             -> f (IeleOpG con' fun' lbl' reg' arg')
retypeIeleOp con fun lbl reg arg inst = case inst of
  Nop -> pure Nop
  Op op1 ret args -> Op op1 <$> reg ret <*> traverse arg args
  VoidOp op0 args -> VoidOp <$> retypeOpcode0 fun lbl op0 <*> traverse arg args
  CallOp opCall rets args -> CallOp <$> retypeOpcodeCall con fun opCall
                                    <*> traverse reg rets <*> traverse arg args
  LiOp opLi ret imm -> (\r -> LiOp opLi r imm) <$> reg ret

ieleOpContract :: Traversal
  (IeleOpG con fun lbl reg arg)
  (IeleOpG con' fun lbl reg arg)
  con con'
ieleOpContract con inst = retypeIeleOp con pure pure pure pure inst

ieleOpFunId :: Traversal
  (IeleOpG con fun lbl reg arg)
  (IeleOpG con fun' lbl reg arg)
  fun fun'
ieleOpFunId fun inst = retypeIeleOp pure fun pure pure pure inst

ieleOpJumpDest :: Traversal
  (IeleOpG con fun lbl reg arg)
  (IeleOpG con fun lbl' reg arg)
  lbl lbl'
ieleOpJumpDest lbl inst = retypeIeleOp pure pure lbl pure pure inst

ieleOpReg :: Traversal
  (IeleOpG con fun lbl reg arg)
  (IeleOpG con fun lbl reg' arg)
  reg reg'
ieleOpReg reg inst = retypeIeleOp pure pure pure reg pure inst

ieleOpArg :: Traversal
  (IeleOpG con fun lbl reg arg)
  (IeleOpG con fun lbl reg arg')
  arg arg'
ieleOpArg arg inst = retypeIeleOp pure pure pure pure arg inst

ieleOpRegs' :: Traversal
  (IeleOpG con fun lbl reg reg)
  (IeleOpG con fun lbl reg' reg')
  reg reg'
ieleOpRegs' reg inst = retypeIeleOp pure pure pure reg reg inst

loadImm :: reg -> Integer -> IeleOpG a b c reg d
loadImm reg imm
 | imm >= 0 = LiOp LOADPOS reg imm
 | otherwise = LiOp LOADNEG reg (-imm)
