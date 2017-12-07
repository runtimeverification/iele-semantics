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
  ,IeleOpcodeLi(..)
  ,IeleOpcodeCall(..)
  ,IeleOp
  ,IeleOpG(..)
  )
where
import Data.Data

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

 | NE
 | LT
 | GT
 | LE
 | GE
 | EQ

 | ISZERO
 | AND
 | OR
 | XOR
 | NOT

 | BYTE
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

 | RETURN (Rets Word16)
 | REVERT (Rets Word16)

 | INVALID
 | SELFDESTRUCT
  deriving (Show, Eq, Data)

instance Bifunctor IeleOpcode0G where
  bimap fun lbl i = case i of
   MSTOREN -> MSTOREN
   MSTORE -> MSTORE
   SSTORE -> SSTORE

   REGISTERS r -> REGISTERS r

   JUMP l -> JUMP (lbl l)
   JUMPI l -> JUMPI (lbl l)
   JUMPDEST l -> JUMPDEST (lbl l)

   CALLDEST f arity -> CALLDEST (fun f) arity
   EXTCALLDEST f arity -> EXTCALLDEST (fun f) arity

   FUNCTION f -> FUNCTION f
   CONTRACT c -> CONTRACT c

   LOG arity -> LOG arity

   RETURN arity -> RETURN arity
   REVERT arity -> REVERT arity

   INVALID -> INVALID
   SELFDESTRUCT -> SELFDESTRUCT

data IeleOpcodeLi =
   LOADPOS
 | LOADNEG
  deriving (Show, Eq, Data)

data IeleOpcodeCall contractId funId =
   CALL funId (Args Word16) (Rets  Word16)
 | STATICCALL funId (Args Word16) (Rets Word16)
 | LOCALCALL funId (Args Word16) (Rets Word16)

 | CREATE contractId (Args Word16) -- contract Id
 | COPYCREATE (Args Word16)
  deriving (Show, Eq, Data, Functor, Foldable, Traversable)
instance Bifunctor IeleOpcodeCall where
  bimap contract fun i = case i of
    CALL f nargs nrets -> CALL (fun f) nargs nrets
    STATICCALL f nargs nrets -> STATICCALL (fun f) nargs nrets
    LOCALCALL f nargs nrets -> LOCALCALL (fun f) nargs nrets
    CREATE c nargs -> CREATE (contract c) nargs
    COPYCREATE nargs -> COPYCREATE nargs

type IeleOp = IeleOpG Word16 Word16 Word16 Int
data IeleOpG contractId funId lblId regId =
   Nop
 | Op IeleOpcode1 regId [regId]
 | VoidOp (IeleOpcode0G funId lblId) [regId]
 | CallOp (IeleOpcodeCall contractId funId) [regId] [regId]
 | LiOp IeleOpcodeLi regId Integer
  deriving (Show, Eq, Data, Functor, Foldable, Traversable)
