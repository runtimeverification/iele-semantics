module IeleAssembler where
import Prelude hiding (LT,EQ,GT)
import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C(pack)
import Data.Serialize.Put

import IeleInstructions

putAsciiString :: Putter String
putAsciiString str = putByteString (C.pack str)

putArgs16 :: Putter (Args Word16)
putArgs16 nargs = putWord16be (argsCount nargs)

putRets16 :: Putter (Rets Word16)
putRets16 nrets = putWord16be (retsCount nrets)

asm_iele_opcode1 :: IeleOpcode1 -> Put
asm_iele_opcode1 op1 = case op1 of
  ADD -> putWord8 0x01
  MUL -> putWord8 0x02
  SUB -> putWord8 0x03
  DIV -> putWord8 0x04
  MOD -> putWord8 0x06
  EXP -> putWord8 0x07
  ADDMOD -> putWord8 0x08
  MULMOD -> putWord8 0x09
  EXPMOD -> putWord8 0x0a

  SIGNEXTEND -> putWord8 0x0b
  TWOS -> putWord8 0x0c
  BSWAP -> putWord8 0x0d

  NE -> putWord8 0x0f
  LT -> putWord8 0x10
  GT -> putWord8 0x11
  LE -> putWord8 0x12
  GE -> putWord8 0x13
  EQ -> putWord8 0x14

  ISZERO -> putWord8 0x15
  AND -> putWord8 0x16
  OR -> putWord8 0x17
  XOR -> putWord8 0x18
  NOT -> putWord8 0x19
  LOG2 -> putWord8 0x1c

  BYTE -> putWord8 0x1a
  SHIFT -> putWord8 0x1b
  SHA3 -> putWord8 0x20

  MLOADN -> putWord8 0x50
  MLOAD -> putWord8 0x51
  SLOAD -> putWord8 0x54

  MOVE -> putWord8 0x60

  IeleOpcodesQuery queryOp -> putWord8 $ case queryOp of
    ADDRESS -> 0x30
    BALANCE -> 0x31
    ORIGIN -> 0x32
    CALLER -> 0x33
    CALLVALUE -> 0x34
    CODESIZE -> 0x38
    GASPRICE -> 0x3a
    EXTCODESIZE -> 0x3b
    BLOCKHASH -> 0x40
    BENEFICIARY -> 0x41
    TIMESTAMP -> 0x42
    NUMBER -> 0x43
    DIFFICULTY -> 0x44
    GASLIMIT -> 0x45

    MSIZE -> 0x56
    GAS -> 0x57

asm_iele_opcode0 :: IeleOpcode0 -> Put
asm_iele_opcode0 op0 = case op0 of
  MSTOREN -> putWord8 0x52
  MSTORE -> putWord8 0x53
  SSTORE -> putWord8 0x55

  REGISTERS i -> putWord8 0x63 >> putWord8 i

  JUMP i -> putWord8 0x64 >> putWord16be i
  JUMPI i -> putWord8 0x65 >> putWord16be i

  JUMPDEST i -> putWord8 0x66 >> putWord16be i
  CALLDEST lbl nargs -> putWord8 0x67 >> putWord16be lbl >> putArgs16 nargs
  EXTCALLDEST lbl nargs -> putWord8 0x68 >> putWord16be lbl >> putArgs16 nargs
  FUNCTION(name) -> putWord8 0x69 >> putWord16be (fromIntegral (length name)) >> putAsciiString name
  CONTRACT(code) -> putWord8 0x6a >> putWord16be (fromIntegral (B.length code)) >> putByteString code


  LOG n | n <= 4 -> putWord8 (0xa0 + n)
        | otherwise -> error "LOG only takes up to 4 values"

  RETURN nargs -> putWord8 0xf6 >> putArgs16 nargs

  REVERT -> putWord8 0xf7
  INVALID -> putWord8 0xfe
  SELFDESTRUCT -> putWord8 0xff

asm_iele_opcode_li :: IeleOpcodeLi -> Put
asm_iele_opcode_li opLi = case opLi of
  LOADPOS -> putWord8 0x61
  LOADNEG -> putWord8 0x62

asm_iele_opcode_call :: IeleOpcodeCall Word16 Word16 -> Put
asm_iele_opcode_call opCall = case opCall of
  CALL call nargs nreturn -> putWord8 0xf2 >> putWord16be call >> putArgs16 nargs >> putRets16 nreturn
  CALLDYN nargs nreturn -> putWord8 0xf3 >> putArgs16 nargs >> putRets16 nreturn
  STATICCALL call nargs nreturn -> putWord8 0xf4 >> putWord16be call >> putArgs16 nargs >> putRets16 nreturn
  STATICCALLDYN nargs nreturn -> putWord8 0xf5 >> putArgs16 nargs >> putRets16 nreturn
  LOCALCALL call nargs nreturn -> putWord8 0xf8 >> putWord16be call >> putArgs16 nargs >> putRets16 nreturn
  LOCALCALLDYN nargs nreturn -> putWord8 0xf9 >> putArgs16 nargs >> putRets16 nreturn
  CALLADDRESS call -> putWord8 0xfa >> putWord16be call

  CREATE contract nargs -> putWord8 0xf0 >> putWord16be contract >> putArgs16 nargs
  COPYCREATE nargs -> putWord8 0xf1 >> putArgs16 nargs

-- Register numbers are packed bitwise
asm_iele_regs :: Word8 -> [Int] -> Put
asm_iele_regs nregs regs =
  let z = foldr (\reg accum -> accum `shiftL` fromIntegral nregs + fromIntegral reg) 0 regs in
  mapM_ putWord8 (be_bytes_width (length regs * fromIntegral nregs) z)

le_bytes :: Integer -> [Word8]
le_bytes 0 = []
le_bytes i = fromIntegral i : le_bytes (i `shiftR` 8)

be_bytes :: Integer -> [Word8]
be_bytes i = reverse (le_bytes i)

be_bytes_width :: Int -> Integer -> [Word8]
be_bytes_width bitWidth i = reverse (take byteWidth (le_bytes i ++ repeat 0))
  where byteWidth = (bitWidth + 7) `quot` 8

rlp_put_length :: Int -> Put
rlp_put_length len
  | len < 56 = putWord8 (0x80 + fromIntegral len)
  | otherwise = let len_bytes = be_bytes (fromIntegral len)
                    len_len = fromIntegral (length (len_bytes))
                in putWord8 (0x80 + 55 + len_len) >> mapM_ putWord8 len_bytes

rlp_put_bytes :: Putter [Word8]
rlp_put_bytes [c] | c < 128 = putWord8 c
rlp_put_bytes bytes = rlp_put_length (length bytes) >> mapM_ putWord8 bytes

asm_iele_op :: Word8 -> Putter IeleOp
asm_iele_op nregs op = case op of
  Nop -> pure ()
  Op opcode reg regs -> asm_iele_opcode1 opcode >> enc_regs (reg:regs)
  VoidOp opcode regs -> asm_iele_opcode0 opcode >> enc_regs regs
  CallOp opcode regs1 regs2 -> asm_iele_opcode_call opcode >> enc_regs (regs1++regs2)
  LiOp opcode r payload ->
    do asm_iele_opcode_li opcode; rlp_put_bytes (be_bytes payload); enc_regs [r]
 where enc_regs all_regs = asm_iele_regs nregs all_regs

asm_iele :: Putter [IeleOp]
asm_iele ops = mapM_ (asm_iele_op nregs) ops
  where nregs = case ops of (VoidOp (REGISTERS n) []:_) -> n; _ -> 5

assemble :: [IeleOp] -> B.ByteString
assemble ops = runPut (put_byte_length (B.length bytes) >> putByteString bytes)
  where bytes = runPut (asm_iele ops)
        put_byte_length len
          | len < 2^32 = putWord32be (fromIntegral len)
          | otherwise = error "Contract byte length does not fit in 4 bytes"

