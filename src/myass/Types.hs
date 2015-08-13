module Types where

import Data.Word

newtype Reg = Reg { unReg :: Word8 } deriving Show

data Immed = Num Word16
           | Lbl String
           deriving Show

data Instr = Not
           | Tdf
           | Out
           | In
           | Add Reg
           | Sft Immed Reg
           | Jz  Immed
           | Imm Immed
           deriving Show
