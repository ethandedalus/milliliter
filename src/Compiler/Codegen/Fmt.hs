{-# LANGUAGE LambdaCase #-}

module Compiler.Codegen.Fmt (prologue, epilogue, fname, fstart, fdecl, indented, instruction, instructions) where

import Compiler.Codegen.Instructions
import Compiler.Codegen.Types (BinaryOperator (..), CodegenError (..), Emitter, Instruction (..), Operand (..), Register (..), UnaryOperator (..))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Control.Monad.Writer (tell)
import Data.Text.Lazy.Builder (fromString)

indented :: Emitter a -> Emitter a
indented = local (+ 2)

parenthesize_ :: Emitter () -> Emitter ()
parenthesize_ e = tell (fromString "(") >> e >> tell (fromString ")")

constant :: Int -> Emitter ()
constant v = tell $ fromString $ '$' : show v

offset :: Int -> Emitter ()
offset x = tell $ fromString (show x)

prologue :: Emitter ()
prologue = do
  asm pushq [rbp]
  asm movq [rsp, rbp]

epilogue :: Emitter ()
epilogue = do
  asm movq [rbp, rsp]
  asm popq [rbp]
  asm ret []

op :: UnaryOperator -> Emitter ()
op Complement = tell $ fromString "notl"
op Negate = tell $ fromString "negl"
op Not = tell $ fromString "notl"

binop :: BinaryOperator -> Emitter ()
binop Add = addl
binop Sub = subl
binop Mul = imull
binop LeftShift = shll
binop RightShift = sarl
binop BitAnd = andl
binop BitOr = orl
binop Xor = xorl

-- binop And = and

operand :: Operand -> Emitter ()
operand = \case
  (Imm v) -> constant v
  (Register AX) -> eax
  (Register DX) -> edx
  (Register R10) -> r10d
  (Register R11) -> r11d
  (Register R12) -> r12d
  (Register R13) -> r13d
  (Register R14) -> r14d
  (Register CX) -> ecx
  (Stack x) -> offset x >> parenthesize_ rbp
  pseudo@(Pseudo _) -> throwError (IllegalOperand pseudo "ICE: all psuedoregisters should be removed before code emission")

instruction :: Instruction -> Emitter ()
instruction = \case
  illegal@(Mov (Stack _) (Stack _)) -> throwError (IllegalInstruction illegal "ICE: mov instruction between two stack addresses is not allowed")
  (Mov src dst) -> asm movl [operand src, operand dst]
  (Unary op' operand') -> asm (op op') [operand operand']
  (Binary LeftShift (Register CX) rhs) -> asm shll [cl, operand rhs]
  (Binary RightShift (Register CX) rhs) -> asm sarl [cl, operand rhs]
  (Binary op' lhs rhs) -> asm (binop op') [operand lhs, operand rhs]
  (StackAlloc bytes) -> asm subq [constant bytes, rsp]
  CDQ -> asm cdq []
  (IDiv operand') -> asm idivl [operand operand']
  Ret -> epilogue
  (Cmp lhs rhs) -> asm cmpl [operand lhs, operand rhs]
  (Jmp l) -> asm jmp [label l]
  (Label l) -> asm (label l >> colon) []
  (JmpCC code l) -> asm (j code) [label l]
  (SetCC code (Register AX)) -> asm (set code) [al]
  (SetCC code (Register DX)) -> asm (set code) [dl]
  (SetCC code (Register R10)) -> asm (set code) [r10b]
  (SetCC code (Register R11)) -> asm (set code) [r11b]
  (SetCC code rhs) -> asm (set code) [operand rhs]

instructions :: [Instruction] -> Emitter ()
instructions = mapM_ instruction

fname :: String -> Emitter ()
fname name = underscore >> tell (fromString name)

fdecl :: String -> Emitter ()
fdecl name = indented $ do
  globl >> spc >> fname name >> nl

fstart :: String -> Emitter ()
fstart name = fname name >> colon >> nl
