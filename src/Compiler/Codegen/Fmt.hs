module Compiler.Codegen.Fmt (prologue, epilogue, fname, fstart, fdecl, indented, instruction, instructions) where

import Compiler.Codegen.Instructions (
  addl,
  andl,
  asm,
  cdq,
  cl,
  colon,
  eax,
  ecx,
  edx,
  globl,
  idivl,
  imull,
  movl,
  movq,
  nl,
  orl,
  popq,
  pushq,
  r10d,
  r11d,
  r12d,
  r13d,
  r14d,
  rbp,
  ret,
  rsp,
  sarl,
  shll,
  spc,
  subl,
  subq,
  underscore,
  xorl,
 )
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

binop :: BinaryOperator -> Emitter ()
binop Add = addl
binop Sub = subl
binop Mul = imull
binop LeftShift = shll
binop RightShift = sarl
binop And = andl
binop Or = orl
binop Xor = xorl

-- binop And = and

operand :: Operand -> Emitter ()
operand operand' = do
  case operand' of
    (Imm v) -> constant v
    (Register AX) -> eax
    (Register DX) -> edx
    (Register R10) -> r10d
    (Register R11) -> r11d
    (Register R12) -> r12d
    (Register R13) -> r13d
    (Register R14) -> r14d
    (Register CX) -> ecx
    (Register CL) -> cl
    (Stack x) -> offset x >> parenthesize_ rbp
    pseudo@(Pseudo _) -> throwError (IllegalOperand pseudo "ICE: all psuedoregisters should be removed before code emission")

instruction :: Instruction -> Emitter ()
instruction ins' = do
  case ins' of
    illegal@(Mov (Stack _) (Stack _)) -> throwError (IllegalInstruction illegal "ICE: mov instruction between two stack addresses is not allowed")
    (Mov src dst) -> asm movl [operand src, operand dst]
    (Unary op' operand') -> asm (op op') [operand operand']
    (Binary op' lhs rhs) -> asm (binop op') [operand lhs, operand rhs]
    (StackAlloc bytes) -> asm subq [constant bytes, rsp]
    CDQ -> asm cdq []
    (IDiv operand') -> asm idivl [operand operand']
    Ret -> epilogue

instructions :: [Instruction] -> Emitter ()
instructions = mapM_ instruction

fname :: String -> Emitter ()
fname name = underscore >> tell (fromString name)

fdecl :: String -> Emitter ()
fdecl name = indented $ do
  globl >> spc >> fname name >> nl

fstart :: String -> Emitter ()
fstart name = fname name >> colon >> nl
