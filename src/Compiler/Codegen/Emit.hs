module Compiler.Codegen.Emit (
  emit,
  r11d,
  rbp,
  r10d,
  eax,
  rsp,
  movl,
  comma,
  spc,
  nl,
  movq,
  popq,
  ret,
  subq,
  globl,
  colon,
  underscore,
  indent,
  pushq,
  fname,
  op,
  constant,
  operand,
  instruction,
  prologue,
  epilogue,
  func,
  parenthesize_,
  emitProgram,
)
where

import Compiler.Codegen.Types (
  BinaryOperator (..),
  CodegenError (..),
  Emitter,
  Func (..),
  Instruction (..),
  Operand (..),
  Program (..),
  Register (..),
  UnaryOperator (..),
 )
import qualified Compiler.Error as CE (CompileError (CodegenError))
import Compiler.Stage (Stage)
import Control.Monad (replicateM_, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, local, runReaderT)
import Control.Monad.Writer (execWriterT, tell)
import Data.Bifunctor (first)
import Data.List (intersperse)
import Data.Text.Lazy.Builder

rbp
  , cdq
  , r10d
  , r11d
  , eax
  , edx
  , rsp
  , movl
  , comma
  , spc
  , nl
  , movq
  , popq
  , ret
  , subq
  , globl
  , colon
  , underscore
  , indent
  , idivl
  , pushq
  , addl
  , subl
  , imull ::
    Emitter ()
spc = tell $ fromString " "
nl = tell $ fromString "\n"
comma = tell $ fromString ","
rbp = tell $ fromString "%rbp"
rsp = tell $ fromString "%rsp"
movl = tell $ fromString "movl"
movq = tell $ fromString "movq"
popq = tell $ fromString "popq"
ret = tell $ fromString "ret"
subq = tell $ fromString "subq"
globl = tell $ fromString ".globl"
underscore = tell $ fromString "_"
colon = tell $ fromString ":"
indent = replicateM_ 2 spc
pushq = tell $ fromString "pushq"
eax = tell $ fromString "%eax"
edx = tell $ fromString "%edx"
r10d = tell $ fromString "%r10d"
r11d = tell $ fromString "%r11d"
cdq = tell $ fromString "cdq"
idivl = tell $ fromString "idivl"
addl = tell $ fromString "addl"
subl = tell $ fromString "subl"
imull = tell $ fromString "imull"

asm :: Emitter () -> [Emitter ()] -> Emitter ()
asm m args = do
  depth <- ask
  replicateM_ depth spc
  m
  unless (null args) $ spc >> sequence_ (intersperse (comma >> spc) args)
  nl

indented :: Emitter a -> Emitter a
indented = local (+ 2)

fname :: String -> Emitter ()
fname name = underscore >> tell (fromString name)

op :: UnaryOperator -> Emitter ()
op Complement = tell $ fromString "notl"
op Negate = tell $ fromString "negl"

binop :: BinaryOperator -> Emitter ()
binop Add = addl
binop Sub = subl
binop Mul = imull

constant :: Int -> Emitter ()
constant v = tell $ fromString $ '$' : show v

parenthesize_ :: Emitter () -> Emitter ()
parenthesize_ e = tell (fromString "(") >> e >> tell (fromString ")")

epilogue :: Emitter ()
epilogue = do
  asm movq [rbp, rsp]
  asm popq [rbp]
  asm ret []

offset :: Int -> Emitter ()
offset x = tell $ fromString (show x)

operand :: Operand -> Emitter ()
operand operand' = do
  case operand' of
    (Imm v) -> constant v
    (Register AX) -> eax
    (Register DX) -> edx
    (Register R10) -> r10d
    (Register R11) -> r11d
    (Stack x) -> offset x >> parenthesize_ rbp
    other -> throwError (IllegalOperand other "ICE: all psuedoregisters should be removed before code emission")

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

prologue :: Emitter ()
prologue = do
  asm pushq [rbp]
  asm movq [rsp, rbp]

func :: Func -> Emitter ()
func (Func name instructions stackAlloc) = do
  indented $ globl >> spc >> fname name >> nl
  fname name >> colon >> nl
  indented $ do
    prologue
    instruction $ StackAlloc stackAlloc
    mapM_ instruction instructions

emitProgram :: Program -> Emitter ()
emitProgram (Program f) = func f

emit :: (i -> Emitter a) -> Stage i Builder
emit f x = first CE.CodegenError q
 where
  q = execWriterT (runReaderT (f x) 0)
