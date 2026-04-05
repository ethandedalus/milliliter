module Codegen.Emit
  ( emit,
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
  )
where

import Codegen.Types
  ( CodegenError (..),
    Emitter,
    Func (..),
    Instruction (..),
    Operand (..),
    Program (..),
    Register (..),
    UnaryOperator (..),
  )
import Control.Monad (replicateM_)
import Control.Monad.Except (throwError)
import Control.Monad.Writer (tell)
import Data.Text.Lazy.Builder (fromString)

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
  pushq ::
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
r10d = tell $ fromString "%r10d"

fname :: String -> Emitter ()
fname name = underscore >> tell (fromString name)

op :: UnaryOperator -> Emitter ()
op Complement = tell $ fromString "notl"
op Negate = tell $ fromString "negl"

constant :: Int -> Emitter ()
constant v = tell $ fromString $ '$' : show v

parenthesize_ :: Emitter () -> Emitter ()
parenthesize_ e = tell (fromString "(") >> e >> tell (fromString ")")

epilogue :: Emitter ()
epilogue = do
  movq >> spc >> rbp >> comma >> spc >> rsp >> nl
  popq >> spc >> rbp >> nl
  ret

offset :: Int -> Emitter ()
offset x = tell $ fromString (show x)

operand :: Operand -> Emitter ()
operand operand' = do
  case operand' of
    (Imm v) -> constant v
    (Register AX) -> eax
    (Register R10) -> r10d
    (Stack x) -> offset x >> parenthesize_ rbp
    other -> throwError (IllegalOperand other "ICE: all psuedoregisters should be removed before code emission")

instruction :: Instruction -> Emitter ()
instruction ins = do
  case ins of
    illegal@(Mov (Stack _) (Stack _)) -> throwError (IllegalInstruction illegal "ICE: mov instruction between two stack addresses is not allowed")
    (Mov src dst) ->
      movl >> spc >> operand src >> comma >> spc >> operand dst >> nl
    (Unary op' operand') ->
      op op' >> spc >> operand operand' >> nl
    (StackAlloc bytes) ->
      subq >> spc >> constant bytes >> comma >> spc >> rsp >> nl
    Ret -> epilogue >> nl

prologue :: Emitter ()
prologue = do
  pushq >> spc >> rbp >> nl
  movq >> spc >> rsp >> comma >> spc >> rbp >> nl

func :: Func -> Emitter ()
func (Func name instructions stackAlloc) = do
  indent >> globl >> spc >> fname name >> nl
  fname name >> colon >> nl
  prologue
  instruction $ StackAlloc stackAlloc
  mapM_ ((indent >>) . instruction) instructions

emit :: Program -> Emitter ()
emit (Program f) = func f
