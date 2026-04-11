module Compiler.Codegen.Instructions where

import Compiler.Codegen.Types (Emitter)
import Control.Monad (replicateM_, unless)
import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.List (intersperse)
import Data.Text.Lazy.Builder

spc :: Emitter ()
spc = tell $ fromString " "

nl :: Emitter ()
nl = tell $ fromString "\n"

comma :: Emitter ()
comma = tell $ fromString ","

asm :: Emitter () -> [Emitter ()] -> Emitter ()
asm m args = do
  depth <- ask
  replicateM_ depth spc
  m
  unless (null args) $ spc >> sequence_ (intersperse (comma >> spc) args)
  nl

globl :: Emitter ()
globl = tell $ fromString ".globl"

underscore :: Emitter ()
underscore = tell $ fromString "_"

colon :: Emitter ()
colon = tell $ fromString ":"

-- Registers
eax :: Emitter ()
eax = tell $ fromString "%eax"

ecx :: Emitter ()
ecx = tell $ fromString "%ecx"

edx :: Emitter ()
edx = tell $ fromString "%edx"

ebx :: Emitter ()
ebx = tell $ fromString "%ebx"

rsp :: Emitter ()
rsp = tell $ fromString "%rsp"

rbp :: Emitter ()
rbp = tell $ fromString "%rbp"

r8d :: Emitter ()
r8d = tell $ fromString "%r8d"

r9d :: Emitter ()
r9d = tell $ fromString "%r9d"

r10d :: Emitter ()
r10d = tell $ fromString "%r10d"

r11d :: Emitter ()
r11d = tell $ fromString "%r11d"

r12d :: Emitter ()
r12d = tell $ fromString "%r12d"

r13d :: Emitter ()
r13d = tell $ fromString "%r13d"

r14d :: Emitter ()
r14d = tell $ fromString "%r14d"

cl :: Emitter ()
cl = tell $ fromString "%cl"

-- mov

movl :: Emitter ()
movl = tell $ fromString "movl"

movq :: Emitter ()
movq = tell $ fromString "movq"

-- arithmetic
cdq :: Emitter ()
cdq = tell $ fromString "cdq"

idivl :: Emitter ()
idivl = tell $ fromString "idivl"

addl :: Emitter ()
addl = tell $ fromString "addl"

subl :: Emitter ()
subl = tell $ fromString "subl"

imull :: Emitter ()
imull = tell $ fromString "imull"

-- bitwise

shll :: Emitter ()
shll = tell $ fromString "shll"

shrl :: Emitter ()
shrl = tell $ fromString "shrl"

sarl :: Emitter ()
sarl = tell $ fromString "sarl"

andl :: Emitter ()
andl = tell $ fromString "andl"

orl :: Emitter ()
orl = tell $ fromString "orl"

xorl :: Emitter ()
xorl = tell $ fromString "xorl"

-- stack

pushq :: Emitter ()
pushq = tell $ fromString "pushq"

popq :: Emitter ()
popq = tell $ fromString "popq"

subq :: Emitter ()
subq = tell $ fromString "subq"

-- ret

ret :: Emitter ()
ret = tell $ fromString "ret"
