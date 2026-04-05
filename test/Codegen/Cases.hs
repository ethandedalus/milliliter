module Codegen.Cases (cases) where

import Codegen.Types
  ( CodegenError,
    Func (..),
    Instruction (..),
    Operand (..),
    Program (..),
    Register (..),
    UnaryOperator (..),
  )
import qualified Compiler.Types as CT (Literal (..))
import qualified IR.Types as IR
  ( Func (..),
    Instruction (..),
    Program (..),
    UnaryOperator (..),
    Val (..),
  )

type Case = (String, IR.Program, Either CodegenError Program)

simpleProgram :: Case
simpleProgram =
  ( "simple program",
    IR.Program $
      IR.Func
        "main"
        [ IR.Unary IR.Negate (IR.Lit $ CT.LiteralInt 42) (IR.Var "tmp.0"),
          IR.Unary IR.Complement (IR.Var "tmp.0") (IR.Var "tmp.1"),
          IR.Unary IR.Negate (IR.Var "tmp.1") (IR.Var "tmp.2"),
          IR.Return (IR.Var "tmp.2")
        ],
    Right $
      Program
        ( Func
            "main"
            [ Mov (Imm 42) (Stack (-4)),
              Unary Negate (Stack (-4)),
              Mov (Stack (-4)) (Register R10),
              Mov (Register R10) (Stack (-8)),
              Unary Complement (Stack (-8)),
              Mov (Stack (-8)) (Register R10),
              Mov (Register R10) (Stack (-12)),
              Unary Negate (Stack (-12)),
              Mov (Stack (-12)) (Register AX),
              Ret
            ]
            12
        )
  )

cases :: [Case]
cases = [simpleProgram]
