module Compiler.Test.Codegen.EmitSpec where

import qualified Compiler.Codegen.Emit as E
import Compiler.Codegen.Types
import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (execWriterT)
import Data.Text.Lazy
import Data.Text.Lazy.Builder
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "helpers" $ do
    let cases =
          [ ("eax", E.eax, Right "%eax")
          , ("rsp", E.rsp, Right "%rsp")
          , ("rbp", E.rbp, Right "%rbp")
          , ("ret", E.ret, Right "ret")
          , ("r10d", E.r10d, Right "%r10d")
          , ("movl", E.movl, Right "movl")
          , ("movq", E.movq, Right "movq")
          , ("pushq", E.pushq, Right "pushq")
          , ("popq", E.popq, Right "popq")
          , ("subq", E.subq, Right "subq")
          , ("globl", E.globl, Right ".globl")
          , ("spc", E.spc, Right " ")
          , ("nl", E.nl, Right "\n")
          , ("comma", E.comma, Right ",")
          , ("colon", E.colon, Right ":")
          , ("indent", E.indent, Right "  ")
          , ("underscore", E.underscore, Right "_")
          , ("fname", E.fname "main", Right "_main")
          , ("op (complement)", E.op Complement, Right "notl")
          , ("op (negate)", E.op Negate, Right "negl")
          , ("constant", E.constant 42, Right "$42")
          , ("parenthesize", E.parenthesize_ E.eax, Right "(%eax)")
          , ("prologue", E.prologue, Right "pushq %rbp\nmovq %rsp, %rbp\n")
          , ("epilogue", E.epilogue, Right "movq %rbp, %rsp\npopq %rbp\nret\n")
          ]

    forM_ cases $ \(name, helper, expected) ->
      it ("case: " ++ name) $ unpack . toLazyText <$> execWriterT (runReaderT helper 0) `shouldBe` expected

  describe "operand" $ do
    let cases =
          [ ("imm", Imm 42, Right "$42")
          , ("register (ax)", Register AX, Right "%eax")
          , ("register (r10)", Register R10, Right "%r10d")
          , ("stack", Stack (-4), Right "-4(%rbp)")
          , ("pseudo", Pseudo "x", Left (IllegalOperand (Pseudo "x") "ICE: all psuedoregisters should be removed before code emission"))
          ]

    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ unpack . toLazyText <$> execWriterT (runReaderT (E.operand input) 0) `shouldBe` expected

  describe "instruction" $ do
    let cases =
          [ ("mov (1)", Mov (Imm 42) (Register AX), Right "movl $42, %eax\n")
          , ("mov (2)", Mov (Stack (-8)) (Register R10), Right "movl -8(%rbp), %r10d\n")
          ,
            ( "illegal mov"
            , Mov (Stack (-8)) (Stack (-12))
            , Left (IllegalInstruction (Mov (Stack (-8)) (Stack (-12))) "ICE: mov instruction between two stack addresses is not allowed")
            )
          , ("unary (1)", Unary Complement (Stack (-12)), Right "notl -12(%rbp)\n")
          , ("unary (2)", Unary Negate (Stack (-12)), Right "negl -12(%rbp)\n")
          , ("stackalloc", StackAlloc 12, Right "subq $12, %rsp\n")
          ]

    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ unpack . toLazyText <$> execWriterT (runReaderT (E.instruction input) 0) `shouldBe` expected
