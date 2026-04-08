module IR.LowerStmtSpec where

import qualified CompileError as CE (CompileError (..))
import Compiler.Types (Literal (LiteralInt))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)
import IR.Emit (lowerStmt)
import IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import Lexer (runLexer)
import Lexer.Types (mkSourceLoc)
import Parser.Combinators (parseStmt)
import Shared.UnitTest (UnitTest (UnitTest))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest [Instruction]

simpleReturn :: Test
simpleReturn =
  UnitTest "simple return" "return 42;" $ Right [Return (Lit (LiteralInt 42))]

returnNestedUnaryExpr :: Test
returnNestedUnaryExpr =
  UnitTest "return nested unary expr" "return -~-42;" $
    Right
      [ Unary Negate (Lit $ LiteralInt 42) (Var "tmp.0")
      , Unary Complement (Var "tmp.0") (Var "tmp.1")
      , Unary Negate (Var "tmp.1") (Var "tmp.2")
      , Return (Var "tmp.2")
      ]

spec :: Spec
spec = do
  describe "lower return statements" $ do
    let lowerStmtTestCases = [simpleReturn, returnNestedUnaryExpr]

    forM_ lowerStmtTestCases $ \(UnitTest caseName input expectedResult) -> it ("case: " ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        stmt <- first CE.ParseError $ evalStateT parseStmt tokens
        first CE.IRError $ evalStateT (lowerStmt stmt) 0
