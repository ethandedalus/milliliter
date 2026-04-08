module Parser.ParseStmtSpec where

import qualified CompileError as CE (CompileError (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)
import Lexer (runLexer)
import Lexer.Types (mkSourceLoc)
import Parser.Combinators (parseStmt)
import Parser.Types (BinaryOperator (..), Expr (..), Factor (..), Stmt (..), UnaryOperator (..))
import Shared.UnitTest (UnitTest (UnitTest))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Stmt

simpleReturnStmt :: Test
simpleReturnStmt =
  UnitTest "simple return stmt" "return 0;" $ Right $ Return (Factor (Lit (LiteralInt 0)))

parenthesizedReturnExpr :: Test
parenthesizedReturnExpr =
  UnitTest "parenthesized return expr" "return ~(-(42));" $
    Right $
      Return (Factor $ Unary Complement (Expr (Factor $ Unary Negate (Expr (Factor (Lit (LiteralInt 42)))))))

binaryReturnExpr :: Test
binaryReturnExpr =
  UnitTest "binary return expr" "return 1 + 1;" $
    Right $
      Return (Binary Add (Factor (Lit (LiteralInt 1))) (Factor (Lit (LiteralInt 1))))

spec :: Spec
spec = do
  describe "parse statements" $ do
    let parseStmtTestCases = [simpleReturnStmt, parenthesizedReturnExpr, binaryReturnExpr]

    forM_ parseStmtTestCases $ \(UnitTest caseName input expectedResult) -> it ("case: " ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        first CE.ParseError $ evalStateT parseStmt tokens
