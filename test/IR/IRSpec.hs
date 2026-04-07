module IR.IRSpec where

import qualified Compiler.Types as CT (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import IR.Emit (lowerFactor, lowerStmt)
import IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import qualified Parser.Types as PT (Expr (..), Factor (..), Stmt (..), UnaryOperator (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "lower expression" $ do
    let cases =
          [ ("constant expression", PT.Lit (CT.LiteralInt 42), Right ([], Lit (CT.LiteralInt 42)))
          ,
            ( "unary operation"
            , PT.Unary PT.Complement (PT.Lit (CT.LiteralInt 42))
            , Right ([Unary Complement (Lit $ CT.LiteralInt 42) (Var "tmp.0")], Var "tmp.0")
            )
          ]

    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ evalStateT (lowerFactor input) 0 `shouldBe` expected

  describe "lower statement" $ do
    let cases =
          [ ("return literal", PT.Return (PT.Factor $ PT.Lit $ CT.LiteralInt 42), Right [Return (Lit (CT.LiteralInt 42))])
          ,
            ( "return unary expression"
            , PT.Return (PT.Factor $ PT.Unary PT.Complement (PT.Lit (CT.LiteralInt 42)))
            , Right [Unary Complement (Lit $ CT.LiteralInt 42) (Var "tmp.0"), Return (Var "tmp.0")]
            )
          ,
            ( "return nested unary expression"
            , PT.Return
                ( PT.Factor
                    ( PT.Unary
                        PT.Negate
                        ( PT.Expr
                            ( PT.Factor
                                ( PT.Unary
                                    PT.Complement
                                    ( PT.Expr
                                        ( PT.Factor
                                            (PT.Unary PT.Negate (PT.Lit (CT.LiteralInt 42)))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            , Right
                [ Unary Negate (Lit $ CT.LiteralInt 42) (Var "tmp.0")
                , Unary Complement (Var "tmp.0") (Var "tmp.1")
                , Unary Negate (Var "tmp.1") (Var "tmp.2")
                , Return (Var "tmp.2")
                ]
            )
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ evalStateT (lowerStmt input) 0 `shouldBe` expected
