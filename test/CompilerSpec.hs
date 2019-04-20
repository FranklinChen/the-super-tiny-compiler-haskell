module CompilerSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)

import Compiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "complete example" $ do
    it "parser" $ do
      parser input `shouldParse` ast
    it "transformer" $ do
      transformer ast `shouldBe` newAst
    it "codeGenerator" $ do
      codeGenerator newAst `shouldBe` output
    it "compiler" $ do
      compiler input `shouldParse` output

input :: String
input = "(add 2 (subtract 4 2))"

ast :: Ast
ast = Program
    [ CallExpression "add"
      [ NumberLiteral 2
      , CallExpression "subtract"
        [ NumberLiteral 4
        , NumberLiteral 2
        ]
      ]
    ]

newAst :: NewAst
newAst = NewProgram
    [ NewExpressionStatement
      ( NewCallExpression (NewIdentifier "add")
        [ NewNumberLiteral 2
        , NewCallExpression (NewIdentifier "subtract")
          [ NewNumberLiteral 4
          , NewNumberLiteral 2
          ]
        ]
      )
    ]

output :: String
output = "add(2, subtract(4, 2));"
