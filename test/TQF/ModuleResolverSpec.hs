module TQF.ModuleResolverSpec
  ( spec
  ) where

import           Helpers
import           TQF.AST
import           TQF.ModuleResolver
import           TQF.Type
import           Test.Hspec

addModuleSpec = do
  it "Should add module declarations for a qualified import" $ shouldResolve
    (ImportStatement [u "Test"] True Nothing)
    (Module [u "Test"] [] [decl1, decl2])
    initialNamespace
    (buildNamespace $ NamespaceLiteral
      []
      [("Test", Left $ NamespaceLiteral [("func1", [decl1]), ("func2", [decl2])] [])]
    )
  it "Should handle nested imports" $ shouldResolve
    (ImportStatement [u "Nested", u "Test"] True Nothing)
    (Module [u "Nested", u "Test"] [] [decl1, decl2])
    initialNamespace
    (buildNamespace $ NamespaceLiteral
      []
      [ ( "Nested"
        , Left $ NamespaceLiteral
          []
          [("Test", Left $ NamespaceLiteral [("func1", [decl1]), ("func2", [decl2])] [])]
        )
      ]
    )
  it "Should add module declarations to the root buildNamespace as well if unqualified"
    $ shouldResolve
        (ImportStatement [u "Test"] False Nothing)
        (Module [u "Test"] [] [decl1, decl2])
        initialNamespace
        (buildNamespace $ NamespaceLiteral
          [("func1", [decl1]), ("func2", [decl2])]
          [("Test", Left $ NamespaceLiteral [("func1", [decl1]), ("func2", [decl2])] [])]
        )
  it "Should be able to handle multiple nested declarations (Module and Module.Nested)" $ do
      -- Module - func1, func2. Module.Nested - func3



    shouldResolve
      (ImportStatement [u "Module"] True Nothing)
      (Module [u "Module"] [] [decl1, decl2])
      (buildNamespace $ NamespaceLiteral
        []
        [ ( "Module"
          , Left $ NamespaceLiteral [] [("Nested", Left $ NamespaceLiteral [("func3", [decl3])] [])]
          )
        ]
      )
      (buildNamespace $ NamespaceLiteral
        []
        [ ( "Module"
          , Left $ NamespaceLiteral [("func1", [decl1]), ("func2", [decl2])]
                                    [("Nested", Left $ NamespaceLiteral [("func3", [decl3])] [])]
          )
        ]
      )
    shouldResolve
      (ImportStatement [u "Module", u "Nested"] True Nothing)
      (Module [u "Module", u "Nested"] [] [decl3])
      (buildNamespace $ NamespaceLiteral
        []
        [("Module", Left $ NamespaceLiteral [("func1", [decl1]), ("func2", [decl2])] [])]
      )
      (buildNamespace $ NamespaceLiteral
        []
        [ ( "Module"
          , Left $ NamespaceLiteral [("func1", [decl1]), ("func2", [decl2])]
                                    [("Nested", Left $ NamespaceLiteral [("func3", [decl3])] [])]
          )
        ]
      )
  it "Should be able to handle renaming imports" $ shouldResolve
    (ImportStatement [u "Module"] True (Just [u "Renamed"]))
    (Module [u "Module"] [] [decl1, decl2])
    initialNamespace
    (buildNamespace $ NamespaceLiteral
      []
      [("Renamed", Left $ NamespaceLiteral [("func1", [decl1]), ("func2", [decl2])] [])]
    )
  it "Should merge two lidents under the same namespce" $ shouldResolve
    (ImportStatement [u "Test"] True Nothing)
    (Module [u "Test"] [] [decl1])
    ( buildNamespace
    $ NamespaceLiteral [] [("Test", Left $ NamespaceLiteral [("func1", [decl1alt])] [])]
    )
    ( buildNamespace
    $ NamespaceLiteral [] [("Test", Left $ NamespaceLiteral [("func1", [decl1, decl1alt])] [])]
    )
  it "Should error when a type and buildNamespace clash" $ shouldError
    (ImportStatement [u "Test"] True Nothing)
    (Module [u "Test"] [] [decl1, decl2])
    (buildNamespace $ NamespaceLiteral [] [("Test", Right $ u "TypeName")])
    (NamespaceTypeClash $ UIdent [] (u "Test"))
 where

  decl1 = FunctionDecl (l "func1") (simpleType Nil) [] (CodeBlock [])
  decl1alt =
    FunctionDecl (l "func1") (simpleType Nil) [(simpleType Number, l "arg")] (CodeBlock [])
  decl2 = FunctionDecl (l "func2") (simpleType String) [] (CodeBlock [])
  decl3 =
    FunctionDecl (l "func3") (simpleType String) [(simpleType Number, l "arg")] (CodeBlock [])


  shouldResolve statement ast input expectedOutput =
    runNamespaceTransformation (addModule statement ast) input `shouldBe` Right expectedOutput
  shouldError statement ast input expectedError =
    runNamespaceTransformation (addModule statement ast) input `shouldBe` Left expectedError

spec = do
  describe "addModule" addModuleSpec
