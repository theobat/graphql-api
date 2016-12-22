{-# LANGUAGE TypeApplications #-}

-- | Tests for query validation.
module ValidationTests (tests) where

import Protolude

import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.Validation
  ( ValidationError(..)
  , findDuplicates
  , getErrors
  )


tests :: IO TestTree
tests = testSpec "Validation" $ do
  describe "getErrors" $ do
    it "Treats simple queries as valid" $ do
      let doc = AST.Document
                [ AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node "me" [] []
                      [ AST.SelectionField (AST.Field "name" "name" [] [] [])
                      ]
                    )
                  )
                ]
      getErrors doc `shouldBe` []

    it "Detects duplicate operation names" $ do
      let doc = AST.Document
                [ AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node "me" [] []
                      [ AST.SelectionField (AST.Field "name" "name" [] [] [])
                      ]
                    )
                  )
                , AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node "me" [] []
                      [ AST.SelectionField (AST.Field "name" "name" [] [] [])
                      ]
                    )
                  )
                ]
      getErrors doc `shouldBe` [DuplicateOperation "me"]

  describe "findDuplicates" $ do
    prop "returns empty on unique lists" $ do
      \xs -> findDuplicates @Int (ordNub xs) === []
    prop "finds only duplicates" $ \xs -> do
      all (>1) (count xs <$> findDuplicates @Int xs)
    prop "finds all duplicates" $ \xs -> do
      (sort . findDuplicates @Int) xs === (ordNub . sort . filter ((> 1) . count xs)) xs


-- | Count the number of times 'x' occurs in 'xs'.
count :: Eq a => [a] -> a -> Int
count xs x = (length . filter (== x)) xs