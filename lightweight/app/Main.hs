module Main where
import BST
import Test.QuickCheck
import Test.QuickCheck.Property (Prop)

main :: IO ()
main = quickCheckWith stdArgs {maxSuccess = 500000} prop_UnionUnionAssoc


