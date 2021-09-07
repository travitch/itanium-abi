module Main ( main ) where

import qualified System.IO as IO
import System.Process
import Test.Framework ( defaultMain, testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )

import ABI.Itanium

mkTestCase :: (String, String) -> Test
mkTestCase (sym, expected) = testCase sym $ do
  case demangleName sym of
    Left err -> assertFailure (sym ++ " : " ++ err)
    Right dname ->
      assertEqual sym (normalize expected) (normalize (cxxNameToString dname))
  where
    normalize = filter (`notElem` "\n")

main :: IO ()
main = do
  inputs <- IO.readFile "tests/test-cases.txt"
  expecteds <- readProcess "c++filt" [] inputs
  let symbols = zip (lines inputs) (lines expecteds)
      tests = [ testGroup "QtGUI" (map mkTestCase symbols) ]
  defaultMain tests


