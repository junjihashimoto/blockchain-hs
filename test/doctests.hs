import Test.DocTest
main = doctest [
    "-isrc"
  , "src/Lib.hs"
  , "src/Sha256.hs"
  ]
