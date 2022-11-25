module HexellTest where

    import Test.HUnit
    import Hexell

    assertAllTrue :: [Bool] -> Bool
    assertAllTrue = foldr (\existing next -> next && existing) True

    sanityA = TestCase (assertEqual  "Simple Print Test" "Just X0X1X2X3X4X5X6X7X8X9XAXBXCXDXEXF" (show $ makeSha256Digest ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']))

    hexDigitOrdTest = TestCase (assertBool "Do Ords Work" (assertAllTrue $ [True, False]))


    tests = TestList [sanityA, hexDigitOrdTest]