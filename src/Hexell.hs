module Hexell (SHA256, makeSha256Digest) where

-- HexDigit

data HexDigit = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | XA | XB | XC | XD | XE | XF | InvalidHexDigit deriving (Eq, Ord, Show)

toHexDigit :: Char -> Maybe HexDigit
toHexDigit '0' = Just X0
toHexDigit '1' = Just X1
toHexDigit '2' = Just X2
toHexDigit '3' = Just X3
toHexDigit '4' = Just X4
toHexDigit '5' = Just X5
toHexDigit '6' = Just X6
toHexDigit '7' = Just X7
toHexDigit '8' = Just X8
toHexDigit '9' = Just X9
toHexDigit 'a' = Just XA
toHexDigit 'b' = Just XB
toHexDigit 'c' = Just XC
toHexDigit 'd' = Just XD
toHexDigit 'e' = Just XE
toHexDigit 'f' = Just XF
toHexDigit _ = Nothing

toInt :: HexDigit -> Int
toInt X0 = 0
toInt X1 = 1
toInt X2 = 2
toInt X3 = 3
toInt X4 = 4
toInt X5 = 5
toInt X6 = 6
toInt X7 = 7
toInt X8 = 8
toInt X9 = 9
toInt XA = 10
toInt XB = 11
toInt XC = 12
toInt XD = 13
toInt XE = 14
toInt XF = 15
toInt InvalidHexDigit = 0

data SHA256 = SHA256Digest ([HexDigit])

instance Show SHA256 where
    show (SHA256Digest digits) = foldr (\new existing -> (show new) ++ existing) [] digits

validHex :: [Char] -> Bool
validHex [digit] = (toHexDigit digit /= Nothing)
validHex (d : ds) = (toHexDigit d /= Nothing) && validHex ds

makeSha256Digest :: [Char] -> Maybe SHA256
makeSha256Digest digits | validHex digits && length digits == 16 = Just $ SHA256Digest (map (\x -> maybe InvalidHexDigit id (toHexDigit x)) digits)
makeSha256Digest _ = Nothing