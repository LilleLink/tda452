alphabet = ['A'..'Z']
chars = "EASYQUTION"

placeInAlphabet :: Char -> Int
placeInAlphabet ch = pia ch alphabet 1
    where
        pia ch [] count = count
        pia ch (c:cs) count | ch == c = count
                            | otherwise = pia ch cs (count+1)

hash :: Char -> Int
hash c = 11* placeInAlphabet c `mod` 16

main :: IO ()
main = do
    let hashes = zip chars (map hash chars)
    print hashes