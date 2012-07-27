import Data.Maybe

main :: IO ()
main = do
     putStr "Enter Number for factoring: "
     s <- getLine
     let n = read s :: Integer
         f = shor n
     if(f == Nothing)
          then putStrLn (show n ++ " is prime.")
          else putStrLn ("factors of " ++ show n ++ " found: " ++ (show . fromJust $ f))
     return ()
     
shor :: Integer -> Maybe (Either Integer (Integer, Integer))
shor n = listToMaybe . mapMaybe (shor' n) $ [2..(n - 1)]

shor' :: Integer -> Integer -> Maybe (Either Integer (Integer, Integer))
shor' n y = if(gcd y n /= 1)
                           then Just (Left (gcd y n))
                           else if(order `mod` 2 /= 0)
                                       then Nothing
                                       else if(y^s `mod` n == (n - 1))
                                                        then Nothing
                                                        else Just (Right (gcd (y ^ s - 1) n, gcd (y ^ s + 1) n))
                        where order = (+2) . length . takeWhile (/= 1) . map (\x -> y^x `mod` n) $ [2..(n - 1)]
                              s = order `div` 2
                                                        
