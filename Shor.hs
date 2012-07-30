import Control.Monad.Loops
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
     args <- getArgs
     s <- if((not . null $ args) && (and . map isNumber . head $ args))
          then return . head $ args
          else  (if(not . null $ args)
                            then putStrLn ((show . head $ args) ++ "is not a valid argument.")
                            else return ()) >> iterateWhile (not . and . map isNumber) (putStr  "Enter Number for factoring: " >> hFlush stdout >> getLine
                                                                                                          >>= (\ x -> if(not . and . map isNumber $ x)
                                                                                                                                   then putStrLn ( show x ++ " is not an integer.") >> return x
                                                                                                                                   else return x))
     let n = read s :: Integer
         f = factor n
     putStrLn (show n ++ ": " ++ (unwords . map show $ f))
     
factor :: Integer -> [Integer]
factor x = case f of
                        (Just (f1, f2)) -> (factor f1) ++ (factor f2)
                        Nothing -> [x]
       where f = shor x

shor :: Integer -> Maybe (Integer, Integer)
shor n = listToMaybe . mapMaybe (shor' n) $ [2..(n - 1)]

shor' :: Integer -> Integer -> Maybe (Integer, Integer)
shor' n y = if(gcd y n /= 1)
                           then Just (gcd y n, n `div` gcd y n)
                           else if(order `mod` 2 /= 0)
                                       then Nothing
                                       else if(y^s `mod` n == (n - 1))
                                                        then Nothing
                                                        else Just (gcd (y ^ s - 1) n, gcd (y ^ s + 1) n)
                        where order = (+2) . length . takeWhile (/= 1) . map (\x -> y^x `mod` n) $ [2..(n - 1)]
                              s = order `div` 2
                                                        
