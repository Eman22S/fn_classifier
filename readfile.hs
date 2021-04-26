import System.IO  
import Data.Char

main = do  
    contents <- readFile "Fake.csv"
    let name = f contents
    putStr name

f :: String -> String
f contents = (map toLower contents)

