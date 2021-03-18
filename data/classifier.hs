{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
	let dataset = ["Hello world", "Is anybody here?", "I hate everyone!", "She is not my friend","I will come tmrw" ]
	print(removestopwords dataset)

stopwords = ["the","a", "and", "is", "be", "will","Is"]
nums :: T.Text
nums = "012345678910!@#$%^&*()_-+=?"
punctuations = ["!","?", ":", ";" ,"(", ")", "-", "_"  ]
nonstopwords :: T.Text -> Bool
nonstopwords text = text `notElem` stopwords 


removestopwords :: [T.Text]  -> [T.Text]
removestopwords [] = []
removestopwords (x:xs) = 
	let tmp = T.words x
	    y = filter nonstopwords tmp 
	    wrap = T.unwords y 
	    h = T.filter (notElem  "aeiou") wrap 
	in  h :removestopwords xs

