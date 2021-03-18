import Data.List

main :: IO ()
main = do
	let dataset = ["Hello world", "Is anybody here?", "I hate everyone!", "She is not my friend","I will come tmrw" ]
	print(removestopwords dataset)

stopwords = ["the","a", "and", "is", "be", "will","Is"]
nums :: String
nums = "01234567891)(*&^%$#@!-+_=?:;"


removestopwords :: [String]  -> [String]
removestopwords [] = []
removestopwords (x:xs) = 
	let tmp = words x
	    y = filter (`notElem` stopwords) tmp 
	    wrap = unwords y 
	    h = filter (`notElem`  nums) wrap 
	in  h :removestopwords xs

