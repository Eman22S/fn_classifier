import Data.List

main :: IO ()
main = do
  let dataset = ["Hello world", "Is anybody here?", "I hate everyone!", "She is not my friend","I will come tmrw" ]
  --let tmp = f vocabs
  print $ vocabs
  print $ createddictionary f vocabs

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
--This accepts a list of e.g ["Hello", "world"]
-- with context 2
makewordpairs :: [String] -> [(String,String)]
makewordpairs [] = []
makewordpairs (x:y:z:h:xs) = (x,y):(x,z):(y,x):(y,z):(y,h):(z,x):(z,y):(z,h):(h,x):(h,y):(h,z):makewordpairs xs
makewordpairs (x:y:z:xs) = (x,y):(x,z):(y,x):(y,z):(z,x):(z,y):makewordpairs xs
makewordpairs (x:y:xs) =  (x,y):makewordpairs xs


vocabs = ["Hello", "world", "Is", "anybody", "here", "I", "hate", "everyone", "She", "is", "I", "will", "come", "not", "my", "friend", "Hello", "I", "hate"]
--This function removes repeatitive words in a ["the", "Hello"..]
f :: [String] -> [String]
f [] = []
f (x:xs) 
	| x `elem` xs = f xs
	| otherwise = x:f xs

--unique = f vocabs
--sorted = sort unique
--dict = zip  [1..length unique]  sorted
createddictionary::([String] -> [String]) -> [String] -> [(Int, String)]
createddictionary f [] = []
createddictionary f vocabs = do
			let tmp = f vocabs
			let sorted = sort tmp
			zip [1..length tmp] sorted


