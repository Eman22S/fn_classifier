import Data.List as L
import Data.Map as M
main :: IO ()
main = do
  let dataset = ["Hello world", "Is anybody here?", "I hate everyone!", "She is not my friend","I will come tmrw" ]
  let vocabs = ["Hello", "world", "Is", "anybody", "here", "I", "hate", "everyone", "She", "is", "I", "will", "come", "not", "my", "friend", "Hello", "I", "hate"] 
  let result = getindices vocabs
  print  $ result
  --print $ b

stopwords = ["the","a", "and", "is", "be", "will","Is"]
nums :: String
nums = "01234567891)(*&^%$#@!-+_=?:;"


removestopwords :: [String]  -> [String]
removestopwords [] = []
removestopwords (x:xs) = 
	let tmp = words x
	    y = L.filter (`notElem` stopwords) tmp 
	    wrap = unwords y 
	    h = L.filter (`notElem`  nums) wrap 
	in  h :removestopwords xs
--This accepts a list of e.g ["Hello", "world"]
-- with context 2
makewordpairs :: [String] -> [(String,String)]
makewordpairs [] = []
makewordpairs (x:y:z:h:xs) = (x,y):(x,z):(y,x):(y,z):(y,h):(z,x):(z,y):(z,h):(h,x):(h,y):(h,z):makewordpairs xs
makewordpairs (x:y:z:xs) = (x,y):(x,z):(y,x):(y,z):(z,x):(z,y):makewordpairs xs
makewordpairs (x:y:xs) =  (x,y):makewordpairs xs


--This function removes repeatitive words in a ["the", "Hello"..]
f :: [String] -> [String]
f [] = []
f (x:xs) 
	| x `elem` xs = f xs
	| otherwise = x:f xs

createddictionary::([String] -> [String]) -> [String] -> [(String, Int)]
createddictionary f vocabs = zip  (sort . f $ vocabs) [1..]

vocabs = ["Hello", "world", "Is", "anybody", "here", "I", "hate", "everyone", "She", "is", "I", "will", "come", "not", "my", "friend", "Hello", "I", "hate"] 
wordpairs = makewordpairs $ vocabs
xss = L.map  (\(a,b) -> a) wordpairs
yss = L.map  (\(a,b) -> b) wordpairs


dict = M.fromList (createddictionary  f  vocabs )
getindices:: [String] -> [Int]
getindices vocabs = L.foldr (\x acc   -> (index x) : acc ) [] vocabs
index :: String -> Int
index x=(\(Just i) -> i) $ M.lookup x  dict
results = getindices vocabs
zeros:: [Float]
zeros = L.replicate (length $ getindices vocabs) 0

onehotencoder ::Num q=> [Int] -> [q] ->[[q]]
onehotencoder xs zs = L.foldr (\x acc -> insertAt zs 1 x: acc) [] xs

insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 1  = elem : x : xs
    | pos > 1 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs) )
----(19,20)
----(_, 20)
ran :: [Float]
ran = L.replicate (length $ getindices vocabs) 0.06
matrix_gen :: Num j=> [j] -> [j] -> [[j]]
matrix_gen ws zs = L.foldr(\x acc -> zs:acc) [] ws
input = onehotencoder results zeros
--output = input * weight
b = 0.3
simple_dense::[Float] -> [Float] -> [Float]
simple_dense [] _ = []
simple_dense _ [] =[]
simple_dense [] [] = []
simple_dense  (x:xs) (y:ys) = x * y + b : simple_dense xs ys
