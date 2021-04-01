import Data.List 
import qualified Data.Map as M
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
	    y =  tmp \\ stopwords
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

createddictionary:: [String] -> [(String, Int)]
createddictionary vocabs = zip  (sort . nub $ vocabs) [1..]
vocabs::[String]
vocabs = ["Hello", "world", "Is", "anybody", "here", "I", "hate", "everyone", "She", "is", "I", "will", "come", "not", "my", "friend", "Hello", "I", "hate"] 
wordpairs = makewordpairs $ vocabs
xss = map  (\(a,b) -> a) wordpairs
yss = map  (\(a,b) -> b) wordpairs


dict = M.fromList (createddictionary vocabs)
getindices:: [String] -> [Int]
getindices vocabs = foldr (\x acc   -> (index x) : acc ) [] vocabs
index :: String -> Int
index x=(\(Just i) -> i) $ M.lookup x  dict
results = getindices vocabs
zeros:: [Float]
zeros = replicate (genericLength $ getindices vocabs) 0

onehotencoder ::Num q=> [Int] -> [q] ->[[q]]
onehotencoder xs zs = foldr (\x acc -> insertAt zs 1 x: acc) [] xs

insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 1  = elem : x : xs
    | pos > 1 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs) )
----(19,20)
----(_, 20)
ran :: [Float]
ran = replicate (genericLength $ getindices vocabs) 0.06
matrix_gen :: Num j=> [j] -> [j] -> [[j]]
matrix_gen ws zs = foldr(\x acc -> zs:acc) [] ws
input = onehotencoder results zeros
-- output = input * weight + biases
-- TODO we need to fix the insertAt function which add extra zero
-- TODO one possible way to go is:
-- TODO insertBy ((==) `on` elemIndex) some_value desired_index elem_tobe_added _the_list
b = 0.3
simple_dense::[Float] -> [Float] -> [Float]
simple_dense [] _ = []
simple_dense _ [] =[]
simple_dense [] [] = []
simple_dense  (x:xs) (y:ys) = x * y + b : simple_dense xs ys
softmax_denom = sum $ map exp $ input !! 0

softmax_activation :: [Float] -> [Float]
softmax_activation xs = foldr (\x acc -> (exp x)/softmax_denom :acc) [] xs

cross_entropy_loss :: [Float] -> [Float] -> [Float]
cross_entropy_loss [] _ = []
cross_entropy_loss _ [] = []
cross_entropy_loss [] [] = []
cross_entropy_loss (x:xs) (y:ys) = 
		let p = [1-x,x]
		    q = [1-y,y]
		    zipped = zipWith (\x y -> x * log y) p q	
		in zipped !!0 - zipped !! 1 : cross_entropy_loss xs ys

expected = input !! 0
predicted = softmax_activation expected
-- TODO expected should be renamed to sth else
-- T0D0 softmax should accept the values from the NN 
-- TODOcurrent expected is wrong
--T0D0 target or expected should be yss onehote encoding
cost = cross_entropy_loss expected predicted
m_prev :: Float
m_prev = 0.0
v_prev :: Float
v_prev = 0.0
alpha :: Float
alpha = 0.001
beta1 :: Float
beta1 = 0.9
beta2 :: Float
beta2 = 0.999
epsilon = 0.00000001
theta  = ran
mhat_previous :: Float -> Float -> Float -> Float
mhat_previous beta1 m_prev theta_current = (beta1 * m_prev + (1-beta1) * theta_current)/ (1-beta1)

vhat_previous :: Float -> Float -> Float -> Float
vhat_previous beta2 v_prev theta_current = (beta2 * v_prev + (1-beta2) * theta_current ^ 2) / (1-beta2^2)


vhat_compute = vhat_previous beta2  
mhat_compute = mhat_previous beta1  

-- TODO log is base 2
-- TODO thetas should be replaced with a func that computes the loss function wrt to thetas
adam_optimizer :: [Float] -> Float -> Float -> Float -> [Float]
adam_optimizer [] _ _ _ = []
adam_optimizer (t:thetas) alpha mhat_prev vhat_prev = do
					let mhat_current = mhat_compute t mhat_prev
					    vhat_current = vhat_compute t vhat_prev
					t - alpha * mhat_current / (sqrt $ vhat_current + epsilon): adam_optimizer thetas alpha mhat_current vhat_current

