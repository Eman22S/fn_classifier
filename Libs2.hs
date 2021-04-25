module Libs
( removeStopWords
,makeWordPairs
,createDictionary
,getIndices
,index
,oneHotEncoder
,insertAt
,ran
,zeros
,xss
,yss
,rawToCleaned
,matrixGen
,nn_Layer
--,grad_CrossEntropy 
,ran
)where


import Data.List 
import qualified Data.Map as M

stopwords = ["the", "a", "and", "is", "be", "will"]
nums = "0123456789)(*&^%$#@!-=_+:;?"

ins = [[1.0, 3.0] , [2.89, 4.78]]
ws = [[0.0, 1.0] , [10.3, 0.7]]
tar = [[1.30, 13.0] , [0.89, 6.78]]
lpha = 0.001
eta1 = 0.9
eta2 = 0.999
eps = 0.001
iter = 10

data Params = Params {weights:: [[Float]], 
			inputs:: [[Float]], 
			targets:: [[Float]], 
			alpha:: Float, 
			beta1:: Float, 
			beta2:: Float, 
			epsilon:: Float, 
			iterations:: Int} deriving (Show)

removeStopWords :: String -> [String]
removeStopWords line = filter (not . ((flip elem ) stopwords )) (words line)

makeWordPairs :: [String] -> [(String,String)]
makeWordPairs [] = []
makeWordPairs (x:y:z:h:xs) = (x,y):(x,z):(y,x):(y,z):(y,h):(z,x):(z,y):(z,h):(h,x):(h,y):(h,z):makeWordPairs xs
makeWordPairs (x:y:z:xs) = (x,y):(x,z):(y,x):(y,z):(z,x):(z,y):makeWordPairs xs
makeWordPairs (x:y:xs) =  (x,y):makeWordPairs xs

createDictionary::(Integral i) => [String] -> [(String, i)]
createDictionary vocabs = zip  (sort . nub $ vocabs) [1..]


oneHotEncoder :: (Num q) => [Int] -> [q] -> [[q]]
oneHotEncoder xs zs = foldr (\x acc -> insertAt zs 1 x: acc) [] xs

insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 1  = elem : x : xs
    | pos > 1 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs) )

ran :: [String] ->[(String,Int)] -> [Float]
ran vocabs dict = replicate (genericLength $ getIndices vocabs dict) 0.06

zeros :: [String] -> [(String,Int)] -> [Double]
zeros vocabs dict = replicate (length $ getIndices vocabs dict) 0

xss :: [(String, String)] -> [String]
xss wordpairs = map (\(a,b) -> a) wordpairs

yss :: [(String, String)] -> [String]
yss wordpairs = map (\(a,b) -> b) wordpairs

rawToCleaned :: [String] -> [String]
rawToCleaned [] = []
rawToCleaned (x:xs) = let sm_var = removeStopWords x
		      in sm_var ++ rawToCleaned xs

matrixGen :: Num j=> [j] -> [j] -> [[j]]
matrixGen ws zs = foldr(\x acc -> zs:acc) [] ws

b :: (Floating f) => f
b = 0.3

nn_Layer::(Floating f) => [f] -> [f] -> [f]
nn_Layer [] _ = []
nn_Layer _ [] = []
nn_Layer [][] = []
nn_Layer (x:xs)(y:ys) = x * y + b : nn_Layer xs ys

index :: String -> [(String,Int)] -> Int
index x dict = (\(Just i) -> i) $ lookup  x dict

getIndices :: [String] -> [(String, Int)] -> [Int]
getIndices vocabs dict = foldr (\x acc -> (index x dict):acc) [] vocabs

softmax_activation ::(Floating q) => [q] -> [q]
softmax_activation xs = let softmax_denom = sum $ map exp xs
 			in foldr (\x acc -> (exp x)/softmax_denom :acc) [] xs

{- 
fullNNLayer :: Params -> [[Float]]
fullNNLayer ps = softmax_activation $ nn_Layer x y : fullNNLayer _inputs _weights
		where
		x = head $ inputs ps
		y = head $ weights ps
		_weights = tail $ weights ps
		_inputs = tail $ inputs ps
		

adam_Optimizer :: Params -> Float -> Float -> [Float] -> [Float]
adam_Optimizer p m_prev v_prev (gt:gts) = do
		let m_current = (_beta1 * m_prev) + (1 - _beta1) * gt 
		let v_current = _beta2 * v_prev + (1 - _beta2) * gt ^ 2
		let mhat_current = m_current / (1 - _beta1)
		let vhat_current = v_current/ (1 - _beta2)
		t - _alpha * mhat_current / ((sqrt vhat_current) + epsilon):adam_Optimizer _thetas gts _alpha m_current v_current _beta1 _beta2 _epsilon 
		where
		    _beta1 = beta1 p
		    _beta2 = beta2 p
		    _alpha = alpha p
		    _epsilon = epsilon p
		    t = head $ weights p
		    _thetas = tail $ weights p


optimizer :: (Params -> Float -> Float -> [Float] -> [Float]) -> Params ->[[Float]]-> Float -> Float -> [[Float]] 
optimizer _ _ [] _ _ = []
optimizer f p (cs:costs) m v = f ws cs _alpha m v _beta1 _beta2 _epsilon:optimizer f p wss costs _alpha m v _beta1 _beta2 _epsilon 
		where
		ws = head $ weights p
		wss = tail $ weights p
		_alpha = alpha p
		_beta1 = beta1 p
		_beta2 = beta2 p
		_epsilon = epsilon p

model :: Params ->  [[Float]]
model ps  | iterations ps == 0 = weights ps 
	  | otherwise = do
			let pred = fullNNLayer ps 
			    full_cost = loss_func grad_CrossEntropy pred (targets ps)
			    new_weights = optimizer adam_Optimizer ps full_cost 0.0 0.0
			    new_params = Params {
			    weights = new_weights, 
			    inputs = inputs ps, 
			    targets = targets ps,
			    alpha = alpha ps, 
			    beta1 = beta1 ps , 
			    beta2 = beta2 ps, 
			    epsilon = epsilon ps, 
			    iterations = (iterations ps) - 1}
			model new_params
-}
