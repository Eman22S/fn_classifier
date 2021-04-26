module Libs
( removeStopWords
,makeWordPairs
,Str
,createDictionary
,getIndices
,index
,model
,oneHotEncoder
,insertAt
,ran
,zeros
,xss
,yss
,rawToCleaned
,matrixGen
,nn_Layer
,ce_loss 
)where


import Data.List 
import qualified Data.Map as M

type Str = String
stopwords = ["the", "a", "and", "is", "be", "will"]
nums = "0123456789)(*&^%$#@!-=_+:;?"

log2 :: (Floating j) => j -> j
log2 = logBase 2

b :: (Floating f) => f
b = 0.3

removeStopWords :: Str -> [Str]
removeStopWords line = filter (not . ((flip elem ) stopwords )) (words line)

makeWordPairs :: [Str] -> [(Str,Str)]
makeWordPairs [] = []
makeWordPairs (x:y:z:h:xs) = (x,y):(x,z):(y,x):(y,z):(y,h):(z,x):(z,y):(z,h):(h,x):(h,y):(h,z):makeWordPairs xs
makeWordPairs (x:y:z:xs) = (x,y):(x,z):(y,x):(y,z):(z,x):(z,y):makeWordPairs xs
makeWordPairs (x:y:xs) =  (x,y):makeWordPairs xs
makeWordPairs (x:xs) = (x, "null"):makeWordPairs xs

createDictionary::(Integral i) => [Str] -> [(Str, i)]
createDictionary vocabs = zip  (sort . nub $ vocabs) [1..]

oneHotEncoder :: (Num q) => [Int] -> [q] -> [[q]]
oneHotEncoder xs zs = foldr (\x acc -> insertAt zs 1 x: acc) [] xs

insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 1  = elem : xs
    | otherwise = x : insertAt xs elem (pos - 1) 

ran :: [Str] -> [(Str,Int)] -> [Float]
ran vocabs dict = replicate (genericLength $ getIndices vocabs dict) 0.06

zeros :: [Str] -> [(Str,Int)] -> [Double]
zeros vocabs dict = replicate (length $ getIndices vocabs dict) 0

xss :: [(Str, Str)] -> [Str]
xss wordpairs = map (\(a,b) -> a) wordpairs

yss :: [(Str, Str)] -> [Str]
yss wordpairs = map (\(a,b) -> b) wordpairs

index :: (Num n) => Str -> [(Str,n )] -> n
index x dict = case lookup x dict of
               Nothing -> -1
	       Just i -> i


getIndices :: [Str] -> [(Str, Int)] -> [Int]
getIndices vocabs dict = foldr (\x acc -> (index x dict):acc) [] vocabs

rawToCleaned :: [Str] -> [Str]
rawToCleaned [] = []
rawToCleaned (x:xs) = let sm_var = removeStopWords x
                      in sm_var ++ rawToCleaned xs
matrixGen :: Num j=> [j] -> [j] -> [[j]]
matrixGen ws zs = foldr(\x acc -> zs:acc) [] ws

nn_Layer::(Floating f) => [f] -> [f] -> [f]
nn_Layer (x:xs) (y:ys) = zipWith (\x y -> x * y + b) xs ys

nnLinear::(Floating f) => [f] -> [f] -> [f]
nnLinear (x:xs) (y:ys) = zipWith (\x y -> x * y + b) xs ys

fullNNLayer :: (Floating q) => [[q]] -> [[q]] -> [[q]]
fullNNLayer _ [] = []
fullNNLayer [] _ = []
fullNNLayer (x:inputs) (y:weights) = (softmax_activation $ nn_Layer x y) : fullNNLayer inputs weights

softmax_activation ::(Floating q) => [q] -> [q]
softmax_activation xs = let softmax_denom = sum $ map exp xs
                        in foldr (\x acc -> (exp x)/softmax_denom :acc) [] xs

loss_func :: (Floating j) => ([j] -> [j] -> [j]) -> [[j]] -> [[j]] -> [j]
loss_func f [] _ = []
loss_func f _ [] = []
loss_func f (p:preds) (t:targets) = (ce_mean $  f p t) : loss_func f preds targets

ce_loss :: (Floating q) => [q] -> [q] -> [q]
ce_loss [] _ = []
ce_loss _ [] = []
ce_loss (y:ys) (yh:yhs) =  - (( y * log2 yh) + ((1 - y) * log2 1 - yh)) : ce_loss ys yhs

ce_mean :: (Floating j) => [j] -> j
ce_mean xs = (sum xs) / genericLength xs

adam_Optimizer :: (Floating j) => [j] -> j -> j -> j -> j -> j -> j -> j -> [j]
adam_Optimizer [] _ _ _ _ _ _ _ = []
adam_Optimizer (t:thetas) gt alpha m_prev v_prev beta1 beta2 epsilon = do
                                let m_current = beta1 * m_prev + (1-beta1) * gt
                                let v_current = beta2 * v_prev + (1-beta2) * gt ^ 2
                                let mhat_current = m_current / (1-beta1)
                                let vhat_current = v_current / (1-beta2)
                                t - alpha * mhat_current / ( (sqrt vhat_current) + epsilon): adam_Optimizer thetas gt alpha m_current v_current beta1 beta2 epsilon


optimizer::(Floating q) =>  ([q] -> q -> q -> q -> q -> q -> q -> q -> [q]) -> [[q]] -> q -> q -> q -> q -> q -> q -> q -> [[q]] 
optimizer f [] _ _ _ _ _ _ _ = []
optimizer f (ws:wss) cost alpha m v beta1 beta2 epsilon = (f ws cost alpha m v beta1 beta2 epsilon):optimizer f wss cost alpha m v beta1 beta2 epsilon 


model :: (Floating j) => [[j]] -> [[j]] -> [[j]] -> j -> j -> j -> j -> Int -> [[j]]
model [] _ _ _ _ _ _ _ = []
model _ [] _ _ _ _ _ _ = []
model _ _ [] _ _ _ _ _ = []
model weights inputs targets alpha beta1 beta2 epsilon iterations 
                        |iterations == 0 = weights
                        |otherwise = do
                                let predicted = fullNNLayer inputs weights
                                let cost = ce_mean $ loss_func ce_loss  predicted targets
                                let new_weights = optimizer adam_Optimizer weights cost alpha 0.0 0.0 beta1 beta2 epsilon
                                model new_weights inputs targets alpha beta1 beta2 epsilon (iterations - 1)
