--import Data.List
import System.IO
import Data.Char
import qualified Data.Map as M
import Libs 

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
dataset :: [Str]
dataset = [ "The future king is the prince", 
 --          "Daughter is the princess.",
 --          "Son is the prince.",
 --          "Only a man can be a king",
 --          "Only a woman can be a queen",
 --          "The princess will be a queen",
 --          "Queen and king rule the realm",
 --          "The prince is a strong man",
 --          "The princess is a beautiful woman",
 --          "The royal family is the king and queen and their children",
 --          "Prince is only a boy now",
           "A boy will be a man"]



cleaned :: [Str]
cleaned = rawToCleaned dataset

paired :: [(Str, Str)]
paired = makeWordPairs cleaned

dict ::  [(Str,Int)]
dict = createDictionary cleaned

input = xss paired

target = yss paired

zs = zeros cleaned dict

ran_wht = replicate (length $ getIndices input dict) 0.06

xss_index = getIndices input dict

yss_index = getIndices target dict

weights = matrixGen ran_wht zs

en_inputs = oneHotEncoder xss_index zs

en_targets = oneHotEncoder yss_index zs

bias = 0.3

main :: IO ()
main = print $ model weights en_inputs en_targets 0.001 0.9 0.999 0.00000001 2

-- TODO implement the accuracy function
-- TODO combine reading file
-- TODO implemement random value generation
-- TODO improve performance and accuracy


