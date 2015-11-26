import Data.Char

ys = [1,2,3,4,5,6]

find      :: Eq a => a -> [(a, b)] -> [b]
find k t  =  [v | (k',v) <- t, k == k']

positions       ::  Eq a => a -> [a] -> [Int]
positions x xs  =   [i | (x',i) <- zip xs [0 .. n], x == x']
                    where n = length xs - 1


let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

xs = 1 : [x + 1 | x <- xs]

riffle xs ys = concat [ [x, y] | (x,y) <- xs `zip` ys]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors n = [x | x <- [1..n], n `divides` x]


type Bit = Int

bin2int :: [Bit] -> Int
bin2int  = foldr (\x y -> x + 2 * y) 0

int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 bits = take 8 (bits ++ repeat 0)

encode' :: String -> [Bit]
encode'  = concat . map (make8 . int2bin . ord)

chop8      :: [Bit] -> [[Bit]]
chop8 []    = []
chop8 bits  = take 8 bits : chop8 (drop 8 bits)

decode' :: [Bit] -> String
decode'  = map (chr . bin2int) . chop8

transmit :: String -> String
transmit  = decode' . channel' . encode'

channel' :: [Bit] -> [Bit]
channel'  = id

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

-- Segmentation fault on a infinite list
--map' :: (a -> b) -> [a] -> [b]
--map' f = foldl (\ xs x -> xs ++ [f x]) []

-- Works on a infinite list
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x xs -> [f x] ++ xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldl (\ xs x -> if p x then xs ++ [x] else xs) []

dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10 * x + y) 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

sumsqreven = compose [map (^ 2), filter even]


