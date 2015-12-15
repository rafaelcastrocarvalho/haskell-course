import Data.Char
import Control.Applicative

ys = [1,2,3,4,5,6]

--find      :: Eq a => a -> [(a, b)] -> [b]
--find k t  =  [v | (k',v) <- t, k == k']

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

data Expr' = Val' Int | Div' Expr' Expr'
data Maybe' a = Nothing' | Just' a

safediv     :: Int -> Int -> Maybe' Int
safediv n m =  if m == 0 then Nothing' else Just' (n `div` m)

eval'            :: Expr' -> Maybe' Int
eval' (Val' n)   =  Just' n
eval' (Div' x y) =  do n <- eval' x
                       m <- eval' y
                       safediv n m

--eval' (Div' x y) =  eval' x >>== (\n -> eval' y >>== (\m -> safediv n m))

--eval' (Div' x y) =  apply' f (eval' x `seqn'` eval' y)
--                    where f (n,m) = safediv n m

--eval' (Div' x y) =  case eval' x of
--                      Nothing' -> Nothing'
--                      Just' n  -> case eval' y of
--                                    Nothing' -> Nothing'
--                                    Just' m  -> safediv n m

seqn' :: Maybe' a -> Maybe' b  -> Maybe' (a, b)
seqn'    Nothing'    _         =  Nothing'
seqn'    _           Nothing'  =  Nothing'
seqn'    x           y         =  do n <- x
                                     m <- y
                                     Just' (n, m)
--seqn'  x           y         =  x >>= (\n -> y >>= (\m -> Just' (n, m) ))
--seqn' (Just' x)    (Just' y) = Just' (x, y)

apply'             :: (a -> Maybe' b) -> Maybe' a -> Maybe' b
apply' f Nothing'  =  Nothing'
apply' f (Just' x) =  f x


instance Monad Maybe' where
  -- return :: a -> Maybe' a
  return x  =  Just' x

  -- (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Nothing'  >>= _ = Nothing'
  (Just' x) >>= f = f x

pairs               :: [a] -> [b] -> [(a,b)]
pairs xs ys         =  do x <- xs
                          y <- ys
                          return (x,y)
--pairs xs ys       =  xs >>==     (\x -> ys >>==       (\y -> return (x, y)))
--pairs xs ys       =  concat (map (\x -> ys >>==       (\y -> return (x, y))       ) xs)
--pairs xs ys       =  concat (map (\x -> (concat ( map (\y -> return (x, y)) ys )) ) xs)
--pairs [1,2] [3,4] =                      concat ( [[(1,3)], [(1,4)]]           )
--                                         [(1,3), (1,4)]
--
--                                         concat ( [[(2,3)], [(2,4)]]           )
--                                         [(2,3], (2,4)]
--
--                     concat ( [[(1,3), (1,4)], [(2,3], (2,4)]] )
--                     [(1,3), (1,4), (2,3], (2,4)]

(>>==)    :: [a] -> (a -> [b]) -> [b]
xs >>== f =  concat (map f xs)

f1 = do x <- [1,2]
        [x, x+1] -- this is monad, right?

f2 = do x <- [1,2]
        return [x, x+1]


type State = Int

data ST a = S (State -> (a, State))

apply         :: ST a -> State -> (a,State)
apply (S f) x =  f x

type Post = String

findPost    :: Int -> Maybe Post
findPost x  =  if x == 1 then (Just "The Post") else Nothing

getPostTitle   :: Post -> String
getPostTitle p = p

type Assoc k v = [(k, v)]

find     :: Eq k => k -> Assoc k v -> v
find k t =  head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

data Prop = AConst Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

s = [('A', False), ('B', True)]

eval               :: Subst -> Prop -> Bool
eval _ (AConst b)  =  b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars              :: Prop -> [Char]
vars (AConst _)   =  []
vars (Var x)      =  [x]
vars (Not p)      =  vars p
vars (And p q)    =  vars p ++ vars q
vars (Imply p q)  =  vars p ++ vars q

bools         :: Int -> [[Bool]]
bools 0       =  [[]]
bools (n + 1) =  map (False:) bss ++ map (True:) bss
                 where bss = bools n
