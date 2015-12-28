import Control.Monad

newtype Parser a =  P (String -> [(a,String)])

instance Monad Parser where
  return v =  P (\inp -> [(v,inp)])
  p >>= f  =  P (\inp ->
                    case parse p inp of
                        [] -> []
                        [(v, out)] -> parse (f v) out)

--return'    :: a -> Parser a
--return' v  =  \inp -> [(v, inp)]
--
--(>>=)   :: Parser a -> (a -> Parser b) -> Parser b
--p >>= f  = \inp -> case parse p inp of
--                        [] -> []
--                        [(v, out)] -> parse (f v) out

failure :: Parser a
failure =  P (\inp -> [])

item :: Parser Char
item =  P (\inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x, xs)])

parse            :: Parser a -> String -> [(a, String)]
parse (P p) inp  =  p inp

parse (item >>= (\x ->
       item >>= (\y ->
       return [x,y]))

       ) "bla"

