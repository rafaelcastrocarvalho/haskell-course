type State = Int

data ST a = S (State -> (a, State))

apply         :: ST a -> State -> (a,State)
apply (S f) x =  f x


instance Monad ST where
  -- return :: a -> ST a
  return x  = S (\s -> (x,s))

  -- (>>=)  :: ST a -> (a -> ST b) -> ST b
  st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a)

fresh :: ST Int
fresh =  S (\n -> (n, n+1))

mlabel            :: Tree a -> ST (Tree (a,Int))
mlabel (Leaf x)   =  do n <- fresh
                        return (Leaf (x,n))
mlabel (Node l r) =  do l' <- mlabel l
                        r' <- mlabel r
                        return (Node l' r')

