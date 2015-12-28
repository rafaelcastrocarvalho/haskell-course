import Control.Monad.Writer


-- debugger function

f,g :: Float -> Float
f x =  x + 1
g x =  x * 2

f',g' :: Float -> (Float, String)
f' x  =  (f x, "f was called.")
g' x  =  (g x, "g was called.")

gf x = let (y,s) = g' x
           (z,t) = f' y in (z,s++t)

bind            :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f' (gx,gs) =  let (fx,fs) = f' gx in (fx,gs++fs)

b = bind f' . g'

unit   :: Float -> (Float, String)
unit x =  (x,"")

lift :: (Float -> Float) -> Float -> (Float, String)
lift f = unit . f


--multivalued function

fm   :: Float -> [Float]
fm x =  [x+1,x+2]

gm   :: Float -> [Float]
gm x =  [x+3,x+4,x+5]

bindm      :: (Float -> [Float]) -> [Float] -> [Float]
bindm f xs =  concat $ map f xs

unitm   :: Float -> [Float]
unitm x =  [x]

liftm f = unitm . f


xxx = do
        let x = 7
        y <- Writer (x+1,"inc\n")
        z <- Writer (2*y,"double\n")
        Writer (z-1,"dec\n")



