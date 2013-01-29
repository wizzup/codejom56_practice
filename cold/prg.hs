import Data.List
import Data.List.Split

main = interact $ f

f = unlines . map g . chunksOf 2 . tail . lines

-- g (a:b:[]) = res $ h (x, y)
g (a:b:[]) = i $ h (x, y)
  where x = read a :: Int
        y = map (\x -> read x::Int) $ words b 

h (x,y) = (y,rr,cc)
  where r = map (take 2) (tails y)
        l = take 2 $ repeat (head y)
        rr = filter (\x -> length x == 2) (l:r)
        cc = map isCold rr
        -- cc = map isCold rr

-- i (a,b,c) = show (a,b,c)
i (a,b,c) = show $ sum $ map cc c
  where cc x = if x then 1 else 0

isCold [x,y] = a || b 
  where a = if x - y > 7 then True else False 
        b = if y < 10 then True else False

