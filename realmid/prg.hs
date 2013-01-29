import Data.List
import Data.List.Split

main = interact $ f

f = unlines . map g . chunksOf 2 . tail . lines

g (a:b:[]) = res $ h (x, y)
  where x = read a :: Int
        y = map (\x -> read x::Float) $ words b 

h (x,y) = (mid,rmid,raw)
  where
    yy = sort y
    l = head yy
    h = last yy
    mid = (h+l)/2
    pmid = (x + 1) `div` 2
    rmid = yy !! (pmid - 1)
    raw = sort y

res (a,b,c) = if a == b then "yes" else "no"
