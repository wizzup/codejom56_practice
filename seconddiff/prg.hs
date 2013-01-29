import Data.Numbers.Primes
import Data.List

main = interact $ f

f = unlines . map (h . g) . tail . lines

g n = map (\x -> read x::Int) $ words n

h [a,b,c,d] = show $ secdif a b c d

secdif a b c d = smax - smin
  where nm = nub $ nums a b c d
        smin = nm !! 1
        smax = (!!1) $ reverse nm

nums a b c d = sort $ [mkNum x | x <- permutations [a,b,c,d]]

mkNum [a,b,c,d] = d + c*10 + b*100 + a*1000

