import Data.Numbers.Primes
import Data.List

main = interact $ f

f = unlines . map g . tail . lines

g n = show $ res (read n::Int)

gen n = (div n 2) + 10

ll n = iterate gen n

df n = takeWhile (/=0) $ zipWith (-) l $ tail l
  where l = ll n

res n = length $ df n


