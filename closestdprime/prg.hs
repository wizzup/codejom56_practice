import Data.Numbers.Primes
import Data.List

main = interact $ f

f = unlines . map g . tail . lines

g n = show $ (!!1) $ res (read n::Int)

cc n = takeWhile (< n) primes

dd n = [[n-(a+b),a+b, a,b] | a <- c, b <-c, a + b <= n]
  where c = cc n

res n = head $ sort $ dd n




