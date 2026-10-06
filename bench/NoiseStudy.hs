-- Reproduce the gradient-distribution sanity check in the Phase 2 log.
-- Run: stack exec runghc -- -isrc bench/NoiseStudy.hs
import Perlin
import Data.List (sort)
main :: IO ()
main = do
 let points = [(fromIntegral x/17+0.13, fromIntegral y/19+0.37,fromIntegral z/13+0.21) | x <- [0..50::Int], y <- [0..50::Int], z <- [0..4::Int]]
     values = sort [perlin3 x y z | (x,y,z) <- points]
     quantile q = values !! floor (q*fromIntegral (length values-1))
     h = 0.001
     energy axis = sum [(case axis of
        0 -> perlin3 (x+h) y z-perlin3 (x-h) y z
        1 -> perlin3 x (y+h) z-perlin3 x (y-h) z
        _ -> perlin3 x y (z+h)-perlin3 x y (z-h))^ (2::Int) | (x,y,z) <- points] / fromIntegral (length points)
 print (length values, quantile 0.01,quantile 0.5,quantile 0.99)
 print (map energy [0,1,2::Int])
