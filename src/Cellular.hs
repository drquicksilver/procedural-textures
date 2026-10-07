-- | Seeded feature points, exact first/second distances and Euclidean Voronoi
-- bisectors. One point per unit lattice cell; jitter is clamped to [0,1].
module Cellular
  ( Metric(..), Output(..), sample, value, identity, colour, edge, feature, hashCell ) where

import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32)
import Vector3

data Metric = Euclidean | Manhattan | Chebyshev deriving (Eq,Show,Enum,Bounded)
data Output = F1 | F2 | Gap deriving (Eq,Show,Enum,Bounded)
type Cell = (Int,Int,Int)

mix :: Word32 -> Word32
mix x = let a=(x `xor` (x `shiftR` 16))*0x7feb352d
            b=(a `xor` (a `shiftR` 15))*0x846ca68b
        in b `xor` (b `shiftR` 16)
hashCell :: Int -> Cell -> Word32
hashCell seed (x,y,z) = mix (fromIntegral seed `xor` (fromIntegral x*0x8da6b343) `xor` (fromIntegral y*0xd8163841) `xor` (fromIntegral z*0xcb1ab31f))
randoms :: Word32 -> Vec3
randoms h = (r 0x68bc21eb,r 0x02e5be93,r 0x967a889b)
  where r salt = fromIntegral (mix (h `xor` salt) .&. 65535)/65536
feature :: Int -> Double -> Int -> Cell -> Vec3
feature dims jitter seed cell@(x,y,z) =
  let (a,b,c)=randoms (hashCell seed cell); j=max 0 (min 1 jitter)
  in (fromIntegral x+0.5+j*(a-0.5),fromIntegral y+0.5+j*(b-0.5),if dims==2 then 0 else fromIntegral z+0.5+j*(c-0.5))
metric :: Metric -> Vec3 -> Double
metric Euclidean = norm
metric Manhattan = \(x,y,z) -> abs x+abs y+abs z
metric Chebyshev = \(x,y,z) -> max (abs x) (max (abs y) (abs z))
project :: Int -> Vec3 -> Vec3
project 2 (x,y,_) = (x,y,0)
project _ p = p
cells :: Int -> Int -> Vec3 -> [Cell]
cells dims radius (x,y,z) = [(a,b,c) | a <- [floor x-radius..floor x+radius],b <- [floor y-radius..floor y+radius],c <- if dims==2 then [0] else [floor z-radius..floor z+radius]]
-- Lower bound from query to a cell's enclosing unit box; prunes without hashing.
boxDelta :: Int -> Vec3 -> Cell -> Vec3
boxDelta dims (x,y,z) (a,b,c) = (axis x a,axis y b,if dims==2 then 0 else axis z c)
  where axis v i = max 0 (max (fromIntegral i-v) (v-fromIntegral i-1))

nearest :: Int -> Double -> Int -> Metric -> Vec3 -> (Double,Double,Cell,Vec3)
nearest dims jitter seed m point =
  let p=project dims point
      step best@(first,second,_,_) cell
        | metric m (boxDelta dims p cell) > second = best
        | otherwise = let q=feature dims jitter seed cell; d=metric m (sub q p)
                      in if d<first then (d,first,cell,q) else if d<second then (first,d,third best,fourth best) else best
      third (_,_,c,_) = c; fourth (_,_,_,q) = q
  -- F2 <= 3 for all metrics: the query cell has L1 distance <= 3;
  -- the neighbour across its nearest face along the most off-centre axis
  -- also has L1 distance <= 3 (other axis offsets cannot be larger).
  -- Any cell outside this neighbourhood is at least distance 3 away.
  in foldl' step (1/0,1/0,(0,0,0),(0,0,0)) (cells dims 3 p)
sample :: Int -> Double -> Int -> Metric -> Output -> Vec3 -> Double
sample dims jitter seed m out p = let (a,b,_,_)=nearest dims jitter seed m p in case out of F1 -> a; F2 -> b; Gap -> b-a
identity :: Int -> Double -> Int -> Vec3 -> Vec3
identity dims jitter seed p = let (_,_,(x,y,z),_)=nearest dims jitter seed Euclidean p in (fromIntegral x,fromIntegral y,fromIntegral z)
value :: Int -> Double -> Int -> Vec3 -> Double
value dims jitter seed p = let (_,_,cell,_)=nearest dims jitter seed Euclidean p
                          in fromIntegral (hashCell seed cell .&. 65535)/65536
-- Inspection vectors are [-1,1], so VectorColour produces random RGB in [0,1].
colour :: Int -> Double -> Int -> Vec3 -> Vec3
colour dims jitter seed p = let (_,_,cell,_)=nearest dims jitter seed Euclidean p
                           in sub (mul 2 (randoms (hashCell seed cell))) (1,1,1)
edge :: Int -> Double -> Int -> Vec3 -> Double
edge dims jitter seed point =
  let p=project dims point; (d,_,cell,q)=nearest dims jitter seed Euclidean p
      step best other
        | other==cell || norm (boxDelta dims p other)>d+2*best = best
        | otherwise = let r=feature dims jitter seed other; v=sub r q; len=norm v
                      in if len<1e-12 then best else min best (max 0 (dot (sub (mul 0.5 (add q r)) p) v/len))
  -- Covering radius is sqrt(dimensions). Every cell boundary is within
  -- that distance; bisectors farther than d+2*best cannot improve it.
  -- d <= sqrt(3), best <= sqrt(3), so radius 6 encloses all competitors.
      initial=foldl' step (sqrt (if dims==2 then 2 else 3)) (cells dims 1 q)
      radius=min 6 (ceiling (d+2*initial))
  in foldl' step initial (cells dims radius p)
