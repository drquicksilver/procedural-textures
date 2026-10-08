-- | Shared XY ownership and projections; Z passes through local coordinates.
module Layout (Layout(..), sample, local, identity, edge, value) where
import Vector3 (Vec3)
import qualified Cellular as C
import Data.Bits ((.&.))
import Data.List (minimumBy)
import Data.Ord (comparing)
data Layout = Grid | RunningBond | Hex | Herringbone deriving(Eq,Show,Enum,Bounded)
sample :: Layout -> Vec3 -> (Vec3,(Int,Int,Int),Double)
sample layout (x,y,z) = case layout of
  Grid -> rectangular (floor x,floor y,0) (x-fromIntegral(floor x::Int)-0.5,y-fromIntegral(floor y::Int)-0.5,z) 0.5
  RunningBond -> let j=floor y; shift=0.5*fromIntegral(j `mod` 2); i=floor(x-shift)
     in rectangular (i,j,0) (x-fromIntegral i-shift-0.5,y-fromIntegral j-0.5,z) 0.5
  Herringbone ->
    let i=floor x; j=floor y; d=(i-j) `mod` 4
        (a,b)=case d of 3->(i,j-1);2->(i-1,j);_->(i,j)
        q=if d==0 || d==3 then (x-fromIntegral a-0.5,y-fromIntegral b-1,z) else (y-fromIntegral b-0.5,fromIntegral a+1-x,z)
    in rectangular (a,b,0) q 1
  Hex -> let h=sqrt 3/2; j0=floor(y/h+0.5); i0=floor(x-0.5*fromIntegral j0+0.5)
             candidates=[(i,j) | i<-[i0-1..i0+1],j<-[j0-1..j0+1]]
             delta (i,j)=(x-fromIntegral i-0.5*fromIntegral j,y-h*fromIntegral j,z)
             distance ij=let (u,v,_)=delta ij in u*u+v*v
             (i,j)=minimumBy (comparing distance) candidates
             q@(u,v,_)=delta(i,j)
         in (q,(i,j,0),max 0 (0.5-maximum[abs u,abs(0.5*u+h*v),abs(0.5*u-h*v)]))
  where rectangular cell q@(u,v,_) halfY=(q,cell,max 0 (min (0.5-abs u) (halfY-abs v)))
local :: Layout -> Vec3 -> Vec3
local l p=let (q,_,_)=sample l p in q
identity :: Layout -> Vec3 -> Vec3
identity l p=let (_,(i,j,k),_)=sample l p in (fromIntegral i,fromIntegral j,fromIntegral k)
edge :: Layout -> Vec3 -> Double
edge l p=let (_,_,e)=sample l p in e
value :: Layout -> Int -> Vec3 -> Double
value l seed p=let (_,cell,_)=sample l p in fromIntegral(C.hashCell seed cell .&. 65535)/65536
