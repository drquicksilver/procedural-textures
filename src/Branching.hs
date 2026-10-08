-- | A finite seeded binary branching network; no per-query growth.
module Branching (Config(..), Segment(..), segments, distance, validate) where
import Vector3 (Vec3,add,sub,mul,dot,norm,normalise)
import qualified Cellular as C

data Config = Config { dimensions :: Int, seed :: Int, depth :: Int, lengthScale :: Double, spread :: Double, taper :: Double, radius :: Double } deriving(Eq,Show)
data Segment = Segment Vec3 Vec3 Double Double deriving(Eq,Show)
validate :: Config -> Either String Config
validate c
 | dimensions c `notElem` [2,3] || seed c<0 || toInteger(seed c)>4294967295 || depth c<1 || depth c>7 = Left "Branching requires dimensions 2/3, unsigned seed and depth 1–7"
 | any (\(x,lo,hi)->isNaN x || isInfinite x || x<lo || x>hi) [(lengthScale c,0.05,0.5),(spread c,5,80),(taper c,0.3,0.95),(radius c,0.001,0.1)] = Left "Branching parameters exceed supported bounds"
 | otherwise=Right c
segments :: Config -> [Segment]
segments c=either error (const (grow 1 (depth c) (0.5,0.06,if dimensions c==2 then 0 else 0.5) (0,1,0) (lengthScale c) (radius c))) (validate c)
 where
 grow _ 0 _ _ _ _=[]
 grow identity remaining start direction len r=
  let end=add start (mul len direction); endRadius=r*taper c
      child side=let (rx,ry,_)=C.randoms(C.hashCell (seed c) (identity,side,remaining))
                     angle=(fromIntegral side*(spread c)+(rx-0.5)*12)*pi/180
                     (x,y,z)=direction
                     v=(x*cos angle-y*sin angle,x*sin angle+y*cos angle,z+(if dimensions c==2 then 0 else (ry-0.5)*0.2))
                 in grow (identity*2+if side<0 then 0 else 1) (remaining-1) end (normalise v) (len*0.65) endRadius
  in Segment start end r endRadius : child (-1) <> child 1
-- Signed distance-like tube envelope with linearly interpolated radius.
-- This is not an exact SDF for tapered tubes or overlapping branches.
distance :: [Segment] -> Vec3 -> Double
distance network p=minimum (map segment network)
 where segment (Segment a b ra rb)=let v=sub b a; d=dot v v;t=if d==0 then 0 else max 0(min 1(dot (sub p a) v/d)) in norm(sub p(add a(mul t v)))-(ra+(rb-ra)*t)
