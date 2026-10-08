{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | Deterministic Float32 Gray–Scott volumes, periodic six-neighbour diffusion.
module Reaction (Config(..), Initial(..), Chemical(..), simulate, sample, cachedVolume, validate, validateFields, simulateFields, sampleFields, cachedFields) where
import Control.Monad (forM_)
import Control.Monad.ST (ST,runST)
import Data.Array.ST (STUArray,newArray,readArray,writeArray,freeze)
import Data.Array (Array,listArray)
import qualified Data.Array
import Data.Array.Unboxed (UArray,(!))
import Data.Bits ((.&.))
import qualified Cellular as C
import Control.Concurrent.MVar (MVar,newMVar,modifyMVar,readMVar)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Vector3 (Vec3)

data Chemical = U | V deriving (Eq,Show,Enum,Bounded)
data Initial = NoisePatches | SeedSpots | SeedSlab deriving (Eq,Show,Ord)
data Config = Config { resolution :: Int, iterations :: Int, feed :: Double, kill :: Double
  , diffusionU :: Double, diffusionV :: Double, timeStep :: Double, seed :: Int, initial :: Initial }
  deriving (Eq,Show,Ord)
validate :: Config -> Either String Config
validate c
  | resolution c<8 || resolution c>64 = Left "Reaction resolution must be an integer from 8 to 64"
  | iterations c<0 || iterations c>4096 || toInteger (resolution c)^ (3::Int)*toInteger (iterations c)>64000000 = Left "Reaction iterations must be 0–4096, with at most 64000000 voxel updates"
  | seed c<0 || toInteger(seed c)>4294967295 = Left "Reaction seed must be an unsigned 32-bit integer"
  | any (\(v,hi)->isNaN v || isInfinite v || v<0 || v>hi) [(feed c,0.1),(kill c,0.1),(diffusionU c,1),(diffusionV c,1),(timeStep c,1)] = Left "Reaction chemistry is outside its supported range"
  | otherwise = Right c
simulate :: Config -> UArray Int Float
simulate c = either error (const (runST build)) (validate c)
  where
    n=resolution c; count=2*n*n*n
    build :: forall s. ST s (UArray Int Float)
    build = do
      a <- newArray (0,count-1) 0 :: ST s (STUArray s Int Float)
      b <- newArray (0,count-1) 0 :: ST s (STUArray s Int Float)
      forM_ [0..n-1] $ \z -> forM_ [0..n-1] $ \y -> forM_ [0..n-1] $ \x -> do
        let random=fromIntegral (C.hashCell (seed c) (x,y,z) .&. 65535)/65536 :: Float
            block=max 2 (n `div` 6)
            active=case initial c of
              NoisePatches -> C.hashCell (seed c) (x `div` block,y `div` block,z `div` block) .&. 255 < 64
              SeedSpots -> all (\v->v `mod` max 4 (n `div` 2)<max 2 (n `div` 8)) [x,y,z]
              SeedSlab -> z*5<n
            at=2*(x+n*(y+n*z))
        writeArray a at (if active then 0.5+0.1*(random-0.5) else 1)
        writeArray a (at+1) (if active then 0.25+0.05*(random-0.5) else 0)
      let f=realToFrac(feed c); k=realToFrac(kill c); du=realToFrac(diffusionU c); dv=realToFrac(diffusionV c); dt=realToFrac(timeStep c)
          index x y z=2*((x `mod` n)+n*((y `mod` n)+n*(z `mod` n)))
          clamp v=max 0 (min 1 v)
          step source target = forM_ [0..n-1] $ \z -> forM_ [0..n-1] $ \y -> forM_ [0..n-1] $ \x -> do
            let at=index x y z
                lap lane center = do
                  s1<-readArray source (index (x+1) y z+lane)
                  s2<-readArray source (index (x-1) y z+lane)
                  s3<-readArray source (index x (y+1) z+lane)
                  s4<-readArray source (index x (y-1) z+lane)
                  s5<-readArray source (index x y (z+1)+lane)
                  s6<-readArray source (index x y (z-1)+lane)
                  pure (((((s1+s2)+s3)+s4)+s5+s6)/6-center)
            u<-readArray source at; v<-readArray source (at+1)
            lu<-lap 0 u; lv<-lap 1 v
            let reaction=(u*v)*v
            writeArray target at (clamp (u+dt*((du*lu-reaction)+f*(1-u))))
            writeArray target (at+1) (clamp (v+dt*((dv*lv+reaction)-(f+k)*v)))
          loop 0 source _=freeze source
          loop remaining source target=step source target >> loop (remaining-1) target source
      loop (iterations c) a b

-- A bounded, thread-safe cache is an optimisation of this pure deterministic
-- function. Keep only four completed volumes; no document state is stored here.
{-# NOINLINE volumes #-}
volumes :: MVar [(Config,UArray Int Float)]
volumes=unsafePerformIO (newMVar [])
{-# NOINLINE cachedVolume #-}
cachedVolume :: Config -> UArray Int Float
cachedVolume c=unsafePerformIO $ modifyMVar volumes $ \entries -> case lookup c entries of
  Just v -> pure ((c,v):filter ((/=c).fst) entries,v)
  Nothing -> do v<-evaluate (simulate c); pure (take 4 ((c,v):entries),v)

-- | Periodic trilinear sampling with lattice samples at voxel centres.
sample :: UArray Int Float -> Int -> Int -> Vec3 -> Double
sample volume n lane (x,y,z) =
  let coord v=let q=(v-fromIntegral(floor v::Int))*fromIntegral n-0.5; i=floor q in (i,q-fromIntegral i)
      (ix,tx)=coord x; (iy,ty)=coord y; (iz,tz)=coord z
      at a b c=realToFrac(volume ! (2*((a `mod` n)+n*((b `mod` n)+n*(c `mod` n)))+lane))
      lerp t a b=a+(b-a)*t
      xy c=lerp ty (lerp tx (at ix iy c) (at (ix+1) iy c)) (lerp tx (at ix (iy+1) c) (at (ix+1) (iy+1) c))
  in lerp tz (xy iz) (xy (iz+1))

-- | Additive field-driven solver, preserving the original 3D initialisations.
validateFields :: Int -> Config -> Either String Config
validateFields dims c
 | dims `notElem` [2,3] = Left "Reaction dimensions must be 2 or 3"
 | resolution c<8 || resolution c>(if dims==2 then 256 else 64) = Left "Field reaction resolution exceeds dimensional bounds"
 | iterations c<0 || iterations c>4096 || toInteger(resolution c)^dims*toInteger(iterations c)>64000000 = Left "Field reaction exceeds 64000000 updates"
 | otherwise=validate c {resolution=8,iterations=0} >> Right c
simulateFields :: Int -> Config -> (Vec3->Double) -> (Vec3->Double) -> (Vec3->Double) -> UArray Int Float
simulateFields dims c seedFn feedFn killFn=either error (const(runST build)) (validateFields dims c)
 where
 n=resolution c; nz=if dims==2 then 1 else n;count=2*n*n*nz
 clamp hi v=realToFrac(max 0(min hi(if isNaN v || isInfinite v then 0 else v))) :: Float
 inputs=listArray (0,n*n*nz-1) [ (clamp 1(seedFn p),clamp 0.1(feedFn p),clamp 0.1(killFn p)) | z<-[0..nz-1],y<-[0..n-1],x<-[0..n-1],let p=((fromIntegral x+0.5)/fromIntegral n,(fromIntegral y+0.5)/fromIntegral n,if dims==2 then 0 else (fromIntegral z+0.5)/fromIntegral n)] :: Array Int (Float,Float,Float)
 build :: forall s. ST s (UArray Int Float)
 build=do
  a<-newArray (0,count-1) 0 :: ST s(STUArray s Int Float)
  b<-newArray (0,count-1) 0 :: ST s(STUArray s Int Float)
  forM_ [0..n*n*nz-1] $ \i->do
   let (mask,_,_)=inputs Data.Array.! i
   writeArray a (2*i) (1-0.5*mask);writeArray a (2*i+1) (0.25*mask)
  let index x y z=2*((x `mod` n)+n*((y `mod` n)+n*(z `mod` nz)))
      du=realToFrac(diffusionU c);dv=realToFrac(diffusionV c);dt=realToFrac(timeStep c)
      step source target=forM_ [0..nz-1] $ \z->forM_ [0..n-1] $ \y->forM_ [0..n-1] $ \x->do
       let at=index x y z;(_,f,k)=inputs Data.Array.! (at `div` 2)
           lap lane centre=do
            s1<-readArray source(index(x+1)y z+lane);s2<-readArray source(index(x-1)y z+lane)
            s3<-readArray source(index x(y+1)z+lane);s4<-readArray source(index x(y-1)z+lane)
            if dims==2 then pure (((s1+s2)+s3+s4)/4-centre) else do
             s5<-readArray source(index x y(z+1)+lane);s6<-readArray source(index x y(z-1)+lane)
             pure (((((s1+s2)+s3)+s4)+s5+s6)/6-centre)
       u<-readArray source at;v<-readArray source(at+1);lu<-lap 0 u;lv<-lap 1 v
       let reaction=(u*v)*v;bounded v'=max 0(min 1 v')
       writeArray target at (bounded(u+dt*((du*lu-reaction)+f*(1-u))))
       writeArray target(at+1)(bounded(v+dt*((dv*lv+reaction)-(f+k)*v)))
      loop 0 source _=freeze source
      loop remaining source target=step source target >> loop(remaining-1) target source
  loop(iterations c)a b
{-# NOINLINE fieldVolumes #-}
fieldVolumes :: MVar [(String,UArray Int Float)]
fieldVolumes=unsafePerformIO(newMVar [])
{-# NOINLINE cachedFields #-}
cachedFields :: String -> Int -> Config -> (Vec3->Double) -> (Vec3->Double) -> (Vec3->Double) -> UArray Int Float
cachedFields key dims c a b d=unsafePerformIO $ do
 -- Evaluate outside the cache lock: a field can itself sample a prepared volume.
 entries<-readMVar fieldVolumes
 case lookup key entries of
  Just v->pure v
  Nothing->do
   v<-evaluate(simulateFields dims c a b d)
   modifyMVar fieldVolumes $ \current->case lookup key current of
    Just existing->pure(current,existing)
    Nothing->pure(take 4((key,v):current),v)
sampleFields :: UArray Int Float -> Int -> Int -> Int -> Vec3 -> Double
sampleFields volume n dims lane p@(x,y,_)
 | dims==3=sample volume n lane p
 | otherwise=
  let coord v=let q=(v-fromIntegral(floor v::Int))*fromIntegral n-0.5;i=floor q in(i,q-fromIntegral i)
      (ix,tx)=coord x;(iy,ty)=coord y
      at a b=realToFrac(volume ! (2*((a `mod` n)+n*(b `mod` n))+lane))
      lerp t a b=a+(b-a)*t
  in lerp ty (lerp tx(at ix iy)(at(ix+1)iy)) (lerp tx(at ix(iy+1))(at(ix+1)(iy+1)))
