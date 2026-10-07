module Texture
  ( Texture(..)
  , Scalar(..), Vector(..), Domain(..), ColourField(..), Arithmetic(..), SdfOperation(..), BlendMode(..), blendColour
  , lowerTexture, colourField, scalarField, vectorField, domainField
  , NoiseStyle(..)
  , textureToImageFn
  , textureToField
  , fbm3Fn
  , fbmFn
  ) where

import qualified Reaction as R
import qualified Cellular as C
import qualified Geometry as G
import Data.List (nub)
import ColourRamps (ColourRamp, RampMode, compileRamp)
import Colours (Colour)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Perlin (perlin3)
import Vector3 (Vec3, sub, dot, mul, norm, normalise)
import Render (ImageFn)

data Texture
  = Flat Colour
  | Linear Vec3 Vec3 RampMode ColourRamp
  | Radial Vec3 Vec3 RampMode ColourRamp
  | Circular Vec3 Double RampMode ColourRamp
  | Perlin Vec3 RampMode ColourRamp
  | Fbm Vec3 Int Double Double NoiseStyle RampMode ColourRamp
  -- ^ Scale, octaves, persistence, lacunarity, style, and the ramp's mode and ramp.
  | Turbulence Double Int Double Double Texture
  | Tiled Int Int Int Texture Texture
  | Layer Texture Texture
  | BlendTexture BlendMode Double Texture Texture
  | Colourise Scalar RampMode ColourRamp
  | InDomain Domain Texture
  | Mix Scalar Texture Texture
  | VectorColour Vector
  deriving (Eq, Show)

-- | Typed, composable core. Texture keeps the readable compatibility spellings.
data BlendMode = NormalBlend | MultiplyBlend | ScreenBlend | OverlayBlend | SoftLightBlend | DarkenBlend | LightenBlend | DifferenceBlend | ExclusionBlend deriving (Eq, Show, Enum, Bounded)
data Arithmetic = Add | Multiply | Minimum | Maximum deriving (Eq, Show)
data SdfOperation = SdfUnion | SdfIntersection | SdfDifference deriving (Eq, Show)
data Scalar
  = Constant Double
  | Planar Vec3 Vec3
  | Distance Vec3 Double
  | Angular Vec3 Vec3
  | Noise
  | Fractal Int Double Double NoiseStyle Scalar
  | AbsoluteFractal Int Double Double Scalar
  | ScalarDomain Domain Scalar
  | Arithmetic Arithmetic Scalar Scalar
  | Remap Double Double Double Double Scalar
  | ReactionField R.Config R.Chemical
  | Worley Int Double Int C.Metric C.Output
  | CellValue Int Double Int
  | CellEdge Int Double Int
  | SdfSphere Vec3 Double
  | SdfBox Vec3 Vec3
  | SdfCylinder Vec3 Double Double
  | SdfTorus Vec3 Double Double
  | SdfPlane Vec3 Double
  | SdfCombine SdfOperation Double Scalar Scalar
  | Threshold Double Double Scalar
  deriving (Eq, Show)
data Vector
  = CellIdentity Int Double Int
  | CellColour Int Double Int
  | VectorConstant Vec3
  | Position
  | Components Scalar Scalar Scalar
  | VectorAdd Vector Vector
  | VectorScale Scalar Vector
  | VectorDomain Domain Vector
  deriving (Eq, Show)
data Domain
  = Translate Vec3
  | Scale Vec3
  | Rotate Vec3
  | Repeat Vec3
  | MirrorDomain Vec3 Vec3
  | PolarRepeat Vec3 Int
  | RadialRepeat Vec3 Double
  | Twist Vec3 Double
  | Bend Vec3 Double
  | Compose Domain Domain
  | Warp Double Vector
  deriving (Eq, Show)
data ColourField
  = Solid Colour
  | Mapped Scalar RampMode ColourRamp
  | DomainColour Domain ColourField
  | Checker Int Int Int ColourField ColourField
  | Over ColourField ColourField
  | Blended BlendMode Double ColourField ColourField
  | Masked Scalar ColourField ColourField
  | VectorMapped Vector
  deriving (Eq, Show)

lowerTexture :: Texture -> ColourField
lowerTexture texture = case texture of
  Flat c -> Solid c
  Linear a b mode ramp -> Mapped (Planar a b) mode ramp
  Circular c r mode ramp -> Mapped (Distance c r) mode ramp
  Radial c axis mode ramp -> Mapped (Angular c axis) mode ramp
  Perlin scale mode ramp -> Mapped (ScalarDomain (Scale (inverse scale)) Noise) mode ramp
  Fbm scale octaves persistence lacunarity style mode ramp ->
    Mapped (ScalarDomain (Scale (inverse scale)) (Fractal octaves persistence lacunarity style Noise)) mode ramp
  Turbulence amount octaves persistence lacunarity base ->
    let source = AbsoluteFractal octaves persistence lacunarity Noise
        component offset = Arithmetic Add (ScalarDomain (Translate (mul (-1) offset)) source) (Constant (-0.5))
        v = Components (component (0,0,0)) (component (19.1,7.7,3.3)) (component (5.2,13.8,29.6))
    in DomainColour (Warp amount v) (lowerTexture base)
  Tiled c r d a b -> Checker c r d (lowerTexture a) (lowerTexture b)
  BlendTexture mode opacity a b -> Blended mode opacity (lowerTexture a) (lowerTexture b)
  Layer a b -> Over (lowerTexture a) (lowerTexture b)
  Colourise field mode ramp -> Mapped field mode ramp
  InDomain domain base -> DomainColour domain (lowerTexture base)
  Mix mask a b -> Masked mask (lowerTexture a) (lowerTexture b)
  VectorColour vector -> VectorMapped vector
  where inverse (x,y,z) = (recip x,recip y,recip z)

-- | How each octave of multi-octave noise is shaped before summing.
data NoiseStyle
  = Smooth
  -- ^ Plain noise: soft, rolling.
  | Billowy
  -- ^ Absolute value: puffy, with sharp creases at the low points.
  | Ridged
  -- ^ Inverted absolute value, squared: sharp ridges, like mountains or veins.
  deriving (Eq, Show)

textureToImageFn :: Texture -> ImageFn
textureToImageFn texture =
  let field = textureToField texture
  in \x y -> field x y 0

textureToField :: Texture -> Double -> Double -> Double -> Colour
textureToField = colourField . lowerTexture

-- Shared vector samples are lazy, scoped to one coordinate domain. An opaque
-- top never demands the hidden sample; a domain application starts a new scope.
colourField :: ColourField -> Double -> Double -> Double -> Colour
colourField field =
  let vectors f = case f of
        Over a b -> vectors a <> vectors b
        Blended _ _ a b -> vectors a <> vectors b
        DomainColour (Warp _ v) _ -> [v]
        _ -> []
      keys = vectors field
      repeated = [v | v <- nub keys, length (filter (==v) keys) > 1]
      compiledVectors = [(v, vectorField v) | v <- repeated]
      samples p = [(v, sample p) | (v, sample) <- compiledVectors]
      compile f = case f of
        VectorMapped v -> let vf=vectorField v in \p _ -> let (x,y,z)=vf p in (clamp01 (0.5+0.5*x),clamp01 (0.5+0.5*y),clamp01 (0.5+0.5*z),1)
        Solid c -> \_ _ -> c
        Mapped scalar mode ramp -> let sf = scalarField scalar; rf = compileRamp mode ramp in \p _ -> rf (sf p)
        DomainColour domain base ->
          let bf = colourField base; df = domainField domain
          in \p cache -> let q = case domain of
                               Warp amount v -> case lookup v cache of
                                 Just displacement -> addVec p (mul amount displacement)
                                 Nothing -> df p
                               _ -> df p
                         in uncurry3 bf q
        Checker c r d a b ->
          let af = compile a; bf = compile b
          in \p@(x,y,z) cache -> if (floor (x * fromIntegral (max 1 c)) + floor (y * fromIntegral (max 1 r)) + floor (z * fromIntegral (max 1 d)) :: Int) `mod` 2 == 0 then af p cache else bf p cache
        Over a b -> let af = compile a; bf = compile b in \p cache -> blend (af p cache) (bf p cache)
        Blended mode opacity a b -> let af=compile a; bf=compile b in \p cache -> blendColour mode opacity (af p cache) (bf p cache)
        Masked mask a b ->
          let mf = scalarField mask; af = compile a; bf = compile b
          in \p cache -> let t = clamp01 (mf p) in if t == 0 then bf p cache else if t == 1 then af p cache else mixColour t (bf p cache) (af p cache)
      fn = compile field
  in \x y z -> let p=(x,y,z) in fn p (samples p)

scalarField :: Scalar -> Vec3 -> Double
scalarField field = case field of
  Constant value -> const value
  Planar from to ->
    let direction = sub to from; len2 = dot direction direction
    in \p -> if len2 <= 0 then 0 else dot (sub p from) direction / len2
  Distance centre radius -> \p -> if radius <= 0 then 0 else norm (sub p centre) / radius
  Angular centre axis ->
    let unit = normalise axis
        project v = sub v (mul (dot v unit) unit)
        candidate = project (0,-1,0)
        north = normalise (if norm candidate < 1e-9 then project (0,0,1) else candidate)
    in \p -> let radial = project (sub p centre); len = norm radial in if len <= 0 then 0.5 else (1-dot north radial/len)/2
  ReactionField c chemical -> let volume=R.cachedVolume c in R.sample volume (R.resolution c) (if chemical==R.U then 0 else 1)
  Worley dims jitter seed m output -> C.sample dims jitter seed m output
  CellValue dims jitter seed -> C.value dims jitter seed
  CellEdge dims jitter seed -> C.edge dims jitter seed
  SdfSphere c r -> G.distance (G.Sphere c r)
  SdfBox c h -> G.distance (G.Box c h)
  SdfCylinder c r h -> G.distance (G.Cylinder c r h)
  SdfTorus c r t -> G.distance (G.Torus c r t)
  SdfPlane n o -> G.distance (G.Plane n o)
  SdfCombine op k a b ->
    let af=scalarField a; bf=scalarField b
        combine = case op of
          SdfUnion -> G.smoothMin k
          SdfIntersection -> \x y -> negate (G.smoothMin k (-x) (-y))
          SdfDifference -> \x y -> negate (G.smoothMin k (-x) y)
    in \p -> combine (af p) (bf p)
  Noise -> uncurry3 perlin3
  ScalarDomain domain source -> let df=domainField domain; sf=scalarField source in sf . df
  Fractal octaves persistence lacunarity style source -> fractalField False octaves persistence lacunarity style source
  AbsoluteFractal octaves persistence lacunarity source -> fractalField True octaves persistence lacunarity Smooth source
  Arithmetic op a b ->
    let af=scalarField a; bf=scalarField b; fn=case op of Add -> (+); Multiply -> (*); Minimum -> min; Maximum -> max
    in \p -> fn (af p) (bf p)
  Remap lo hi outLo outHi source ->
    let sf=scalarField source in \p -> if hi == lo then outLo else outLo+(sf p-lo)/(hi-lo)*(outHi-outLo)
  Threshold lo hi source ->
    let sf=scalarField source in \p -> let v=sf p; t=if hi == lo then (if v < lo then 0 else 1) else clamp01 ((v-lo)/(hi-lo)) in t*t*(3-2*t)

vectorField :: Vector -> Vec3 -> Vec3
vectorField field = case field of
  CellIdentity dims jitter seed -> C.identity dims jitter seed
  CellColour dims jitter seed -> C.colour dims jitter seed
  VectorConstant v -> const v
  Position -> id
  Components x y z -> let xf=scalarField x; yf=scalarField y; zf=scalarField z in \p -> (xf p,yf p,zf p)
  VectorAdd a b -> let af=vectorField a; bf=vectorField b in \p -> addVec (af p) (bf p)
  VectorScale scalar vector -> let sf=scalarField scalar; vf=vectorField vector in \p -> mul (sf p) (vf p)
  VectorDomain domain vector -> let df=domainField domain; vf=vectorField vector in vf . df

domainField :: Domain -> Vec3 -> Vec3
domainField domain = case domain of
  Translate offset -> \p -> sub p offset
  Scale (sx,sy,sz) -> \(x,y,z) -> (divide x sx,divide y sy,divide z sz)
  Rotate (x,y,z) -> rotateX (-x) . rotateY (-y) . rotateZ (-z)
  Repeat (sx,sy,sz) -> \(x,y,z) -> (repeatAxis sx x,repeatAxis sy y,repeatAxis sz z)
  MirrorDomain centre (ax,ay,az) -> \p -> let (x,y,z)=sub p centre in addVec centre (if ax >= 0.5 then abs x else x,if ay >= 0.5 then abs y else y,if az >= 0.5 then abs z else z)
  PolarRepeat centre count -> \p ->
    let (x,y,z)=sub p centre; radius=sqrt (x*x+y*y)
        sector=2*pi/fromIntegral (max 1 count)
        angle=repeatAxis sector (if radius == 0 then 0 else atan2 y x)
    in addVec centre (radius*cos angle,radius*sin angle,z)
  RadialRepeat centre period -> \p ->
    let (x,y,z)=sub p centre; radius=sqrt (x*x+y*y)
        wrapped=if period <= 0 then radius else radius-fromIntegral (floor (radius/period) :: Integer)*period
        scale=if radius == 0 then 0 else wrapped/radius
    in addVec centre (x*scale,y*scale,z)
  Twist centre amount -> \p -> let q@(_,_,z)=sub p centre in addVec centre (rotateZ (-amount*z) q)
  Bend centre amount -> \p -> let q@(x,_,_)=sub p centre in addVec centre (rotateZ (-amount*x) q)
  Compose first second -> let a=domainField first; b=domainField second in b . a
  Warp amount field -> let vf=vectorField field in \p -> addVec p (mul amount (vf p))
  where divide x scale = if scale == 0 then 0 else x/scale

fractalField :: Bool -> Int -> Double -> Double -> NoiseStyle -> Scalar -> Vec3 -> Double
fractalField absolute octaves persistence lacunarity style source =
  let count=max 1 octaves
      total=if absolute then (if persistence == 1 then fromIntegral count else (1-persistence ** fromIntegral count)/(1-persistence)) else sum (take count (iterate (*persistence) 1))
      transforms=octaveTransforms count lacunarity
      sf=scalarField source
      shape n | absolute = abs (2*n-1)
              | otherwise = case style of Smooth -> n; Billowy -> abs (2*n-1); Ridged -> let r=1-abs (2*n-1) in r*r
  in \(x,y,z) ->
    let go i amp acc | i >= count = acc
                     | otherwise = let q=transformOctave transforms i x y z
                                       offset=if absolute then (0,0,0) else mul (fromIntegral i) (31.7,17.3,11.9)
                                   in go (i+1) (amp*persistence) (acc+amp*shape (sf (addVec q offset)))
        value=if total <= 0 then (if absolute then 0 else 0.5) else go 0 1 0/total
    in if absolute then value else clamp01 (spread style value)

addVec :: Vec3 -> Vec3 -> Vec3
addVec (x,y,z) (a,b,c) = (x+a,y+b,z+c)
uncurry3 :: (Double -> Double -> Double -> a) -> Vec3 -> a
uncurry3 fn (x,y,z) = fn x y z
-- Mix straight RGB and alpha by an explicit scalar mask, independently of over.
mixColour :: Double -> Colour -> Colour -> Colour
mixColour t (r,g,b,a) (x,y,z,w) = (lerp t r x,lerp t g y,lerp t b z,lerp t a w)

blend :: Colour -> Colour -> Colour
blend top@(_, _, _, a1) bottom
  | a1 == 1.0 = top
  | otherwise = blendGeneral top bottom

blendGeneral :: Colour -> Colour -> Colour
blendGeneral (r1, g1, b1, a1) (r2, g2, b2, a2) =
  let a = a1 + a2 * (1.0 - a1)
      weightTop =
        if a <= 0.0
          then 0.0
          else a1 / a
      weightBottom = 1.0 - weightTop
  in ( lerp weightBottom r1 r2
     , lerp weightBottom g1 g2
     , lerp weightBottom b1 b2
     , a
     )

lerp :: Double -> Double -> Double -> Double
lerp t a b =
  a + (b - a) * t

-- | Multi-octave ("fractal Brownian motion") noise in [0, 1]. Octave @i@
-- samples Perlin noise at @lacunarity^i@ times the base frequency, weighted
-- by @persistence^i@, offset so that octaves do not line up at the noise
-- lattice points. Each style's sum is then stretched to use most of [0, 1]
-- (see 'spread') and clamped, so ramps designed for [0, 1] fit it.
fbmFn :: (Double, Double) -> Int -> Double -> Double -> NoiseStyle -> Double -> Double -> Double
fbmFn (sx,sy) octaves persistence lacunarity style =
  let f = fbm3Fn (sx,sy,sqrt (abs (sx*sy))) octaves persistence lacunarity style
  in \x y -> f x y 0

fbm3Fn :: Vec3 -> Int -> Double -> Double -> NoiseStyle -> Double -> Double -> Double -> Double
fbm3Fn (sx, sy, sz) octaves persistence lacunarity style =
  let safeOctaves = max 1 octaves
      weights = take safeOctaves (iterate (* persistence) 1.0)
      total = sum weights
      shape n =
        case style of
          Smooth -> n
          Billowy -> abs (2.0 * n - 1.0)
          Ridged -> let r = 1.0 - abs (2.0 * n - 1.0) in r * r
      octaves' = octaveTransforms safeOctaves lacunarity
  in \x y z ->
      let sxx = x * sx
          syy = y * sy
          szz = z * sz
          go :: Int -> Double -> Double -> Double
          go i amp acc
            | i >= safeOctaves = acc
            | otherwise =
                let offset = fromIntegral i
                    (rx, ry, rz) = transformOctave octaves' i sxx syy szz
                    n = perlin3 (rx + 31.7 * offset) (ry + 17.3 * offset) (rz + 11.9 * offset)
                in go (i + 1) (amp * persistence) (acc + amp * shape n)
          value = if total <= 0.0 then 0.5 else go 0 1.0 0.0 / total
      in clamp01 (spread style value)

-- | Explicit artistic contrast mappings for material ramps. These are not
-- probability normalisations; the raw 3D distribution is recorded by
-- bench/NoiseStudy.hs and the Phase 2 decision log.
spread :: NoiseStyle -> Double -> Double
spread style value =
  case style of
    Smooth -> 0.5 + (value - 0.5) * 2.0
    Billowy -> value * 1.75
    Ridged -> (value - 0.2) / 0.72

-- | Each octave's frequency and rotation as a 3x3 matrix, stored as
-- consecutive columns scaled by @lacunarity^i@: octave @i@ samples
-- the noise @lacunarity^i@ times finer, turned by @i@ times 'octaveRotation'
-- so the lattices of successive octaves don't line up with each other.
-- Computed once per texture node rather than per pixel.
octaveTransforms :: Int -> Double -> UArray Int Double
octaveTransforms octaves lacunarity =
  listArray (0, 9 * octaves - 1) (concatMap matrix [0..octaves-1])
  where
    matrix i =
      let a = fromIntegral i * octaveRotation
          f = lacunarity ^ i
          rotate x y z =
            let u = x*cos a-y*sin a
                v = x*sin a+y*cos a
                w = v*cos (a*0.71)-z*sin (a*0.71)
                q = v*sin (a*0.71)+z*cos (a*0.71)
            in [f*(u*cos (a*0.53)+q*sin (a*0.53)), f*w, f*(q*cos (a*0.53)-u*sin (a*0.53))]
      in rotate 1 0 0 <> rotate 0 1 0 <> rotate 0 0 1

{-# INLINE transformOctave #-}
transformOctave :: UArray Int Double -> Int -> Double -> Double -> Double -> Vec3
transformOctave transforms i x y z =
  let at n = transforms `unsafeAt` (9*i+n)
  in (x*at 0+y*at 3+z*at 6, x*at 1+y*at 4+z*at 7, x*at 2+y*at 5+z*at 8)

-- | Radians between successive octaves (about 47.6 degrees).
octaveRotation :: Double
octaveRotation = 0.83

clamp01 :: Double -> Double
clamp01 v = max 0.0 (min 1.0 v)

-- Degrees are editor-friendly. Inverse Euler rotation undoes z, y, then x.
rotateX, rotateY, rotateZ :: Double -> Vec3 -> Vec3
rotateX degrees (x,y,z) = let a=degrees*pi/180 in (x,y*cos a-z*sin a,y*sin a+z*cos a)
rotateY degrees (x,y,z) = let a=degrees*pi/180 in (x*cos a+z*sin a,y,z*cos a-x*sin a)
rotateZ degrees (x,y,z) = let a=degrees*pi/180 in (x*cos a-y*sin a,x*sin a+y*cos a,z)
repeatAxis :: Double -> Double -> Double
repeatAxis period x | period <= 0 = x
                    | otherwise = x-period*fromIntegral (floor (x/period+0.5) :: Integer)

-- | Separable RGB blend with source-over alpha, following W3C compositing.
-- New blends clamp input channels/alpha; the legacy Layer convention is unchanged.
blendColour :: BlendMode -> Double -> Colour -> Colour -> Colour
blendColour mode opacity top bottom =
  let (sr,sg,sb,sa)=top; (br,bg,bb,ba)=bottom
      a=clamp01 opacity * clamp01 sa; b=clamp01 ba; alpha=a+b*(1-a)
      channel source backdrop =
        let s=clamp01 source; d=clamp01 backdrop
            mixed=case mode of
              NormalBlend -> s
              MultiplyBlend -> s*d
              ScreenBlend -> s+d-s*d
              OverlayBlend -> if d<=0.5 then 2*s*d else 1-2*(1-s)*(1-d)
              SoftLightBlend -> if s<=0.5 then d-(1-2*s)*d*(1-d) else d+(2*s-1)*((if d<=0.25 then ((16*d-12)*d+4)*d else sqrt d)-d)
              DarkenBlend -> min s d
              LightenBlend -> max s d
              DifferenceBlend -> abs (d-s)
              ExclusionBlend -> d+s-2*d*s
        in if alpha<=0 then 0 else ((1-a)*b*d+(1-b)*a*s+a*b*mixed)/alpha
  in if clamp01 opacity==0 then let (_,_,_,ab)=bottom in if clamp01 ab==0 then (0,0,0,0) else (clamp01 br,clamp01 bg,clamp01 bb,clamp01 ab)
     else (channel sr br,channel sg bg,channel sb bb,alpha)
