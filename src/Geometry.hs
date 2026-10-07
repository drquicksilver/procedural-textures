-- | Exact signed distances and boolean solids, shared with future field nodes.
module Geometry
  ( SDF(..), Profile(..), Vec2, distance, profileDistance, smoothMin
  , Shape(..), shapes, shapeName, shapeSolid
  ) where

import Vector3

type Vec2 = (Double, Double)

data SDF
  = Sphere Vec3 Double
  | Box Vec3 Vec3
  | Cylinder Vec3 Double Double
  | Torus Vec3 Double Double
  | Plane Vec3 Double
  | Union SDF SDF
  | Intersection SDF SDF
  | Difference SDF SDF
  | Blend Double SDF SDF
  -- ^ Smooth union with blend radius k: a fillet where the solids meet.
  | Rounded Double SDF
  -- ^ Grows the solid by a radius, rounding its convex edges.
  | Revolve Vec3 Profile
  -- ^ Lathe: the profile's (radius, height) plane swept around the vertical
  -- axis through the point. Height is measured up, towards smaller y.
  | Extrude Vec3 Double Profile
  -- ^ The profile's (x, height) plane, relative to the point, extruded
  -- along z by the given half-depth.
  | Turn Vec3 Double SDF
  -- ^ Rotated about the vertical axis through the point by an angle in
  -- radians, anticlockwise from +x towards +z.
  | RadialRepeat Vec3 Int SDF
  -- ^ n copies around the vertical axis through the point. The child is
  -- drawn once near the +x direction and must fit inside its sector.
  | Scaled Vec3 Double SDF
  -- ^ Uniformly scaled about the point.
  | Bounded SDF SDF
  -- ^ A cheap solid enclosing an expensive one: far from the bound its
  -- distance stands in, so the expensive solid is only evaluated nearby.
  deriving (Eq, Show)

-- | Two-dimensional solids for 'Revolve' and 'Extrude'.
data Profile
  = Disc Vec2 Double
  | Rect Vec2 Vec2 Double
  -- ^ Centre, half extents and corner radius.
  | Polygon [Vec2]
  | ProfileUnion Profile Profile
  | ProfileBlend Double Profile Profile
  | ProfileDifference Profile Profile
  deriving (Eq, Show)

distance :: SDF -> Vec3 -> Double
distance solid p = case solid of
  Sphere c radius -> norm (sub p c)-radius
  Box c (hx,hy,hz) ->
    let (x,y,z) = sub p c
        q = (abs x-hx, abs y-hy, abs z-hz)
        (a,b,d) = q
    in norm (max 0 a,max 0 b,max 0 d) + min 0 (max a (max b d))
  Cylinder c radius halfHeight ->
    let (x,y,z) = sub p c
        a = sqrt (x*x+z*z)-radius
        b = abs y-halfHeight
    in sqrt (max 0 a ^ (2::Int)+max 0 b ^ (2::Int)) + min 0 (max a b)
  Torus c major minor ->
    let (x,y,z) = sub p c
        q = sqrt (x*x+z*z)-major
    in sqrt (q*q+y*y)-minor
  Plane n offset -> dot (normalise n) p-offset
  Union a b -> min (distance a p) (distance b p)
  Intersection a b -> max (distance a p) (distance b p)
  Difference a b -> max (distance a p) (negate (distance b p))
  Blend k a b -> smoothMin k (distance a p) (distance b p)
  Rounded r a -> distance a p-r
  Revolve c profile ->
    let (x,y,z) = sub p c
    in profileDistance profile (sqrt (x*x+z*z),negate y)
  Extrude c halfDepth profile ->
    let (x,y,z) = sub p c
    in extrude (profileDistance profile (x,negate y)) (abs z-halfDepth)
  Turn c angle a ->
    let (x,y,z) = sub p c
        (co,si) = (cos angle,sin angle)
    in distance a (add c (co*x+si*z,y,co*z-si*x))
  RadialRepeat c n a ->
    let (x,y,z) = sub p c
        sector = 2*pi/fromIntegral n
        angle = atan2 z x
        folded = angle-sector*fromIntegral (round (angle/sector) :: Int)
        r = sqrt (x*x+z*z)
    in distance a (add c (r*cos folded,y,r*sin folded))
  Scaled c k a -> k*distance a (add c (mul (1/k) (sub p c)))
  Bounded bound a ->
    let d = distance bound p
    in if d > 0.02 then d else distance a p

profileDistance :: Profile -> Vec2 -> Double
profileDistance profile p@(px,py) = case profile of
  Disc (cx,cy) radius -> sqrt ((px-cx)^(2::Int)+(py-cy)^(2::Int))-radius
  Rect (cx,cy) (hx,hy) radius ->
    let a = abs (px-cx)-hx+radius
        b = abs (py-cy)-hy+radius
    in sqrt (max 0 a ^ (2::Int)+max 0 b ^ (2::Int)) + min 0 (max a b)-radius
  Polygon vertices -> polygonDistance vertices p
  ProfileUnion a b -> min (profileDistance a p) (profileDistance b p)
  ProfileBlend k a b -> smoothMin k (profileDistance a p) (profileDistance b p)
  ProfileDifference a b -> max (profileDistance a p) (negate (profileDistance b p))

-- | Exact distance to a closed polygon, signed by the crossing number.
polygonDistance :: [Vec2] -> Vec2 -> Double
polygonDistance [] _ = 1/0
polygonDistance vertices (px,py) =
  let edges = zip vertices (last vertices : vertices)
      step (best,inside) ((ax,ay),(bx,by)) =
        let (ex,ey) = (bx-ax,by-ay)
            (wx,wy) = (px-ax,py-ay)
            t = max 0 (min 1 ((wx*ex+wy*ey)/(ex*ex+ey*ey)))
            (dx,dy) = (wx-ex*t,wy-ey*t)
            conditions = [py >= ay, py < by, ex*wy > ey*wx]
            flips = and conditions || not (or conditions)
        in (min best (dx*dx+dy*dy), if flips then not inside else inside)
      (squared,isInside) = foldl step (1/0,False) edges
  in (if isInside then negate else id) (sqrt squared)

extrude :: Double -> Double -> Double
extrude a b = min 0 (max a b) + sqrt (max 0 a ^ (2::Int)+max 0 b ^ (2::Int))

-- | Polynomial smooth minimum: never above 'min', so tracing stays safe.
smoothMin :: Double -> Double -> Double -> Double
smoothMin k a b | k <= 0 = min a b
smoothMin k a b =
  let h = max 0 (k-abs (a-b))/k
  in min a b-h*h*k/4

data Shape
  = Ball | Cube | Tube | Ring | BittenCube | CutSphere | CutCube
  | Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Show, Enum, Bounded)

shapes :: [Shape]
shapes = [minBound..maxBound]

shapeName :: Shape -> String
shapeName s = case s of
  Ball -> "sphere"
  Cube -> "cube"
  Tube -> "cylinder"
  Ring -> "torus"
  BittenCube -> "bitten-cube"
  CutSphere -> "cut-sphere"
  CutCube -> "cut-cube"
  Pawn -> "pawn"
  Rook -> "rook"
  Knight -> "knight"
  Bishop -> "bishop"
  Queen -> "queen"
  King -> "king"

shapeSolid :: Shape -> SDF
shapeSolid s = case s of
  Ball -> Sphere centre 0.43
  Cube -> cube
  Tube -> Cylinder centre 0.35 0.4
  Ring -> Torus centre 0.29 0.13
  BittenCube -> Difference cube (Sphere (0.83,0.18,0.08) 0.43)
  CutSphere -> Difference (Sphere centre 0.43) (Box (1,0,0) (0.5,0.5,0.5))
  CutCube -> Intersection cube (Plane (0.7,-0.2,-1) 0.05)
  Pawn -> piece Pawn pawn
  Rook -> piece Rook rook
  Knight -> piece Knight knight
  Bishop -> piece Bishop bishop
  Queen -> piece Queen queen
  King -> piece King king
  where
    centre = (0.5,0.5,0.5)
    cube = Box centre (0.38,0.38,0.38)

-- Chess pieces ---------------------------------------------------------------
--
-- Classic Staunton pieces on one scale. They are modelled with the king just
-- under one unit tall and foot radii of 0.16 (pawn), 0.18 (rook, knight,
-- bishop) and 0.2 (queen, king), then all enlarged by 'pieceScale'. Each piece
-- is centred vertically on the object centre; profiles are written as
-- (radius, height above the floor).

-- | Height of each piece, used to stand it so that it is vertically centred.
pieceHeight :: Shape -> Double
pieceHeight s = case s of
  Pawn -> 0.5
  Rook -> 0.56
  Knight -> 0.68
  Bishop -> 0.76
  Queen -> 0.895
  _ -> 0.98

-- | Radius of each piece's foot.
pieceRadius :: Shape -> Double
pieceRadius s = case s of
  Pawn -> 0.16
  Queen -> 0.2
  King -> 0.2
  _ -> 0.18

-- | Pieces are modelled at a convenient size and then enlarged to fill the
-- view like the other shapes do.
pieceScale :: Double
pieceScale = 1.2

-- | A scaled piece inside its bounding cylinder, so rays far from it stay
-- cheap. The knight's muzzle reaches beyond its foot.
piece :: Shape -> SDF -> SDF
piece s =
  Bounded (Cylinder (0.5,0.5,0.5) (pieceScale*(pieceRadius s+margin)) (pieceScale*(pieceHeight s/2+0.01)))
    . Scaled (0.5,0.5,0.5) pieceScale
  where margin = if s == Knight then 0.03 else 0.005

-- | The point on the axis at floor level.
floorOf :: Shape -> Vec3
floorOf s = (0.5,0.5+pieceHeight s/2,0.5)

-- | A point on the axis at a height above the floor, offset in x and z.
at :: Shape -> Double -> Double -> Double -> Vec3
at s x h z = let (cx,cy,cz) = floorOf s in (cx+x,cy-h,cz+z)

-- | A rectangle from radius 0 to r between two heights.
band :: Double -> Double -> Double -> Double -> Profile
band r h0 h1 = Rect (0,(h0+h1)/2) (r,(h1-h0)/2)

-- | A tapering column from radius r0 at h0 to r1 at h1.
taper :: Double -> Double -> Double -> Double -> Profile
taper r0 h0 r1 h1 = Polygon [(0,h0),(r0,h0),(r1,h1),(0,h1)]

blends :: Double -> [Profile] -> Profile
blends k = foldr1 (ProfileBlend k)

-- | The turned foot shared by every piece: a bevelled plinth, a bead and a
-- step, ending at about 0.6 radii above the floor.
foot :: Double -> Profile
foot r = blends (0.04*r)
  [ band r 0 (0.22*r) (0.06*r)
  , band (0.86*r) (0.18*r) (0.3*r) 0
  , Disc (0.82*r,0.36*r) (0.09*r)
  , band (0.72*r) (0.3*r) (0.55*r) (0.05*r)
  ]

-- | A flattened ring around the stem below a head or crown.
collar :: Double -> Double -> Double -> Profile
collar r h thickness = band r (h-thickness/2) (h+thickness/2) (thickness/2)

lathe :: Shape -> Profile -> SDF
lathe s = Revolve (floorOf s)

pawn :: SDF
pawn = lathe Pawn $ blends 0.012
  [ ProfileBlend 0.06 (foot (pieceRadius Pawn)) (taper 0.1 0.08 0.05 0.28)
  , collar 0.1 0.29 0.028
  , Disc (0,0.395) 0.105
  ]

rook :: SDF
rook =
  let r = pieceRadius Rook
      body = blends 0.012
        [ ProfileBlend 0.07 (foot r) (taper 0.125 0.09 0.1 0.4)
        , collar 0.125 0.41 0.025
        , ProfileDifference (band 0.15 0.42 0.56 0.01) (band 0.1 0.525 0.6 0)
        ]
      crenel = Box (at Rook 0.15 0.56 0) (0.06,0.04,0.026)
  in Difference (lathe Rook body) (RadialRepeat (floorOf Rook) 6 crenel)

bishop :: SDF
bishop =
  let r = pieceRadius Bishop
      body = blends 0.012
        [ ProfileBlend 0.07 (foot r) (taper 0.12 0.09 0.05 0.45)
        , collar 0.11 0.455 0.03
        , collar 0.085 0.485 0.02
        , ProfileBlend 0.05 (Disc (0,0.585) 0.085) (Polygon [(0,0.55),(0.07,0.6),(0,0.71)])
        , Disc (0,0.735) 0.026
        ]
      -- The mitre's slit: a thin slab, diagonal across the side facing the
      -- default camera, cut to just past the axis.
      facing = normalise (0.52,0,-0.85)
      right = normalise (0.85,0,0.52)
      normal = normalise (add right (0,-1,0))
      centre = at Bishop 0 0.62 0
      slit = Intersection
        (Intersection (Plane normal (dot normal centre+0.011)) (Plane (mul (-1) normal) (0.011-dot normal centre)))
        (Plane (mul (-1) facing) (0.015-dot facing centre))
  in Difference (lathe Bishop body) slit

knight :: SDF
knight =
  let r = pieceRadius Knight
      base = lathe Knight $ blends 0.012
        [ ProfileBlend 0.05 (foot r) (taper 0.13 0.09 0.12 0.15)
        , collar 0.14 0.155 0.03
        ]
      -- Side view of the head facing -x, as (x, height above the floor).
      horse = Polygon
        [ (0.11,0.16), (0.13,0.28), (0.125,0.38), (0.1,0.47), (0.065,0.55)
        , (0.045,0.6), (0.04,0.665), (0.005,0.62), (-0.04,0.6), (-0.1,0.54)
        , (-0.15,0.47), (-0.17,0.43), (-0.165,0.38), (-0.15,0.36)
        , (-0.11,0.355), (-0.06,0.39), (-0.045,0.36), (-0.075,0.27), (-0.105,0.16)
        ]
      neck = Rounded 0.012 (Extrude (floorOf Knight) 0.056 horse)
      bothSides x h radius = Union (Sphere (at Knight x h (-0.076)) radius) (Sphere (at Knight x h 0.076) radius)
      mouth = Box (at Knight (-0.17) 0.395 0) (0.04,0.005,1)
      -- A row of rounded tufts along the back of the neck makes the mane.
      mane = foldr1 (Blend 0.01)
        [ Sphere (at Knight x h 0) 0.032
        | (x,h) <- [(0.12,0.22),(0.122,0.28),(0.118,0.34),(0.113,0.4),(0.1,0.46),(0.083,0.51),(0.062,0.56)]
        ]
      headBox = Box (at Knight (-0.01) 0.415 0) (0.185,0.275,0.075)
      carving = foldr1 Union [bothSides (-0.06) 0.53 0.017, bothSides (-0.15) 0.44 0.013, mouth]
  in Blend 0.02 base (Turn (floorOf Knight) 0.3 (Bounded headBox (Difference (Blend 0.015 neck mane) carving)))

-- | The queen and king share a foot, stem and collars below their crowns.
royalBody :: Double -> [Profile]
royalBody top =
  [ ProfileBlend 0.08 (foot (pieceRadius King)) (taper 0.135 0.1 0.06 top)
  , collar 0.125 (top+0.01) 0.03
  , collar 0.1 (top+0.04) 0.02
  ]

queen :: SDF
queen =
  let crown = ProfileDifference
        (ProfileUnion (taper 0.055 0.6 0.115 0.76) (collar 0.122 0.765 0.04))
        (Disc (0,0.84) 0.095)
      body = blends 0.012 (royalBody 0.55 <> [crown, Disc (0,0.79) 0.055, Disc (0,0.865) 0.03])
      points = RadialRepeat (floorOf Queen) 8 (Sphere (at Queen 0.135 0.81 0) 0.05)
  in Difference (lathe Queen body) points

king :: SDF
king =
  let crown = blends 0.02
        [ taper 0.055 0.62 0.11 0.77
        , collar 0.115 0.775 0.03
        , Disc (0,0.775) 0.09
        ]
      body = blends 0.012 (royalBody 0.57 <> [crown])
      crucifix = Rounded 0.006 (Union
        (Box (at King 0 0.91 0) (0.016,0.064,0.012))
        (Box (at King 0 0.92 0) (0.054,0.016,0.012)))
  in Blend 0.015 (lathe King body) crucifix
