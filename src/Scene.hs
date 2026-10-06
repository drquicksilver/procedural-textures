-- | Perspective sphere tracing with object-space materials, and planar slices.
module Scene
  ( Camera(..), View(..), SliceAxis(..), defaultCamera, defaultView
  , renderView, viewImageFn, traceRay, normalAt, cameraRay, slicePoint
  ) where

import Codec.Picture (Image, PixelRGBA8)
import Geometry
import Render (ImageFn, renderImage)
import Texture (Texture, textureToField)
import Vector3

data Camera = Camera { cameraYaw :: Double, cameraPitch :: Double, cameraDistance :: Double }
  deriving (Eq, Show)
data SliceAxis = XY | XZ | YZ deriving (Eq, Show, Enum, Bounded)
data View = Scene Shape Camera | Slice SliceAxis Double deriving (Eq, Show)

defaultCamera :: Camera
defaultCamera = Camera 0.55 0.35 2.1

defaultView :: View
defaultView = Scene BittenCube defaultCamera

-- | Slices are unlit field cross-sections: comparing z=0 with legacy images
-- remains useful, while changing the plane reveals genuine volume.
slicePoint :: SliceAxis -> Double -> Double -> Double -> Vec3
slicePoint axis position u v = case axis of
  XY -> (u,v,position)
  XZ -> (u,position,v)
  YZ -> (position,u,v)

renderView :: Int -> View -> Texture -> Image PixelRGBA8
renderView size view texture = renderImage size size (viewImageFn view texture)

viewImageFn :: View -> Texture -> ImageFn
viewImageFn view texture =
  let material = textureToField texture
  in case view of
    Slice axis position -> \u v -> let (x,y,z) = slicePoint axis position u v in material x y z
    Scene shape camera ->
      let solid = shapeSolid shape
          ray = cameraRay camera
          light = normalise (-0.6,-0.8,-1)
      in \u v ->
        let (origin,direction) = ray u v
            background = (0.055,0.075,0.11,1)
        in case traceRay solid origin direction of
             Nothing -> background
             Just point ->
               let normal = normalAt solid point
                   illumination = 0.3 + 0.7 * max 0 (dot normal light)
                   (x,y,z) = point
                   (r,g,b,a) = material x y z
                   (br,bg,bb,_) = background
               in (r*illumination*a+br*(1-a),g*illumination*a+bg*(1-a),b*illumination*a+bb*(1-a),1)

-- | Camera always looks at the object's fixed centre; orbiting moves only the
-- camera, never the material coordinates. A fixed 40-degree vertical FOV.
cameraRay :: Camera -> Double -> Double -> (Vec3, Vec3)
cameraRay camera =
  let yaw = cameraYaw camera
      pitch = max (-1.45) (min 1.45 (cameraPitch camera))
      radius = max 1.1 (min 6 (cameraDistance camera))
      centre = (0.5,0.5,0.5)
      origin = add centre (mul radius (sin yaw*cos pitch,-sin pitch,-cos yaw*cos pitch))
      forward = normalise (sub centre origin)
      right = normalise (cross forward (0,-1,0))
      down = cross forward right
      scale = tan (20*pi/180)
  in \u v -> (origin,normalise (add forward (add (mul ((2*u-1)*scale) right) (mul ((2*v-1)*scale) down))))

-- | Trace only inside the common bounding sphere. Distances are in object
-- units; bounded steps guarantee pathological silhouettes cannot hang.
traceRay :: SDF -> Vec3 -> Vec3 -> Maybe Vec3
traceRay solid origin direction =
  let q = sub origin (0.5,0.5,0.5)
      b = dot q direction
      discriminant = b*b-dot q q+0.75*0.75
  in if discriminant < 0 then Nothing else
       let root = sqrt discriminant
           start = max 0 (-b-root)
           end = -b+root
           go steps t
             | steps >= (128::Int) || t > end = Nothing
             | otherwise =
                 let p = add origin (mul t direction)
                     d = distance solid p
                 in if abs d < 0.0005 then Just p
                    else go (steps+1) (t+max 0.0001 (abs d * 0.9))
       in go 0 start

normalAt :: SDF -> Vec3 -> Vec3
normalAt solid p =
  let h = 0.0001
      derivative offset = distance solid (add p offset)-distance solid (sub p offset)
  in normalise (derivative (h,0,0),derivative (0,h,0),derivative (0,0,h))
