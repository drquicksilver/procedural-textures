module Colours where

type Colour = (Double, Double, Double, Double)

rgb :: Int -> Int -> Int -> Colour
rgb r g b = (channel r, channel g, channel b, 1.0)

rgba :: Int -> Int -> Int -> Int -> Colour
rgba r g b a = (channel r, channel g, channel b, channel a)

channel :: Int -> Double
channel value = fromIntegral value / 255.0

aliceBlue :: Colour
aliceBlue = rgb 240 248 255

antiqueWhite :: Colour
antiqueWhite = rgb 250 235 215

aqua :: Colour
aqua = rgb 0 255 255

aquamarine :: Colour
aquamarine = rgb 127 255 212

azure :: Colour
azure = rgb 240 255 255

beige :: Colour
beige = rgb 245 245 220

bisque :: Colour
bisque = rgb 255 228 196

black :: Colour
black = rgb 0 0 0

blanchedAlmond :: Colour
blanchedAlmond = rgb 255 235 205

blue :: Colour
blue = rgb 0 0 255

blueViolet :: Colour
blueViolet = rgb 138 43 226

brown :: Colour
brown = rgb 165 42 42

burlyWood :: Colour
burlyWood = rgb 222 184 135

cadetBlue :: Colour
cadetBlue = rgb 95 158 160

chartreuse :: Colour
chartreuse = rgb 127 255 0

chocolate :: Colour
chocolate = rgb 210 105 30

coral :: Colour
coral = rgb 255 127 80

cornflowerBlue :: Colour
cornflowerBlue = rgb 100 149 237

cornsilk :: Colour
cornsilk = rgb 255 248 220

crimson :: Colour
crimson = rgb 220 20 60

cyan :: Colour
cyan = rgb 0 255 255

darkBlue :: Colour
darkBlue = rgb 0 0 139

darkCyan :: Colour
darkCyan = rgb 0 139 139

darkGoldenrod :: Colour
darkGoldenrod = rgb 184 134 11

darkGray :: Colour
darkGray = rgb 169 169 169

darkGrey :: Colour
darkGrey = rgb 169 169 169

darkGreen :: Colour
darkGreen = rgb 0 100 0

darkKhaki :: Colour
darkKhaki = rgb 189 183 107

darkMagenta :: Colour
darkMagenta = rgb 139 0 139

darkOliveGreen :: Colour
darkOliveGreen = rgb 85 107 47

darkOrange :: Colour
darkOrange = rgb 255 140 0

darkOrchid :: Colour
darkOrchid = rgb 153 50 204

darkRed :: Colour
darkRed = rgb 139 0 0

darkSalmon :: Colour
darkSalmon = rgb 233 150 122

darkSeaGreen :: Colour
darkSeaGreen = rgb 143 188 143

darkSlateBlue :: Colour
darkSlateBlue = rgb 72 61 139

darkSlateGray :: Colour
darkSlateGray = rgb 47 79 79

darkSlateGrey :: Colour
darkSlateGrey = rgb 47 79 79

darkTurquoise :: Colour
darkTurquoise = rgb 0 206 209

darkViolet :: Colour
darkViolet = rgb 148 0 211

deepPink :: Colour
deepPink = rgb 255 20 147

deepSkyBlue :: Colour
deepSkyBlue = rgb 0 191 255

dimGray :: Colour
dimGray = rgb 105 105 105

dimGrey :: Colour
dimGrey = rgb 105 105 105

dodgerBlue :: Colour
dodgerBlue = rgb 30 144 255

fireBrick :: Colour
fireBrick = rgb 178 34 34

floralWhite :: Colour
floralWhite = rgb 255 250 240

forestGreen :: Colour
forestGreen = rgb 34 139 34

fuchsia :: Colour
fuchsia = rgb 255 0 255

gainsboro :: Colour
gainsboro = rgb 220 220 220

ghostWhite :: Colour
ghostWhite = rgb 248 248 255

gold :: Colour
gold = rgb 255 215 0

goldenrod :: Colour
goldenrod = rgb 218 165 32

gray :: Colour
gray = rgb 128 128 128

grey :: Colour
grey = rgb 128 128 128

green :: Colour
green = rgb 0 128 0

greenYellow :: Colour
greenYellow = rgb 173 255 47

honeydew :: Colour
honeydew = rgb 240 255 240

hotPink :: Colour
hotPink = rgb 255 105 180

indianRed :: Colour
indianRed = rgb 205 92 92

indigo :: Colour
indigo = rgb 75 0 130

ivory :: Colour
ivory = rgb 255 255 240

khaki :: Colour
khaki = rgb 240 230 140

lavender :: Colour
lavender = rgb 230 230 250

lavenderBlush :: Colour
lavenderBlush = rgb 255 240 245

lawnGreen :: Colour
lawnGreen = rgb 124 252 0

lemonChiffon :: Colour
lemonChiffon = rgb 255 250 205

lightBlue :: Colour
lightBlue = rgb 173 216 230

lightCoral :: Colour
lightCoral = rgb 240 128 128

lightCyan :: Colour
lightCyan = rgb 224 255 255

lightGoldenrodYellow :: Colour
lightGoldenrodYellow = rgb 250 250 210

lightGray :: Colour
lightGray = rgb 211 211 211

lightGrey :: Colour
lightGrey = rgb 211 211 211

lightGreen :: Colour
lightGreen = rgb 144 238 144

lightPink :: Colour
lightPink = rgb 255 182 193

lightSalmon :: Colour
lightSalmon = rgb 255 160 122

lightSeaGreen :: Colour
lightSeaGreen = rgb 32 178 170

lightSkyBlue :: Colour
lightSkyBlue = rgb 135 206 250

lightSlateGray :: Colour
lightSlateGray = rgb 119 136 153

lightSlateGrey :: Colour
lightSlateGrey = rgb 119 136 153

lightSteelBlue :: Colour
lightSteelBlue = rgb 176 196 222

lightYellow :: Colour
lightYellow = rgb 255 255 224

lime :: Colour
lime = rgb 0 255 0

limeGreen :: Colour
limeGreen = rgb 50 205 50

linen :: Colour
linen = rgb 250 240 230

magenta :: Colour
magenta = rgb 255 0 255

maroon :: Colour
maroon = rgb 128 0 0

mediumAquamarine :: Colour
mediumAquamarine = rgb 102 205 170

mediumBlue :: Colour
mediumBlue = rgb 0 0 205

mediumOrchid :: Colour
mediumOrchid = rgb 186 85 211

mediumPurple :: Colour
mediumPurple = rgb 147 112 219

mediumSeaGreen :: Colour
mediumSeaGreen = rgb 60 179 113

mediumSlateBlue :: Colour
mediumSlateBlue = rgb 123 104 238

mediumSpringGreen :: Colour
mediumSpringGreen = rgb 0 250 154

mediumTurquoise :: Colour
mediumTurquoise = rgb 72 209 204

mediumVioletRed :: Colour
mediumVioletRed = rgb 199 21 133

midnightBlue :: Colour
midnightBlue = rgb 25 25 112

mintCream :: Colour
mintCream = rgb 245 255 250

mistyRose :: Colour
mistyRose = rgb 255 228 225

moccasin :: Colour
moccasin = rgb 255 228 181

navajoWhite :: Colour
navajoWhite = rgb 255 222 173

navy :: Colour
navy = rgb 0 0 128

oldLace :: Colour
oldLace = rgb 253 245 230

olive :: Colour
olive = rgb 128 128 0

oliveDrab :: Colour
oliveDrab = rgb 107 142 35

orange :: Colour
orange = rgb 255 165 0

orangeRed :: Colour
orangeRed = rgb 255 69 0

orchid :: Colour
orchid = rgb 218 112 214

paleGoldenrod :: Colour
paleGoldenrod = rgb 238 232 170

paleGreen :: Colour
paleGreen = rgb 152 251 152

paleTurquoise :: Colour
paleTurquoise = rgb 175 238 238

paleVioletRed :: Colour
paleVioletRed = rgb 219 112 147

papayaWhip :: Colour
papayaWhip = rgb 255 239 213

peachPuff :: Colour
peachPuff = rgb 255 218 185

peru :: Colour
peru = rgb 205 133 63

pink :: Colour
pink = rgb 255 192 203

plum :: Colour
plum = rgb 221 160 221

powderBlue :: Colour
powderBlue = rgb 176 224 230

purple :: Colour
purple = rgb 128 0 128

rebeccaPurple :: Colour
rebeccaPurple = rgb 102 51 153

red :: Colour
red = rgb 255 0 0

rosyBrown :: Colour
rosyBrown = rgb 188 143 143

royalBlue :: Colour
royalBlue = rgb 65 105 225

saddleBrown :: Colour
saddleBrown = rgb 139 69 19

salmon :: Colour
salmon = rgb 250 128 114

sandyBrown :: Colour
sandyBrown = rgb 244 164 96

seaGreen :: Colour
seaGreen = rgb 46 139 87

seashell :: Colour
seashell = rgb 255 245 238

sienna :: Colour
sienna = rgb 160 82 45

silver :: Colour
silver = rgb 192 192 192

skyBlue :: Colour
skyBlue = rgb 135 206 235

slateBlue :: Colour
slateBlue = rgb 106 90 205

slateGray :: Colour
slateGray = rgb 112 128 144

slateGrey :: Colour
slateGrey = rgb 112 128 144

snow :: Colour
snow = rgb 255 250 250

springGreen :: Colour
springGreen = rgb 0 255 127

steelBlue :: Colour
steelBlue = rgb 70 130 180

tan :: Colour
tan = rgb 210 180 140

teal :: Colour
teal = rgb 0 128 128

thistle :: Colour
thistle = rgb 216 191 216

tomato :: Colour
tomato = rgb 255 99 71

transparent :: Colour
transparent = rgba 0 0 0 0

turquoise :: Colour
turquoise = rgb 64 224 208

violet :: Colour
violet = rgb 238 130 238

wheat :: Colour
wheat = rgb 245 222 179

white :: Colour
white = rgb 255 255 255

whiteSmoke :: Colour
whiteSmoke = rgb 245 245 245

yellow :: Colour
yellow = rgb 255 255 0

yellowGreen :: Colour
yellowGreen = rgb 154 205 50
