module Curves ( Point,
                Curve,
                point,
                pointX,
                pointY,
                curve,
                connect,
                rotate,
                translate,
                Line (Vertical,Horizontal),
                reflect,
                bbox,
                width,
                height,
                toList,
                normalize,
                toFile) where
import Text.Printf (printf)

data Point = Point Double Double deriving (Show)
point :: (Double, Double) -> Point
point (x,y) = Point x y

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

instance Eq Point where
    (Point x1 y1) == (Point x2 y2)
       | abs (x1-x2) < 0.01 && abs (y1-y2) < 0.01 = True
       | otherwise                                = False

data Curve = Curve Point [Point] deriving (Show)
instance Eq Curve where
  Curve h1 t1 == Curve h2 t2
    | h1 == h2 && t1 == t2 = True
    | otherwise            = False

curve :: Point -> [Point] -> Curve
curve = Curve

connect :: Curve -> Curve -> Curve
connect (Curve h1 t1) (Curve h2 t2) = Curve h1 (t1 ++ [h2] ++ t2)

rotate :: Curve -> Double -> Curve
rotate (Curve h t) d = Curve h' t' where
    r = deg2rad d
    h' = rotatePoint r h
    t' = map (rotatePoint r) t

rotatePoint :: Double -> Point -> Point
rotatePoint r p = Point (rotateX p r) (rotateY p r)

rotateX :: Point -> Double -> Double
rotateX p r = pointX p * cos r - pointY p * sin r

rotateY :: Point -> Double -> Double
rotateY p r = pointY p * cos r + pointX p * sin r

deg2rad :: Double -> Double
deg2rad x = x * pi / 180.0


translate :: Curve -> Point -> Curve
translate (Curve h t) (Point x y) = Curve h' t' where
    dx = x - pointX h
    dy = y - pointY h
    h' = (Point x y)
    t' = map (transPoint dx dy) t

transPoint :: Double -> Double -> Point -> Point
transPoint dx dy (Point x y) = Point (x + dx) (y + dy)


data Line = Vertical Double | Horizontal Double
reflect :: Curve -> Line -> Curve
reflect (Curve h t) (Vertical x) = Curve h' t' where
    h' = reflectPointV x h
    t' = map (reflectPointV x) t
reflect (Curve h t) (Horizontal y) = Curve h' t' where
    h' = reflectPointH y h
    t' = map (reflectPointH y) t

reflectPointH :: Double -> Point -> Point
reflectPointH l (Point x y) = Point x (2*l - y)

reflectPointV :: Double -> Point -> Point
reflectPointV l (Point x y) = Point (2*l - x) y


bbox :: Curve -> (Point, Point)
bbox (Curve h t) = (Point minX minY, Point maxX maxY) where
    l = h : t
    minX = minTot [pointX p | p <- l]
    minY = minTot [pointY p | p <- l]
    maxX = maxTot [pointX p | p <- l]
    maxY = maxTot [pointY p | p <- l]


width, height :: Curve -> Double
width c = r - l where
    box = bbox c
    r = pointX (snd box)
    l = pointX (fst box)
height c = u - l where
    box = bbox c
    u = pointY (snd box)
    l = pointY (fst box)


toList :: Curve -> [Point]
toList (Curve h t)= h : t


normalize :: Curve -> Curve
normalize (Curve h t) = translate (Curve h t) (Point dx dy) where
    corner = fst $ bbox (Curve h t)
    dx = pointX h - pointX corner
    dy = pointY h - pointY corner

toSVG ::Curve -> String
toSVG (Curve h t) = svgString where
  l            = h:t
  svgString    = printf "<svg xmlns=\"http://www.w3.org/2000/svg\"\n\twidth=\"%dpx\" height=\"%dpx\" version=\"1.1\">\n<g>" ((ceiling (width (Curve h t)))::Int) ((ceiling (height (Curve h t))::Int)) ++ (repString l) ++ "\n</g>\n</svg>"

repString :: [Point] -> String
repString []     = ""
repString (p1:p2:ps) = (printf "\n<line style=\"stroke-width: 2px; stroke: black; fill:white\" x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />" ((pointX p1)::Double) ((pointX p2)::Double) ((pointY p1)::Double) ((pointY p2)::Double)) ++ (repString (p2:ps))
repString p      = ""

toFile :: Curve -> FilePath -> IO ()
toFile c f = writeFile f (toSVG c)

maxTot :: [Double] -> Double
maxTot []     = 0
maxTot (x1:x2:xs)
  | x1 < x2   = maxTot (x2:xs)
  | otherwise = maxTot (x1:xs)
maxTot (x:xs) = x

minTot :: [Double] -> Double
minTot []     = 0
minTot (x1:x2:xs)
  | x1 < x2   = minTot (x2:xs)
  | otherwise = minTot (x1:xs)
minTot (x:xs) = x
