module Curves where

data Point = Point Double Double deriving (Show)
-- point :: (Double, Double) -> Point
-- point (x,y) = Point x y

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

instance Eq Point where
    (Point x1 y1) == (Point x2 y2)
       | abs (x1-x2) < 0.01 && abs (y1-y2) < 0.01 = True
       | otherwise                                = False

data Curve = Curve Point [Point] deriving (Show)
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
rotateX p r = (pointX p) * cos r - (pointY p) * sin r

rotateY :: Point -> Double -> Double
rotateY p r = (pointY p) * cos r + (pointX p) * sin r

deg2rad :: Double -> Double
deg2rad x = x * pi / 180.0


translate :: Curve -> Point -> Curve
translate (Curve h t) (Point x y) = Curve h' t' where
    dx = x - (pointX h)
    dy = y - (pointY h)
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
    minX = minimum [pointX p | p <- l]
    minY = minimum [pointY p | p <- l]
    maxX = maximum [pointX p | p <- l]
    maxY = maximum [pointY p | p <- l]


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
    

testCurve1, testCurve2 :: Curve
testCurve1 = Curve (Point 3.0 5.0) [(Point 6.0 4.0), (Point 1 2)]
testCurve2 = rotate testCurve1 90.0
