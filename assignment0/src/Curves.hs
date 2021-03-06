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
                toFile,
                toSVG) where
import Text.Printf (printf)

-- Data constructor for Point coordinates
data Point = Point Double Double deriving (Show)
point :: (Double, Double) -> Point
point (x,y) = Point x y

-- Getters for Point coordinates
pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

-- Instantiating Point as member of Equivalency class
instance Eq Point where
    (Point x1 y1) == (Point x2 y2)
       | abs (x1-x2) < 0.01 && abs (y1-y2) < 0.01 = True
       | otherwise                                = False

-- Data constructor for Curves which is a starting point followed by a list of 0 or more points
data Curve = Curve Point [Point] deriving (Show)

-- Instantiating Curve as member of Equivalency class
instance Eq Curve where
  Curve h1 t1 == Curve h2 t2
    | h1 == h2 && t1 == t2 = True
    | otherwise            = False

curve :: Point -> [Point] -> Curve
curve = Curve

-- Connect function which adds the next curve to the chain of points in the original curve
connect :: Curve -> Curve -> Curve
connect (Curve h1 t1) (Curve h2 t2) = Curve h1 (t1 ++ [h2] ++ t2)

-- Rotates a curve by applying rotatePoint to each point in the curve
rotate :: Curve -> Double -> Curve
rotate (Curve h t) d = Curve h' t' where
    r = _deg2rad d
    h' = _rotatePoint r h
    t' = map (_rotatePoint r) t

_rotatePoint :: Double -> Point -> Point
_rotatePoint r p = Point (_rotateX p r) (_rotateY p r)

-- Rotates x and y coordinates by a given number of radians
_rotateX :: Point -> Double -> Double
_rotateX p r = pointX p * cos r - pointY p * sin r

_rotateY :: Point -> Double -> Double
_rotateY p r = pointY p * cos r + pointX p * sin r

_deg2rad :: Double -> Double
_deg2rad x = x * pi / 180.0

-- Translates a curve relative to start at a specified point
translate :: Curve -> Point -> Curve
translate (Curve h t) (Point x y) = Curve h' t' where
    dx = x - pointX h
    dy = y - pointY h
    h' = (Point x y)
    t' = map (_transPoint dx dy) t

_transPoint :: Double -> Double -> Point -> Point
_transPoint dx dy (Point x y) = Point (x + dx) (y + dy)


data Line = Vertical Double | Horizontal Double
reflect :: Curve -> Line -> Curve
reflect (Curve h t) (Vertical x) = Curve h' t' where
    h' = _reflectPointV x h
    t' = map (_reflectPointV x) t
reflect (Curve h t) (Horizontal y) = Curve h' t' where
    h' = _reflectPointH y h
    t' = map (_reflectPointH y) t

_reflectPointH :: Double -> Point -> Point
_reflectPointH l (Point x y) = Point x (2*l - y)

_reflectPointV :: Double -> Point -> Point
_reflectPointV l (Point x y) = Point (2*l - x) y


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
  svgString    = (printf "<svg xmlns=\"http://www.w3.org/2000/svg\"\n\twidth=\"%dpx\" height=\"%dpx\" version=\"1.1\">\n<g>" ((ceiling ((width (Curve h t)) + (abs $ pointX h ))::Int)) ((ceiling $ (height (Curve h t)) + (abs $ pointY h))::Int)) ++ (repString l) ++ "\n</g>\n</svg>"

repString :: [Point] -> String
repString []     = ""
repString (p1:p2:ps) = (printf "\n<line style=\"stroke-width: 2px; stroke: black; fill:white\" x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />" ((pointX p1)::Double) ((pointX p2)::Double) ((pointY p1)::Double) ((pointY p2)::Double)) ++ (repString (p2:ps))
repString p      = ""

toFile :: Curve -> FilePath -> IO ()
toFile c f = writeFile f (toSVG c)

maxTot :: (Ord a) => [a] -> a
maxTot xs = foldr1 (\x acc -> if x > acc then x else acc) xs

minTot :: (Ord a) => [a] -> a
minTot xs = foldr1 (\x acc -> if x < acc then x else acc) xs

peano :: Curve -> Curve
peano c = c0 `connect` c1 `connect` c2 `connect` c3 `connect` c4 `connect` c5 `connect` c6 `connect` c7 `connect` c8 where
  w = width c
  h = height c
  p = 6.0
  ch  = reflect c $ Vertical 0
  cv  = reflect c $ Horizontal 0
  cvh = reflect ch $ Horizontal 0

  c0 = c
  c1 = ch `translate` point (w, h+p)
  c2 = c `translate` point (0, 2*(p+h))
  c3 = cv `translate` point (w+p, h+2*(p+h))
  c4 = cvh `translate` point (w+p+w, h+p+h)
  c5 = cv `translate` point (w+p, h)
  c6 = c `translate` point (2*(p+w), 0)
  c7 = ch `translate` point (w+2*(w+p), h+p)
  c8 = c `translate` point (2*(w+p), 2*(p+h))

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)
