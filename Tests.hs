module Tests () where
import Curves

-- testing the library
testCurve1, testCurve2, testCurve3, testCurve4, testCurve5 :: Curve
testCurve1 = curve (Point 0.0 1.0) [Point 0.0 5.0, Point 0.0 10.0]
testCurve2 = rotate testCurve1 -90.0 -- coordinates of points should be flipped
testCurve3 = translate testCurve2 (Point 0, 0) -- line from 0 to 9 on x axis
testCurve4 = reflect testCurve3 (Horizontal 1) -- line from 0 to 9 with y = 2
testCurve5 = connect testCurve3 testCurve4

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

hilbCom = hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []

hilbN :: Int -> Curve
hilbN 1 = hilbert $ curve (point (0,0)) []
hilbN n = hilbert $ hilbN (n-1)

testCurve1, testCurve2 :: Curve
testCurve1 = curve (point (3.0,5.0)) [point (6.0, 4.0), point (1, 2)]
testCurve2 = rotate testCurve1 90.0

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

peanoBase :: Curve
peanoBase = peano $ curve (point (0,0)) []

peanoN :: Int -> Curve
peanoN 1 = peanoBase
peanoN n = peano $ peanoN (n-1)


curve1 :: Curve
curve1 = (curve (point (1,1)) [point (7,8)]) `translate` point (5, 6)

testCurve3 :: Curve
testCurve3 = peanoBase `translate` point (0, 2*(6+(height peanoBase)))

testCurve4 :: Curve
testCurve4 = reflect (reflect peanoBase $ Vertical 0) $ Horizontal 0

refVert :: Curve
refVert = (reflect peanoBase $ Horizontal 0)

testCurve4' :: Curve
testCurve4' = testCurve4 `translate` point (w+p, h+2*(p+h)) where
  w = width testCurve4
  h = height testCurve4
  p = 6.0