module Tests () where
import Curves

-- testing the library by drawing an inverted lambda :)
testCurve1, testCurve2, testCurve3, testCurve4, testCurve5 :: Curve
testCurve1 = curve (point (0, 50)) [(point (0, 0)), (point (0, 100))] -- vertical line with the head in the middle
testCurve2 = rotate testCurve1 (-30) -- now this already looks like the long \
testCurve3 = translate testCurve2 (point (50, 50)) -- shift it to the right
testCurve4 = connect (curve (point (10, 93)) []) testCurve3 -- the 2. leg, lambda finished, lets destroy it ;)
testCurve5 = reflect testCurve4 (Vertical 0) -- mirror it to the left, it is off the screen now
testCurve6 = normalize testCurve5 -- but normalize brings it back to sight

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
