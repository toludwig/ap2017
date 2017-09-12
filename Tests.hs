module Tests () where
import Curves

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
  ch = reflect c $ Vertical 0
  cv = reflect c $ Horizontal 0

  c0 = c
  c1 = c `translate` point (w, h+p+h)
  c2 = c `translate` point (w, h+2*(p+h))
  c3 = c `rotate` (180) `translate` point (w+p+h, h+2*(w+p+h))
  c4 = c3 `translate` point (w+p+h, h+p+h)
  c5 = c3 `translate` point (w+p+h, h)
  c6 = c `translate` point (w+2*(p+h), h)
  c7 = c `translate` point (w+2*(p+h), h+p+h)
  c8 = c `translate` point (w+2*(p+h), h+2*(p+h))

  peano' :: Curve -> Curve
  --peano' c = c0 `connect` c1 `connect` c2 `connect` c3 `connect` c4 `connect` c5 `connect` c6 `connect` c7 `connect` c8 where
  peano' c = c `connect` cv `connect` c2v `connect` c2h `connect` ch
    w = width c
    h = height c
    p = 6

    ch  = Vertical w
    c2h = Vertical (2*w)

    cv  = Horizontal h
    c2v = Horizontal (2*h)

    c0 =

peano'' :: Curve -> Curve
peano'' c = c0 `connect` c1 `connect` c2 `connect` c3 `connect` c4 `connect` c5 `connect` c6 `connect` c7 `connect` c8 where
  w = width c
  h = height c
  p = 6.0
  ch = reflect c $ Vertical 0
  cv = reflect c $ Horizontal 0

  c0 = c
  c1 = cv `translate` point (w, h+p)
  c2 = c `translate` point (0, h+2*(p+h))
  c3 = c `rotate` (180) `translate` point (w+p+h, h+2*(w+p+h))
  c4 = c3 `translate` point (w+p+h, h+p+h)
  c5 = c3 `translate` point (w+p+h, h)
  c6 = c `translate` point (w+2*(p+h), h)
  c7 = c `translate` point (w+2*(p+h), h+p+h)
  c8 = c `translate` point (w+2*(p+h), h+2*(p+h))
  c9 =

peanoBase :: Curve
peanoBase = peano $ curve (point (0,0)) []

peanoN :: Int -> Curve
peanoN 1 = peanoBase
peanoN n = peano $ peanoN (n-1)


curve1 :: Curve
curve1 = (curve (point (1,1)) [point (7,8)]) `translate` point (5, 6)
