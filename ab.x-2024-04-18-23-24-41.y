
:set +m
let isCCWX :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> Vector3 GLdouble -> Bool
    isCCWX (p0, p1, p2) up = True 
      where 
        ax = 4
-- /Users/aaa/myfile/github/PlotGeometry//tmp/a.x-2024-04-18-23-24-24.x
--            v01 = p0 -: p1
--            v12 = p1 -: p2
--            vc = v01 `crossF` v12
--            ve = case vc of
--                    Just v -> v
--                    Nothing -> error "ERROR: three pts are colinear"
--            ang = angle2Vector ve up
--            del = ang - pi/2
--            fx = True
--        fa = case del of
--                d | abs d <= epsilon -> False   -- perpendicular, colinear
--                  | d < 0            -> True    -- angl < pi/2
--                  | otherwise        -> False   -- ang > pi/2

:set +m
let fun :: Int -> Int
    fun x = 3 + ax
      where 
        ax = 4

p0 = Vertex3 0 0 0
p1 = Vertex3 1 0 0
p2 = Vertex3 0 1 0
ve = Vector3 0 0 1

b = isCCWX (p0, p1, p2) ve
fw "b"
b

a = fun 3
a
