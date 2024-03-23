:{
let isCCW3dX :: (Floating a, Ord a) => (Vertex3 a, Vertex3 a, Vertex3 a) -> Bool 
    isCCW3dX (p0, p1, p2) | ang < pi/2 = True 
                          | ang > pi/2 = False
      where
        isCol = isColinear3d p0 p1 p2
        isYZPlane = (vx_1 p0) == (vx_1 p1) &&  (vx_1 p1) == (vx_1 p2)
        isXZPlane = (vx_2 p0) == (vx_2 p1) &&  (vx_2 p1) == (vx_2 p2)
        isXYPlane = (vx_3 p0) == (vx_3 p1) &&  (vx_3 p1) == (vx_3 p2)
        epsilon = 1e-12
        v10 = p1 -: p0
        v12 = p1 -: p2
        vn = v10 `crossF` v12
        ang = case vn of
                Just v -> not isYZPlane ? angle2Vector v (Vector3 0 1 0) $ angle2Vector v (Vector3 1 0 0) 
                Nothing -> error "ERROR: three pts are colinear"
:}

:{
let det3Co :: (Fractional a) => [[a]] -> (a, a, a)
    det3Co [[a11, a12, a13], 
            [b21, b22, b23],
            [c31, c32, c33]] = (a11 * d1 * (-1)^(1 + 1), a12 * d2 * (-1)^(1 + 2), a13* d3 * (-1)^(1 + 3))
          
      where
        d1 = det2 [[b22, b23],
                   [c32, c33]]
        d2 = det2 [[b21, b23],
                   [c31, c33]]
        d3 = det2 [[b21, b22],
                   [c31, c32]]
:}

:{
let v33ToM33:: (Fractional a) => (Vector3 a, Vector3 a, Vector3 a) -> [[a]]
    v33ToM33 (v0, v1, v2) = [ x v0, x v1, x v2]
      where
        x = vecToList
:}

:{
let isCCW :: (Floating a, Ord a) => (Vertex3 a, Vertex3 a, Vertex3 a) -> Bool
    isCCW (p0, p1, p2) = if notAll ve then (ang < pi/2 ? True $ False) else ((x' + y' + z') > 0 ? True $ False)
      where
        v01 = p0 -: p1
        v12 = p1 -: p2
        vc = v01 `crossF` v12
        notAll (Vector3 x y z) = x /= 0 && y /= 0 && z /= 0
        ve = case vc of
                Just v -> v
                Nothing -> error "ERROR: three pts are colinear"
        ang = angle2Vector ve (Vector3 0 1 0)
        x' = ve_1 ve 
        y' = ve_2 ve
        z' = ve_3 ve
        
:}




:{
let tupToVec:: (Fractional a) => (a, a, a) -> Vector3 a 
    tupToVec (x, y, z) = Vector3 x y z 
:}

p0  = Vertex3 0 0 0
p1  = Vertex3 0.2 0.2 0.2 
p2  = Vertex3 0.2 0.3 (-0.4)

b = isCCW3dX (p0, p1, p2)
fw "b"
b



p0  = Vertex3 0 0 0
p1  = Vertex3 0.0 0.2 0.2 
p2  = Vertex3 0 0.3 (-0.4)

b3 = isCCW (p0, p1, p2)
fw "b3"
b3
b3 == False

p0  = Vertex3 0 0 0
p1  = Vertex3 0.2 0 0
p2  = Vertex3 0 0 (-0.4)

b4 = isCCW (p0, p1, p2)
fw "b4"
b4
b4 == True  

p0  = Vertex3 0 0.1 0.1 
p1  = Vertex3 0 0.3 (-0.2) 
p2  = Vertex3 0 0.2 0.3 

b5 = isCCW (p0, p1, p2)
fw "b5"
b5
b5 == True  

v00 = Vector3 1 1 1
v01 = p0 -: p1
v12 = p1 -: p2
vn = v01 `crossF` v12

m3 = v33ToM33 (v00, v01, v12)
d = det3Co m3
fw "det3Co"
d
vec1 = tupToVec d
fw "vec1"
vec1

ang2 = angle2Vector (Vector3 0 1 0) vec1
fw "ang2"
ang2




epsilon = 1e-12
v10 = p1 -: p0
v12 = p1 -: p2
vn = v10 `crossF` v12

:set +m
let ang = case vn of
          Just v -> angle2Vector v (Vector3 0 1 0)     
          Nothing -> error "ERROR: three pts does not form a triangle"

fw "ang"
ang
fw "pi/2"
pi/2

b1 = abs (ang - pi/2) < epsilon
fw "b1"
b1

diff = abs(ang - pi/2)
fw "diff"
diff

b2 = abs (ang - pi/2) == epsilon
fw "b2"
b2

ls <- rfl "/tmp/n"
lt = filter (>4) $ map (\x -> read x :: Float) ls
ave = (sum lt) / len lt
fw "ave"
ave



