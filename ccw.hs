


:{
let isCCWUp :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> Vector3 GLdouble -> (Bool, GLdouble)
    isCCWUp (p0, p1, p2) up = (b, ang) 
      where
        epsilon = 1e-12 
        v01 = p0 -: p1
        v12 = p1 -: p2
        vc = v01 `crossF` v12
        ve = case vc of
                Just v -> v
                Nothing -> error "ERROR: three pts are colinear"
        ang = angle2Vector ve up
        del = ang - pi/2
        b = case del of
                 d | (abs del) <= epsilon -> False
                   | del < 0              -> True 
                   | otherwise            -> False
:}

epsilon = 1e-12 

:set +m
  p0 = Vertex3 0 0 0
  p1 = Vertex3 1 0 0
  p2 = Vertex3 0 1 0
  up = Vector3 0 0 1 
  t1 = isCCWUp (p0, p1, p2) up

:set +m
  p0 = Vertex3 0 0 0
  p1 = Vertex3 1 0 0
  p2 = Vertex3 0 1 0
  up = Vector3 0 0 (-1) 

  t2 = isCCWUp (p0, p1, p2) up

fw "test1"
print $ fst t1 == True 
print $ snd t1
print $ (abs $ snd t1) <= epsilon

fw "test2"
print $ fst t2 == False
print $ snd t2 
print $ (abs $ snd t2 - pi) <= epsilon

