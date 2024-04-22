
p = printMat
rs = redisSet

e1 = Vector3 1 0 0
v1 = Vector3 (1) 1 1
vp = projXZ v1
a = angle2Vector e1 vp
fw "a"
a
180/pi * a
fw "kk"
v1 = Vector3 (-1) 1 (-1) 
vp = projXZ v1
a = angle2Vector e1 vp
180/pi * a

:{
let funx :: Int -> Int
    funx x = x + a 
      where
        a = 4 :: Int
:}


:{
let copyen :: String -> IO()
    copyen s = do
      getEnv s >>= pbcopy
:}

funx 4

p0 = Vertex3 0 0 0
p1 = Vertex3 1 0 0
p2 = Vertex3 0.5 0 0

tx = isColinearXY2d p0 p1 p2
fw "tx"
tx

p0 = Vertex3 0 0 0
p1 = Vertex3 0 1 0
p2 = Vertex3 0 0.5 0

tx1 = isColinearXY2d p0 p1 p2
fw "tx1"
tx1

a = 1::Int
b = 2 :: Int

p0 = Vertex3 0 0 0
p1 = Vertex3 0 a 0
p2 = Vertex3 0 b 0

tx2 = isColinearXY2d p0 p1 p2
fw "tx2"
tx2
