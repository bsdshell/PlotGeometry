
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
