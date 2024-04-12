
vx = Vertex3
p0 = vx 1 0 0
p1 = vx 0 0 0
p2 = vx 0 1 0

v10 = p1 -: p0
v12 = p1 -: p2
ang = angle2Vector v10 v12

ang
180/pi * ang

fw "angle3Pts"
a = angle3Pts p0 p1 p2
180/pi * a

md = matId 3
vn = Vector3 1 1 1 
m = rotMat vn (pi/2)
printMat m
m1 = m `multiMat` md
fw "m1"
printMat m1

mr = tran m1
fw "mr"
printMat mr

m2 = mr `multiMat` m1
fw "m2"
printMat m2
