
p0 = Vertex3 0 0 0
p1 = Vertex3 1 0 0
p2 = Vertex3 1 0 1

q0 = Vertex3 0 1 0
q1 = Vertex3 1 1 0
q2 = Vertex3 1 1 1

b1 = isParallelPlane (p0, p1, p2) (q0, q1, q2)
fw "b1"
b1 == True

p0 = Vertex3 0 0 0
p1 = Vertex3 1 0 0
p2 = Vertex3 1 0 1

q0 = Vertex3 0 2 0
q1 = Vertex3 1 1 0
q2 = Vertex3 1 1 1

b2 = isParallelPlane (p0, p1, p2) (q0, q1, q2)
fw "b2"
b2 == False 


v0 = Vertex3 0 0 0 -: Vertex3 0 1 0
pb1 = isPerpPlane v0 (p0, p1, p2) 
fw "pb1"
pb1 == True

v0 = Vertex3 0 0 0 -: Vertex3 0 1 0.00001 
pb2 = isPerpPlane v0 (p0, p1, p2) 
fw "pb2"
pb2 == False 
