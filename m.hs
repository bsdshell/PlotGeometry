

p = printMat

m = [
      [0, 0, 1],
      [0, 1, 0],
      [1, 0, 0]
     ]

v = [1, 2, 3]

u = join $ m `multiVec` v
w = m `multiVec` u

fw "m"
p m

fw "u"
u

fw "w"
w
