
s <- rfl "/tmp/c.x"
fw "s"
pre s
ls = map (\x -> splitSPC x) s
fw "ls"
pre ls

lt = map (\r -> map (\s -> read s :: GLdouble) r) ls
fw "lt"
pre lt

lm = map (\(x:y:_) -> Vertex3 x y 0) lt 
fw "lm"
pre lm
