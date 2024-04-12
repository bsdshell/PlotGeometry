import qualified Data.Map as DM

:{
let cmdTable :: String -> [(String, String)]
    cmdTable  s = ls 
      where
        ls = map (\(a:b:_) -> (trim a, trim b)) $ map (splitStr "=") $ splitStr "," s
:}

:{
type BufferMap = DM.Map String (String, [String])
:}


:set +m
let addShape :: [String] -> BufferMap -> BufferMap
    addShape s bm | cmd == "add" = DM.insert key (shape, tail s) bm
                  | cmd == "del" = DM.delete key bm
                  | otherwise = error $ "ERROR: Invalid cmd=" ++ cmd
      where
        tb = DM.fromList $ cmdTable $ head s 
        cmd = case DM.lookup "cmd" tb of
                    Just x -> x
                    Nothing -> error $ "Invalid format11"
        key = case DM.lookup "key" tb of
                    Just x -> x
                    Nothing -> error $ "Invalid format22" 
        shape = case DM.lookup "shape" tb of
                    Just x -> x
                    Nothing -> error $ "Invalid format33"

s = cmdTable "cmd = add, shape = triangle, key = abc"
fw "s"
pre s

ls = ["cmd = add, shape = triangle, key = k1",
      "(Vertex3 1 2 3, Vertex3 1 2 3, Vertex3 1 2 3)"
      ]

smap = DM.empty
shape = addShape ls smap 
shape
