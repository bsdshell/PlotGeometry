import Data.Maybe
import Control.Lens


ls <- getEnv "glog" >>= rfl 

fw "len s"
print $ len ls 

bs = filter (/= []) $ map (hasStrBlock "True") $ splitBlock ls "----"
fw "len bs"
print $ len bs

prex $ take 3 $ join bs

wfl "/tmp/b" $ (join . join) bs


