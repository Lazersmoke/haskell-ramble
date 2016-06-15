module Op where
f(x:xs)=foldl(\x(o,y)->o x y)x(zip(cycle[(+),(-),(*),(/),flip(^).floor])xs)
n= foldl(flip id)0.zipWith flip((+):cycle[(+),(-),(*),(/),(**)])

testFunc :: [[a]] -> [[a]]
testFunc (l:ls) = l >>= \x -> testFunc
