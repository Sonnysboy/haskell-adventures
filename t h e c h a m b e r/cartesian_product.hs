

cartesianProduct :: [a] -> [a] -> [(a,a)]
cartesianProduct x y
     = [(a, b) | a <- x, b <- y]

