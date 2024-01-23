

cartesianProduct :: [a] -> [a] -> [(a,a)]
cartesianProduct x y
    | length x /= length y = error "Lengths must be equal"
    | otherwise = [(a, b) | a <- x, b <- y]

