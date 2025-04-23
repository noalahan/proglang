triAccHelper :: Int -> Int -> Int
triAccHelper 0 s = s
triAccHelper n s = triAccHelper (n-1) (s + n)

triAcc :: Int -> Int
triAcc 0 = 0
triAcc n = triAccHelper n 0

duplicateAllAcc :: [a] -> [a] -> [a]
duplicateAllAcc [] a = a
duplicateAllAcc (x:xs) a = duplicateAllAcc xs (a ++ [x, x])