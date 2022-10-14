-- 20. Apresente uma definição recursiva da função,
-- powerEnumFrom :: Int -> Int -> [Int]
-- que dado um valor n e um valor m constrói a lista [n^0, . . . , n^(m−1)].

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = [0]
powerEnumFrom n m = powerEnumFrom n (m - 1) ++ [n^m]
