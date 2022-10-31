-- 44. Apresente uma definição recursiva da função pré-definida partitionEithers :: [Either a b] -> ([a],[b])
-- que divide uma lista de Eithers em duas listas.

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([], [])
partitionEithers ((Left a) : t) = let (as, bs) = partitionEithers t
                                in (a : as, bs)
partitionEithers ((Right b) : t) = let (as, bs) = partitionEithers t
                                in (as, b : bs)
