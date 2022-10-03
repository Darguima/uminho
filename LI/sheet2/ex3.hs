-- Defina uma função recursiva que recebe uma lista de pares de inteiros e adiciona um valor dado à primeira componente de cada par.

sumTupleList :: [(Int, Int)] -> Int -> [(Int, Int)]
sumTupleList [] _ = []
sumTupleList ((headTuple, tailTuple) : tailList) sumNum = ( headTuple + sumNum, tailTuple ) : sumTupleList tailList sumNum
