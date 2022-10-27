-- 9. Defina cada uma das listas seguintes por compreensão.
-- (a) [1,2,4,8,16,32,64,128,256,512,1024]
-- (b) [(1,5),(2,4),(3,3),(4,2),(5,1)]
-- (c) [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
-- (d) [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]
-- (e) [1,2,6,24,120,720]

listA = [2 ^ x | x <- [1..10]]
listB = [(x, 6 - x) | x <- [1..5]]
listC = [[1..y] | y <- [1..5]]
listD = [replicate y 1 | y <- [1..5]]