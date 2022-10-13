--  double :: [Float] -> [Float] que recebe uma lista e produz a lista em que
-- cada elemento é o dobro do valor correspondente na lista de entrada.

double :: [Float] -> [Float]
double [] = []
double (h : t) = h * 2 : double t
