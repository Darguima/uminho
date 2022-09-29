-- Vamos representar horas por um par de números inteiros:
-- type Hora = (Int,Int)
-- Assim o par (0,15) significa meia noite e um quarto e (13,45) duas menos um quarto.
-- Defina funções para:
-- ( isValid ) testar se um par de inteiros representa uma hora do dia válida;
-- ( isThen ) testar se uma hora é ou não depois de outra (comparação);
-- ( convertTimeToMinutes ) converter um valor em horas (par de inteiros) para minutos (inteiro);
-- ( convertMinutesToTime ) converter um valor em minutos para horas;
-- ( subtractTime ) calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos);
-- (f) adicionar um determinado número de minutos a uma dada hora.

type Time = (Int, Int)

isValid :: Time -> Bool
isValid (h, m) = h >= 0 && h < 24 && m >= 0 && m < 60

isThen :: Time -> Time -> Bool
isThen (h2, m2) (h1, m1)
  | h2 > h1 = True
  | h2 == h1 = m2 > m1
  | otherwise = False

convertTimeToMinutes :: Time -> Int
convertTimeToMinutes (h1, m1) = h1 * 60 + m1

convertMinutesToTime ::  Int -> Time
convertMinutesToTime minutes = (div minutes 60, mod minutes 60)

subtractTime :: Time -> Time -> Int
subtractTime time2 time1 = convertTimeToMinutes time2 - convertTimeToMinutes time1

addMinutesToTime :: Time -> Int -> Time
addMinutesToTime time addMinutes = convertMinutesToTime (convertTimeToMinutes time + addMinutes)