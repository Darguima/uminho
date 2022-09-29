-- Repita o exercı́cio anterior assumindo agora que as horas são representadas por um
-- novo tipo de dados:
-- data Hora = H Int Int deriving (Show,Eq)
-- Com este novo tipo a hora meia noite e um quarto é representada por H 0 15 e a hora
-- duas menos um quarto por H 13 45.

-- Defina funções para:
-- ( isValid ) testar se um par de inteiros representa uma hora do dia válida;
-- ( isThen ) testar se uma hora é ou não depois de outra (comparação);
-- ( convertTimeToMinutes ) converter um valor em horas (par de inteiros) para minutos (inteiro);
-- ( convertMinutesToTime ) converter um valor em minutos para horas;
-- ( subtractTime ) calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos);
-- (f) adicionar um determinado número de minutos a uma dada hora.

data Hour = H Int Int deriving (Show,Eq)

meioDiaMeia :: Hour
meioDiaMeia = H 12 30


isValid :: Hour -> Bool
isValid (H h m) = h >= 0 && h < 24 && m >= 0 && m < 60

isThen :: Hour -> Hour -> Bool
isThen (H h2 m2) (H h1 m1)
  | h2 > h1 = True
  | h2 == h1 = m2 > m1
  | otherwise = False

convertTimeToMinutes :: Hour -> Int
convertTimeToMinutes (H h1 m1) = h1 * 60 + m1

convertMinutesToTime ::  Int -> Hour
convertMinutesToTime minutes = H (div minutes 60) (mod minutes 60)

subtractTime :: Hour -> Hour -> Int
subtractTime time2 time1 = convertTimeToMinutes time2 - convertTimeToMinutes time1

addMinutesToTime :: Hour -> Int -> Hour
addMinutesToTime time addMinutes = convertMinutesToTime (convertTimeToMinutes time + addMinutes)