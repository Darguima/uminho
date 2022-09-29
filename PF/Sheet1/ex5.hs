-- Considere o seguinte tipo de dados para representar os possı́veis estados de um semáforo: data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
-- (a) Defina a função next :: Semaforo -> Semaforo que calcula o próximo estado de um semáforo.
-- (b) Defina a função stop :: Semaforo -> Bool que determina se é obrigatório parar num semáforo.
-- (c) Defina a função safe :: Semaforo -> Semaforo -> Bool que testa se o estado de dois semáforos num cruzamento é seguro.

data TrafficLight = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: TrafficLight -> TrafficLight
next tLight
  | tLight == Verde = Amarelo
  | tLight == Amarelo = Vermelho
  | tLight == Vermelho = Verde

stop :: TrafficLight -> Bool
stop tLight = tLight == Amarelo || tLight == Vermelho

safe :: TrafficLight -> TrafficLight -> Bool
safe tLight1 tLight2 = tLight1 == Vermelho || tLight2 == Vermelho
