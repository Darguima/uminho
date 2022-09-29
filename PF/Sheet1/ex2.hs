-- Defina as seguintes funções sobre polinómios de 2o grau:
-- (a) A função nRaizes que recebe os (3) coeficientes de um polinómio de 2o grau e que calcula o número de raı́zes (reais) desse polinómio.
-- (b) A função raizes que, usando a função anterior, recebe os coeficientes do polinómio e calcula a lista das suas raı́zes reais.

raizes a b c
  | nRaizes a b c == 0 = []
  | nRaizes a b c == 1 = [-b / (2*a) ]
  | nRaizes a b c == 2 = [ r1, r2 ]

  where delta = b² - 4*a*c
    r1 = (-b - sqrt delta) / (2*a)
    r2 = (-b + sqrt delta) / (2*a)
