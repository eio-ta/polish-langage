COMMENT Calcul le n-ème nombre de Fibonacci,
COMMENT https://en.wikipedia.org/wiki/Fibonacci_number
COMMENT Le premier nombre a indice 0.
READ n
x := 0
y := 1
IF n = 0
  PRINT 0
ELSE
  n := - n 1
  WHILE n > 0
    z := + x y
    x := y
    y := z
    n := - n 1
  PRINT y
