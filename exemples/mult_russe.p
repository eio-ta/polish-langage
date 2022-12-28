COMMENT Algorithme de multiplication du paysan russe.
READ m
READ n
res := 0
WHILE n <> 0
  IF % n 2 = 1
    res := + res m
  m := * 2 m
  n := / n 2
PRINT res
