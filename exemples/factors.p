COMMENT Computes the prime factors of a positive integer (with repetition)
COMMENT Prints the found factors (or just n if n is prime)
COMMENT Prints only -1 if n is not positive
READ n
IF n <= 0
  PRINT -1
ELSE
  i := 2
  WHILE * i i <= n
    IF % n i = 0
      PRINT i
      n := / n i
    ELSE
      i := + i 1
  PRINT n
