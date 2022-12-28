COMMENT computes the Syracuse sequence of an integer n
READ n
WHILE n > 1
  IF % n 2 = 0
    n := / n 2
  ELSE
    n := + 1 * 3 n
  PRINT n
