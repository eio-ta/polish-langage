COMMENT Convert a non-negative integer n to base b
COMMENT This program prints the least significant digit first,
COMMENT and the least significant last.
READ n
READ b
WHILE n <> 0
  PRINT % n b
  n := / n b
