PROGRAM TEST19 OPTIONS DUMP;

VAR I, J, K, L, M, N: INTEGER;
    B1, B2: BOOLEAN;
    P, Q: ^ BOOLEAN;
    S1, S2, S3: SET OF 1 .. 10;
    STR1, STR2, STR3: STRING [10];

BEGIN
  IF (I > J) THEN
    B1 := P <= Q
  ELSE
    B2 := B1 <= B2;
  S3 := S1 * S2 - S1;
  STR1 := 'A' || STR2[3:5] || STR3 [2];
END.
   