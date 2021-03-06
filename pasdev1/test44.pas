PROGRAM TEST44 OPTIONS DUMP;

(*  CONSTANT ARRAYS AND SUBSCRIPT REFERENCES  *)

CONST ASIZE = 10;

TYPE  A = ARRAY [1..ASIZE] OF 0 .. ASIZE;

VAR   X: INTEGER;
(*  LEGAL ARRAY CONSTANTS  *)


CONST A1: A = (2,3,4,5,6,7,8,9,10,0);
      A2: A = (3,4,5,6,7,8,9,10,2,0);
      A3: A = (1,2,3,4,5,6,7,8,9,10);

(*  BAD ARRAY CONSTANTS  *)

CONST B1: A = (1,3,5,7,9);
      B2: A = (1,2,3,4,5,6,7,8,9,10,0);
      B3: A = (2,3,4,5,6,7,8,9,10,11);
      B4: A = (1,2,3,4,5,X+1,7,8,9,10);

(*  LEGAL ARRAY REFERENCES  *)

CONST C1 = A1 [1];
      C2 = A2 [A2[2]];
      C3 = A3 [A3[A3[3]]];

(*  BAD ARRAY REFERENCES  *)

CONST D1 = A1 [0];
      D2 = A2 [11];
      D3 = A1 [A1[10]];

BEGIN  END.
   