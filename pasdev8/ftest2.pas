program ftest2;

var i: integer;



(* EDGE 1 -> 2 *)

(* EDGE 1 -> 3 *)

(* EDGE 2 -> 4 *)

(* EDGE 3 -> 4 *)

(* EDGE 3 -> 5 *)

(* EDGE 3 -> 6 *)

(* EDGE 4 -> 7 *)

(* EDGE 5 -> 8 *)

(* EDGE 6 -> 9 *)

(* EDGE 6 -> 10 *)

(* EDGE 7 -> 11 *)

(* EDGE 7 -> 12 *)

(* EDGE 8 -> 14 *)

(* EDGE 9 -> 10 *)

(* EDGE 9 -> 13 *)

(* EDGE 10 -> 9 *)

(* EDGE 10 -> 13 *)

(* EDGE 11 -> 7 *)

(* EDGE 12 -> 15 *)

(* EDGE 13 -> 14 *)

(* EDGE 14 -> 15 *)
label
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15;

begin

1:
  case i of
    others: goto 2;
    2: goto 3
  end;

2:
  case i of
    others: goto 4
  end;

3:
  case i of
    others: goto 4;
    2: goto 5;
    3: goto 6
  end;

4:
  case i of
    others: goto 7
  end;

5:
  case i of
    others: goto 8
  end;

6:
  case i of
    others: goto 9;
    2: goto 10
  end;

7:
  case i of
    others: goto 11;
    2: goto 12
  end;

8:
  case i of
    others: goto 14
  end;

9:
  case i of
    others: goto 10;
    2: goto 13
  end;

10:
  case i of
    others: goto 9;
    2: goto 13
  end;

11:
  case i of
    others: goto 7
  end;

12:
  case i of
    others: goto 15
  end;

13:
  case i of
    others: goto 14
  end;

14:
  case i of
    others: goto 15
  end;

15:
  stop;

end.

