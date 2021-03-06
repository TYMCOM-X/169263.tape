module test59 options dump;

type
  color = packed (red, green, blue);
  t1 = array[1..4] of boolean;
  t2 = array[2..5] of boolean;
  t3 = array[color] of boolean;
  t4 = array[1..*] of boolean;
  t5 = array[2..*] of boolean;
  t6 = array[red..*] of boolean;
  t7 = array[*] of boolean;

external procedure p1 (t1);
external procedure p2 (t2);
external procedure p3 (t3);
external procedure p4 (t4);
external procedure p5 (t5);
external procedure p6 (t6);
external procedure p7 (t7);


public procedure test (
  v1: t1;
  v2: t2;
  v3: t3;
  v4: t4;
  v5: t5;
  v6: t6;
  v7: t7  );
begin
  p1 (v1);
  p1 (v2);
  p1 (v3);
  p1 (v4);
  p1 (v5);
  p1 (v6);
  p1 (v7);
  p2 (v1);
  p2 (v2);
  p2 (v3);
  p2 (v4);
  p2 (v5);
  p2 (v6);
  p2 (v7);
  p3 (v1);
  p3 (v2);
  p3 (v3);
  p3 (v4);
  p3 (v5);
  p3 (v6);
  p3 (v7);
  p4 (v1);
  p4 (v2);
  p4 (v3);
  p4 (v4);
  p4 (v5);
  p4 (v6);
  p4 (v7);
  p5 (v1);
  p5 (v2);
  p5 (v3);
  p5 (v4);
  p5 (v5);
  p5 (v6);
  p5 (v7);
  p6 (v1);
  p6 (v2);
  p6 (v3);
  p6 (v4);
  p6 (v5);
  p6 (v6);
  p6 (v7);
  p7 (v1);
  p7 (v2);
  p7 (v3);
  p7 (v4);
  p7 (v5);
  p7 (v6);
  p7 (v7);
end.
 