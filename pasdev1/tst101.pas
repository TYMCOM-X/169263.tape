program tst101;

 type color = (red, orange, yellow, green, blue, violet);
 type smallint = -32..31;

 var  a: array[orange..green, red..blue] of smallint;
 var  b: packed array [color] of smallint;

 var i: 0..31;
 var c1, c2: color;

 type flexrec =
   packed record
     field1, field2: 0..777777B;
     field3: smallint;
     arr: array[1..*] of smallint
   end;

 var p: ^ flexrec;
begin
  new (p, 27);
  i := p^.field1;
  i := p^.field2;
  i := p^.field3;
  i := upperbound (p^.arr, 1);
  i := lowerbound (p^.arr, 1);
  i := p^.arr [i+2];

  i := dimension (a,2);
end.
