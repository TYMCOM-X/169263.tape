program strtst;

  function copy (str: packed array [1..*] of char; n: 0..255): string [20];
   var i: 0 .. 20;
   begin
    copy := 'X';
    for i := 1 to n do
      copy := copy || str;
   end;

 var c: string [4];

 begin
  c := copy (c, 4);
 end.
    