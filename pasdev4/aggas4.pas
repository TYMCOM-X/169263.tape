program agg_ass;
var p : ^ record x : integer; y : array [1..*] of char end;
    q : ^ record x : integer; y : packed array [1..*] of char end;
    r : ^ packed array [1..*] of char;
    n : integer;
begin
  p^ := (n, ('a', 'b', 'c'));
  q^ := (n, 'ABCDEF');
  q^ := (n, r^);
end.
    