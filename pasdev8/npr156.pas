program npr156;
const a: string [3] = 'ABC';
      b: packed array [1..3] of char = 'DEF';
      c = 'GHI';
var d: string [3] := 'JKL';
    e: packed array [1..3] of char := 'MNO';
var f: file of *;
begin
  write (f, a, b, c, d, e, 'PQR');
end.
