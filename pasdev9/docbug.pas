program bug;

var f: file of *;
    p: ^ array[1..*] of integer;

begin
  write (f, extent (p), p^: extent (p));
  write (f, upperbound (p^), p^: size (p^, upperbound (p^)));
end.
 