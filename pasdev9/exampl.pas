program example;

var str: string;
    i, j: integer;

begin
  str := substr (str, i, j) || substr (str, j, i) || 'x';
  str := substr (str, i, 6) || substr (str, j, 9) || 'y';
end.
    