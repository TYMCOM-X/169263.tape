module vaxbug options special;

public procedure vaxbug ( b: boolean; var str: string[*]);

var i: integer;
    p: ptr;

begin
  p := address (b);
  p := address (str);
  i := upperbound (str);
  vaxbug (b, str);
end.
    