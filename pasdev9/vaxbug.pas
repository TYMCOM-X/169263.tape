program vaxbug options special;

const
  fake_ptr = ptr (0);

var
  p: ^ integer;

begin
  p := fake_ptr;
end.
   