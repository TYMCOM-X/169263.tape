program str;

var
  string_var : string[10];

  procedure junk (trash : packed array [1..4] of char);

  begin
    assert (trash = '3456');
  end;

begin
  string_var := '1234567890';
  junk (substr (string_var, 3, 4));
end.
