program getstr;

var
  int_var : integer;
  str_var : string[2];
  char_var : char;

begin
  getstring (str_var, int_var);  (* works fine *)
  getstring ('12', int_var);     (* works fine *)
  getstring (char_var, int_var); (* incorrect error message *)
  getstring ('1', int_var);      (* incorrect error messages *)
end.
    