external function LIB$DO_COMMAND (command : string [256]): boolean 
  options fortran;
(* Note that the LIB$DO_COMMAND function is declared as an external
   FORTRAN function.  This enables the Cmd parameter to be passed as a
   string descriptor, instead of a value or a var parameter.
*)
 
public procedure DO_CMD (Cmd : string [256]);


var Result : boolean;
begin
  Result := LIB$DO_COMMAND (Cmd);
end.
    