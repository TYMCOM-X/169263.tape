 external function LIB$RUN_PROGRAM ( prg_name: string [256]) : boolean options fortran;
(*
   Note that LIB$RUN_PROGRAM is declared as an external FORTRAN function.
   This results in prg_name being passed to LIB$RUN_PROGRAM as a string
   descriptor, instead of as a value or a var parameter.
*)

 public var auto_run : boolean;

 public procedure RUN ( FN: string [256];
                        PROMPT : boolean);

  var Result : boolean;

  begin
    If PROMPT = true
      then auto_run := true
      else auto_run := false;
    Result := LIB$RUN_PROGRAM (FN);
  end.
   