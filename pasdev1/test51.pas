program test51 options dump;

 condition
   xyz (): real;
   abc (var c: char);

 public condition pubcond: boolean;
 external condition extcond;
 static condition statcond;		(* erroneous *)

 procedure inner;
  condition bad;			(* never at level > 1 *)
  begin
    return;
  end;


begin
  stop
end.

  