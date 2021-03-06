module vaxass;
$system pascal.inc
$system pasfil.inc
$system pasist.inc
external function cv_source_id (source_id): string;
external procedure err_failure;
$PAGE ass_failure
public procedure ass_failure;
var n: nam;
begin
  open (tty);
  if cur_block^.kind = subr_blk
    then n := cur_block^.subr_sym^.name
    else n := cur_block^.id;
  writeln ( tty, 'Internal error at block ' || substr(n^.text,1,n^.len) || 
	     ' line ' || cv_source_id (cur_source));
  writeln ( tty, '   please report with stack trace and line number information' );
  break ( ttyoutput );
(* These "STOP" the compiler, lets let itcontinue for this version
  close (tty);
  err_failure; *)
end.
  