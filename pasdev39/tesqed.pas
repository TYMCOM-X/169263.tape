$page QED -- Front End and Initialization for Our Glorious QEDitor
program qed
  options storage(3072);

const	cr = chr (#o15);			(* Carriage Return	*)
var
  buffer:	qbuffer;			(* working buffer *)
  idx : 0..15;
  promptfile : text;

begin
  open (tty, [ascii]);
  rewrite (ttyoutput);
  tty^ := cr;					(* Initialize fake line end	*)
  qinitexec( buffer );				(* init buffer and 
						   editor parameters *)
  writeln (ttyoutput, 'QED test version of ', compdate, ';');
  writeln (ttyoutput, '   IO through QTERM;');
  writeln (ttyoutput, '   Prompts from disk file;');
$IF P10
$IF TYMSHARE
  qlang ('qpromt.eng[31024,332220]', 'qederr.msg[,320156]');
$END
$IF ADP
   qlang ('qpromt.eng[52250,221]', 'qederr.msg[52250,227]');
$END
$IFNONE (TYMSHARE, ADP)
You must select either TYMSHARE or ADP if compiling for P10.
$END
$END
$IF VAX
  qlang ('[250245.qedtest]qpromt.eng', '[250245.qnc]qederr.msg');
$END
  repeat
    qedcl (buffer, [minimum (qedcmds)..maximum (qedcmds)])
  until (not buffer.changes) orif query ('Unwritten changes, OK')
end.						(* start-up *)
