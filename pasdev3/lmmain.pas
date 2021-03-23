(* LMMAIN - VAX link management test program.  *)

program lmmain;

$INCLUDE mapimg.typ
$INCLUDE mapimg.inc

external procedure lmm1;

public var
  pub_var: string := 'Public var in main routine';

var
  i: 1..3;
  x: real;
  map_status: mif_status;

public procedure main_proc ( s: string );
  begin
    writeln ( ttyoutput, 'MAIN_PROC: ', s );
  end;

begin
  rewrite ( ttyoutput );
  writeln ( ttyoutput, 'Begin LMMAIN' );
  x := random ( 1 );
  for i := 1 to 9 do write ( ttyoutput, random );
  writeln ( ttyoutput );
  x := random ( 1 );

  for i := 1 to 3 do begin
    writeln ( ttyoutput, random );
    map_image_file ( 'lmm1v1.exe', map_status );
    writeln ( ttyoutput, ord(map_status) );

    if map_status = mif_ok then begin
      lmm1;
      writeln ( ttyoutput, 'Successful return from LMM1V1' );
      map_image_file ( 'lmm1v2.exe', map_status );
      writeln ( ttyoutput, ord(map_status) );
      if map_status = mif_ok then begin
	lmm1;
	writeln ( ttyoutput, 'Successful return from LMM1V2' );
      end;
    end;

  end  (* for *) ;
end (* LMMAIN *) .
