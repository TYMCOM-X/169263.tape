program dirtst;
$include dtime.typ
$include dtime.inc[31024,320156]
$include pasdir.typ
$include pasdir.inc[31024,320156]
$include reprot.inc[31024,320156]

var
fn: dir_fname;
pat: dir_m_str;
dir: dir_ext_id;
err: dir_errors;
ch: dir_int_id;
com: char;
att: dir_attrs;
pro: dir_prot;

type acct_scl_const = array [ dir_acct_scls ] of string [ 8 ];
var acct_scl : dir_acct_scls;
const acct_scl_id : acct_scl_const := ('System','Owner','Group','World' );
$PAGE PRINT_PRO
procedure print_pro ( scl : dir_prot_set );

begin
  if dir_read in scl then write ( tty , 'R' );
  if dir_write in scl then write ( tty , 'W' );
  if dir_execute in scl then write ( tty , 'E' );
  if dir_delete in scl then write ( tty , 'D' );
  if scl = [] then write ( tty , ' no access ');
  writeln ( tty )
End;
$PAGE GET_PROT
Function GET_PROT : DIR_PROT;

var str : string[10];
    scl : dir_acct_scls;
    prot_scl : dir_prot_scls;

type scl_const = array [ dir_prot_scls ] of char;
const prot_scl_const : scl_const := ( 'R' , 'W' , 'E' , 'D' );
begin
  for scl := minimum(dir_acct_scls) to Maximum(dir_acct_scls) Do begin
    write ( tty , acct_scl_id[scl], ' protection[RWED]:' );
    break ( tty );
    readln ( tty );
    read ( tty , str );

    get_prot [ scl ] := [];
    str := uppercase ( str );

    for prot_scl := minimum(dir_prot_scls) to maximum(dir_prot_scls) do
      if search ( str , [ prot_scl_const[prot_scl] ] ) <> 0
	then get_prot[scl] := get_prot[scl] + [ prot_scl ];
  end
end;
$PAGE DO_REPROTECT
Procedure DO_REPROTECT;

var pro : dir_prot;
    err : pro_err;
    fn  : file_name;

begin
  write(tty,'Enter file:');
  break ( tty );
  readln ( tty );
  if not eoln(tty)
    then begin
      fn := '';
      while not eoln(tty) do begin
	fn := fn || tty^;
	get ( tty )
      end;
      pro := get_prot;

      reprotect ( fn , pro , err );

      writeln ( tty , 'Err= ', ord(err):2 )
    end
end;
$PAGE MAIN routine
begin
open(tty);rewrite(ttyoutput);
loop
  write(tty,'>>');break(tty);readln(tty);read(tty,com);
  case uppercase(com) of
    'Q': stop;

    'O': begin
      write(tty,'Dir [n]:');break(tty);readln(tty);
      dir:='';
      while not eoln(tty) do begin
	dir:=dir || tty^;
	get(tty)
	end;
      dir_open(err,ch,dir);
      writeln(tty,'err=',ord(err):2, '  ch=',ord(ch));
      end;

    'G':begin
      dir_next(err,ch,fn);
      writeln(tty,'err=',ord(err):2, '  fn=[',fn,']')
      end;

    'A':begin
      write(tty,'Enter file:');break(tty);readln(tty);
      if not eoln(tty) then begin
	fn:='';
	while not eoln(tty) do begin
	  fn:=fn||tty^;
	  get(tty)
	  end
	end;
      dir_attr(err,fn,att);
      writeln(tty,'err=',ord(err):2);
      with att do begin
(*	writeln(tty,'Full xform: ',dc_ext(creation));*)
	writeln ( tty , 'Name [' , name , '].' );
	for acct_scl := dir_system to dir_world do
	  begin
	    write ( tty , acct_scl_id[acct_scl] , ' protected : ' );
	    print_pro ( protect [ acct_scl ] )
	  end;
	writeln(tty,'Cre= ',ns_d2(extr_date(creation)),' at ',
	  ns_t1 ( extr_time ( creation ) ) );
	writeln ( tty , 'Acc = ' , ns_d2(extr_date(accessed)), ' at ' ,
	  ns_t1 ( extr_time ( accessed ) ) );
	writeln ( tty , 'Size = ' , size:6 )
	end
      end;

    'C':begin
      dir_close(err,ch);
      writeln(tty,'err=',ord(err):2)
      end;

    'M':begin
      pat:='';
      write(tty,'Enter pattern:');break(tty);readln(tty);
      while not eoln(tty) do begin
	pat:=pat || uppercase(tty^);
	get(tty)
	end;
      dir_next (ERR,ch,fn);
      if err <> dir_ok then
	writeln(tty,'No directory open.')
      else
	while err = dir_ok do begin
(*	  writeln (tty, 'Returned from dir_next: [',fn, ']');  *)
	  if dir_match (fn,pat) then
	    writeln(tty,'[',fn,'] matches [',pat,']');
	  dir_next(err,ch,fn)
	  end
      end;

    'R' : DO_REPROTECT;

    others: writeln(tty,'NFG command.')
    end
  end
end.
  