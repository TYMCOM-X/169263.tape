Program RNMTST;

(* RENAME TEST program for the VAX rename RDLIB routine *)

$include rename.inc

var str1,str2 : file_name;
    err : rnm_err;

begin
  open ( tty );
  rewrite ( ttyoutput );

  loop
    write ( tty , 'From: ' );
    break ( tty );
    readln ( tty );
    read ( tty , str1 );

  exit if str1 = '';

    write ( tty , 'To: ' );
    break ( tty );
    readln ( tty );
    read ( tty , str2 );

    rename ( str1 , str2 , err );

    if err = rnm_ok
      then writeln ( tty , 'Success.' )
    else writeln ( tty , 'Err: ' , Ord ( err ) )

  end
end.
