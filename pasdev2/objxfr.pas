(* DLOAD - DEC10 end of EXORmacs file transfer.
   Read lines from a user specified file, compress them, add on a
   checksum byte and send them to the EXORmacs by typing them
   to TTYOUTPUT. *)

program download options special;

const
  cmax    =       132;                    (* max no or chars in one block *)
  rmax      =       132;                    (* max single record size *)
  blkcnt= 1;                      (* number of blocks under one checksum *)
  bmax       =       268;                    (* buffer size = blkcnt*cmax+rmax+2 *)
  option=       'AC';                   (* com/char protocol value *)
  chrmax=        127;                    (* maximum character value *)

type
  bkindex = 1..cmax;
  bfindex = integer;
  bftype  = packed array [1..bmax] of char;
  rsptype = packed array [1..2] of char;
  rectype = packed array [1..rmax] of char;
  obj_byte = 0..255;
  obj_unit = packed array [1..4] of obj_byte;

var
  q  : boolean;
  infil     : file of obj_unit;                             (* source file *)
  ascii : array [char] of integer;   (* ascii character codes *)
  ashex : packed array [1..36] of char;    (* for hex conversion *)
  rec : rectype;                      (* holds raw record *)
  recsiz: integer;                      (* holds length of raw record *)
  buff        : bftype;                       (* buff holds packed block *)
  buffp : bfindex;                       (* index into buffer *)
  response: rsptype;                   (* ack nak response *)
$IF TYMSHARE
$SYSTEM (pasdev2)uuocal
$ENDIF
$IF TYMSHARE
$PAGE auto_cr_lf
(*  AUTO CR LF will turn on or off the operating system switch which
    controls the automatic generation of a cr-lf following the 80-th
    character of each line.  It returns the old value of the switch.  *)

function auto_cr_lf ( on: boolean ): boolean;

type
    characteristics =
      ( lc_dm1, lc_dm2, lc_ecs, lc_pss, lc_obs, lc_hdx, lc_esc, lc_crd, lc_dfr,
	lc_nop, lc_nfc, lc_brk, lc_axc, lc_ncm, lc_hht, lc_lcp, lc_ptm, lc_hff );
    char_set = set of characteristics;

    line_char_record = packed record
      case boolean of
	false: ( bits: char_set;
		 port: 0 .. #o777777 );
	true:  ( value: machine_word );
    end;

var chars: line_char_record;
    x: machine_word;

begin
  chars.value := -1;
  if uuo_call (#o51, 6, 0, address (chars), x) then ;
  auto_cr_lf := not (lc_nfc in chars.bits);
  if auto_cr_lf <> on then begin
    if on
      then chars.bits := chars.bits - [lc_nfc]
      else chars.bits := chars.bits + [lc_nfc];
    if uuo_call (#o51, 7, 0, address (chars), x) then ;
  end;
end;
$ENDIF
$PAGE initialize

procedure initialize;

type
  chrarray = packed array [1..16] of char;

var
  i       : integer;
  input_file_name: file_name;
  good_open: boolean;

procedure asgcode(code:integer; chrset: chrarray);
  
  var
    i: integer;

  begin
    for i := 1 to 16 do
      ascii[ chrset[i] ] := code + i - 1;
  end;

begin                                     (* procedure initialize *)
$IF TYMSHARE  if auto_cr_lf ( false ) then;
$IF P10
$IFNONE (ADP,TYMSHARE) (*$ Enable either switch "ADP" or "TYMSHARE". *)
$ENDIF
$IF VAX (*$ Work out how to set terminal width to 150 on VAX. *)
  for i:= 0 to chrmax do
    ascii[ chr(i) ] := 32;

  repeat
    writeln ( ttyoutput, 'Enter input file name' );
    break ( ttyoutput );
    if eoln ( tty ) then readln ( tty );
    read ( tty, input_file_name );
    if input_file_name = '' then begin
$IF ADP      writeln ( ttyoutput, 'EXIT' );
      stop;
    end;
    reset ( infil, '.RO ' || input_file_name );
    good_open := iostatus = io_ok;
    if not good_open then begin
      writeln ( ttyoutput, 'Open failure on input file' );
    end;
  until good_open;

  ashex := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  asgcode( 112, 'pqrstuvwxyz{|}~ ');
  asgcode( 32, ' !"#$%&''()*+,-./');
  asgcode( 48, '0123456789:;<=>?');
  asgcode( 64, '@ABCDEFGHIJKLMNO' );
  asgcode( 80, 'PQRSTUVWXYZ[\]^_' );
  asgcode( 96, '`abcdefghijklmno' );

  buffp := 4;                                (*  start packing records here *)
  repeat
    write ( ttyoutput, 'PREAMBLED' );      (* send preamble *)
    write ( ttyoutput, option );   (* send option *)
    write ( ttyoutput, cmax:3 );     (* send block size *)
    writeln ( ttyoutput, blkcnt:1 );     (* send block count *)
    break ( ttyoutput );
    if eoln ( tty ) then readln ( tty );
    read ( tty, response );
  until (response[1] = 'N') or (response[1] = 'T');

  if response[ 1 ] = 'T'
    then q := true;                   (* early termination *)
end  (* proc initialize *);
$PAGE compress

procedure compress ( var rec: rectype; var recsiz: integer; var buff: bftype;
              var p: bfindex; var ccnt: integer );

var
  i,j : integer;
  recptr: integer;

procedure cout ( c: char );
  begin
    buff[ p ] := c;
    p := p + 1;
    ccnt := ccnt + 1;
  end;

begin
  recptr := 1;
  while recptr <= recsiz do begin
    i := recptr;
    while (i <= recsiz) and (rec[recptr] = rec[i]) and
       (i - recptr < 35) do
      i := i + 1;
    i := i - recptr;                 (* number of matching chars *)
    if (i <= 3) and (i > 0) and (rec[recptr] <> '|') then begin
      for j := 1 to i do cout ( rec[ recptr ] );
      recptr := recptr + i;
    end
    else begin
      if i > 0 then begin
    cout ( '|' );
 cout ( rec[recptr] );
 cout ( ashex[i + 1] );
        recptr := recptr + i;
      end;
    end (* then *);
  end  (* while *) ;
end  (* compress *);
$PAGE getrec
(*  GETREC - read 64 bytes from object file, encode them into printable
    ascii, compress them via run-length encoding and pack them into the
    output buffer.  *)

procedure getrec ( var p: bfindex;  var buff: bftype );

var
  cpos    : bfindex;                      (* position in BUFF for character count *)
  ccnt      : integer;                      (* number of chars in record *)
  i,j  : 0..16;
  obj_element: obj_unit;
  raw_value: obj_byte;


procedure pack ( sentinel: obj_byte; value: obj_byte );

  begin
    if sentinel <> 0 then begin
      recsiz := recsiz + 1;
      rec[ recsiz ] := chr ( sentinel );
    end;
    recsiz := recsiz + 1;
    rec[ recsiz ] := chr ( value );
  end  (* proc pack *) ;

begin
  if not eof ( infil ) then begin
    cpos := p;
    ccnt := 0;
    p := p + 2;
    recsiz := 0;
    for i := 1 to 16 do begin
      read ( infil, obj_element );
      for j := 1 to 4 do begin
        raw_value := obj_element[ j ];
        case raw_value of
     0..#H1f:      pack ( #H7d, raw_value + #H20 );
        #H20..#H7c:   pack ( 0,    raw_value );
       #H7d..#Haf:   pack ( #H7d, raw_value - #H7d + #H40 );
         #Hb0..#Hff:   pack ( #H7e, raw_value - #Hb0 + #H20 );
       end  (* case *) ;
      end  (* for j := 1 ... *) ;
    end  (* for i := 1 ....  *) ;
    compress ( rec, recsiz, buff, p, ccnt );
    buff[ cpos ] := ashex[ ccnt div 16 + 1 ];    (* construct hex char count *)
    buff[ cpos+1 ] := ashex[ ccnt mod 16 + 1 ];
  end  (* then *);
end  (* getrec *);
$PAGE putrec
procedure putrec ( var q: boolean; var p: bfindex; var buff: bftype;
              var response: rsptype );

var
  i : bfindex;

procedure outblock ( rtype: char );

  var
    i, j:   integer;
    c:                integer;                (* checksum *)

  begin                                       (* proc outblock *)
    buff[ 3 ] := rtype;                    (* mark block type *)
    c := 0;
    for i := 3 to cmax * blkcnt do  (* compute checksum *)
      c := c + ascii[ buff[i] ];
    c := c mod 255 + 1;                       (* 0 checksum is wild card *)
    buff[ 1 ] := ashex[ c div 16 + 1];   (* place checksum into block *)
    buff[ 2 ] := ashex[ c mod 16 + 1];
  
    repeat
      for j := 0 to blkcnt - 1 do begin
      for i := j * cmax + 1 to (j + 1)*cmax do  (* write output block *)
      write ( ttyoutput, buff[ i ] );
     writeln ( ttyoutput );
      end;
      break ( ttyoutput );
      if eoln ( tty ) then readln ( tty );
      read ( tty, response );
    until (response[ 1 ] = 'N') or (response[ 1 ] = 'T');
    q := response[ 1 ] = 'T';
    c := blkcnt * cmax + 1;                (* justify any block overflow *)
    while c < p do begin
      buff[ c - blkcnt * cmax + 3] := buff[ c ];
      c := c + 1;                 
    end;
    p := p - (blkcnt * cmax - 3);   (* reset next in pointer *)
  end  (* proc outblock *) ;


begin                                    (* proc putrec *)
  while p > blkcnt * cmax + 1 do
    outblock ( 'A' );                      (* output intermediate block *)
  if eof ( infil ) then begin
    for i := p to blkcnt * cmax do
      buff[ i ] := ' ';                     (* force blanks to end of block *)
    outblock ( 'Z' );                       (* output last block *)
    q := true;
  end
  else begin
    if p > blkcnt * cmax
      then outblock ( 'A' );            (* output intermediate block *)
  end;
end  (* putrec  *);
$PAGE main routine
begin
  open ( tty );
  rewrite ( ttyoutput );
  q := false;
  initialize;
  while not q do begin
    getrec ( buffp, buff );
    putrec ( q, buffp, buff, response );
  end;
end.
  
