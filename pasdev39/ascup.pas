program upload options special;

(*****************************************************************)
(* This program reads packed blocks from the terminal, separates *)
(* individual records and writes them to an output file.  The    *)
(* program runs on a DEC 10.                                     *)
(*                                                               *)
(* On 18 August 1982, W.N.H. keyed this in with QED, trans-      *)
(* lating Motorola Pascal into MDSI Pascal on the fly.           *)
(* The Motorola source code is on the EXORmacs under the         *)
(* name 313..ULOAD.SA.                                           *)
(*****************************************************************)

const
   cmax = 80;		(* Block size *)
   rmax = 255;		(* Maximum record size *)
   blkcnt = 1;		(* Number of blocks under one checksum *)
   bmax = blkcnt*cmax;		(* Buffer size = blkcnt*cmax *)
   option = 'AA';	(* com/char protocol options *)
   chrmax = 127;	(* maximum character value *)

type
   cindex = 0..255;
   rectype = array [1..rmax] of char;
   bftype = packed array [1..bmax] of char;
   bfindex = integer;
   bktype = packed array [1..cmax] of char;

var
   q : Boolean;
   outfile : text;	(* output file *)
   outfilename : file_name;
   good_open : Boolean;
   ascii : packed array [char] of cindex;
			(* ASCII character codes *)
   ashex : packed array [1..36] of char;
			(* for hex conversion *)
   num : packed array [char] of cindex;
			(* ASCII-decimal conversion *)
   rec : rectype;	(* holds output record *)
   buffp : bfindex;	(* index into holding buffer *)
   buff : bftype;	(* holding buffer *)
   count : integer;	(* holds char count for record *)
   response : char;	(* holds response char *)
   bcnt : integer;	(* number of repeated characters *)
   bchr : char;		(* repeated char *)
$IF TYMSHARE
$SYSTEM (pasdev2)uuocal
$ENDIF
$IF TYMSHARE
$PAGE auto_cr_lf
(*  AUTO CR LF will turn on or off the operating system switch which
    controls the automatic generation of a cr-lf following the 80-th
    character of each line.  It returns the old value of the switch.  *)

function auto_cr_lf ( on: boolean ): boolean options special;

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
$PAGE ttyread
function ttyread () : string [bmax];
   var str : string [bmax];

   begin (* ttyread *);
   if eoln (tty) then readln (tty);
   read (tty, str);
   ttyread := str;
   end (* ttyread *);

procedure thumb_twiddle;
var twiddles : 1..500000;
begin
for twiddles := 1 to 500000 do;
end;
$PAGE initialize
procedure initialize;
   type charray = packed array [1..16] of char;

   var
      i : integer;
      c : char;

   procedure asgcode (code : integer; chrset : charray);
      var i : integer;

      begin (* asgcode *)
      for i := 1 to 16 do
         ascii [chrset [i]] := code + i - 1;
      end; (* asgcode *)

   begin (* initialize *)
$IF TYMSHARE  if auto_cr_lf ( false ) then;
   for i := 0 to chrmax do
      begin
      ascii [chr (i)] := 0;	(* indicate non-ASCII characters *)
      num [chr (i)] := 0;
      end;

   repeat
      writeln (ttyoutput, 'Enter receiving file name.');
      break (ttyoutput);
      outfilename := ttyread ();
      rewrite (outfile, outfilename, [retry]);
      good_open := iostatus = io_ok;
      if not good_open then
         begin
         writeln (ttyoutput, 'Open failure on receiving file.');
         end;
   until good_open;
   outfilename := filename (outfile);

   ashex := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
   asgcode (112, 'pqrstuvwxyz{|}~ ');
   asgcode (32, ' !"#$%&''()*+,-./');
   asgcode (48, '0123456789:;<=>?');
   asgcode (64, '@ABCDEFGHIJKLMNO');
   asgcode (80, 'PQRSTUVWXYZ[\]^_');
   asgcode (96, '`abcdefghijklmno');
   for i := 1 to 36 do
      num [ashex[i]] := i - 1;
   buffp := blkcnt * cmax + 1;
   buff [1] := 'A';
   write (ttyoutput, 'PREAMBLEU');
   write (ttyoutput, option);
   write (ttyoutput, cmax:3);
   writeln (ttyoutput, blkcnt:1);
   break (ttyoutput);
   response := ' ';
   end;  (* initialize *)
$PAGE getrec
procedure getrec (var q : boolean; var p : bfindex;
                  var buff : bftype; var rec : rectype;
                  var response : char);

var i, j : integer;
   c1, c2 : char;
$PAGE getframe (within getrec)
   procedure getframe (var buff : bftype; var q : boolean;
                       var response : char);

   var
      i : integer;
      j : integer;
      k : integer;
      cksum1 : integer;
      cksum2 : integer;
      tblock : bktype;

      begin (*getframe *)

      if buff [3] = 'Z' then
         q := true;
      if not q then
         repeat
            i := 1;
            j := 1;
            if response <> ' ' then
               begin
               thumb_twiddle;
               writeln (ttyoutput, response); (* Respond to last frame *)
               break (ttyoutput);
               end;
            while j <= blkcnt do
               begin
               tblock := ttyread ();
               j := j + 1;
               for k := 1 to cmax do
                  begin
                  buff [i] := tblock [k];
                  i := i + 1;
                  end;
               if buff [1] = 'T' then
                  j := blkcnt + 1; (* forced termination *)
               end;
            q := buff [1] = 'T';
            if not q then
               begin
	       response := 'N';
               cksum1 := 16 * num [buff[1]] + num[buff[2]];
               if cksum1 <> 0 then
                  begin
                  cksum2 := 0;
                  for i := 3 to blkcnt * cmax do
                     cksum2 := cksum2 + ascii[buff[i]];
                  cksum2 := (cksum2 mod 255) + 1;
                  if cksum1 <> cksum2 then
                     response := 'R';    (* NAK *)
                  end;
               end;
         until (response = 'N') or (q);
      end; (* getframe *)
$PAGE getc (within getrec)
   procedure getc (var c : char; var buff : bftype; var p : bfindex;
                   var q : Boolean);

      begin (* getc *)
      if p <= blkcnt * cmax then
         begin
         c := buff [p];
         p := p + 1;
         end
      else
         begin
         getframe (buff, q, response);
         c := buff [4];
         p := 5;
         end;
      end; (* getc *)
$PAGE getrec (body)
   begin (* getrec *)
   getc (c1, buff, p, q);
   getc (c2, buff, p, q);
   if c1 = ' ' then q := true;
   if not q then
      begin
      count := num [c1] * 16 + num [c2];
      j := 1;
      i := 1;
      while i <= count do
         begin
         getc (rec [j], buff,p,q);
         if rec [j] = '|' then
            begin
            getc (bchr, buff, p, q); (* get compressed character *)
            getc (c1, buff, p, q);   (* get count code *)
            bcnt := num [c1];
            while bcnt > 0 do
               begin
               rec [j] := bchr;      (* expand compressed characters *)
               bcnt := bcnt - 1;
               j := j + 1;
               end;
	    i := i + 3;               (* advance get counter *)
	    end
	 else
	    begin
	    i := i + 1;               (* advance get counter *)
	    j := j + 1;
	    end;
	 end;
      count := j - 1;
      end;
   end; (* getrec *)
$PAGE putrec
procedure putrec (var count : integer; var rec : rectype);

   var i : integer;

   begin (* putrec *)
   i := 1;
   while i <= count do
      begin
      write (outfile, rec [i]);
      i := i + 1;
      end;
   writeln (outfile);
   end; (* putrec *)
$PAGE ascup
begin (* main *)
open (tty); rewrite (ttyoutput);
initialize;
q := false;
getrec (q, buffp, buff, rec, response);
while not q do
   begin
   putrec (count, rec);
   getrec (q, buffp, buff, rec, response);
   end;
thumb_twiddle;
writeln (ttyoutput, 'T');
end. (* main *)
    