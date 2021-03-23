(*   +--------------------------------------------------------------+
     I                                                              I
     I                      Q E D E R R O R                         I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 17-Aug-77

     PURPOSE: This prints messages assocaited with a QED error code.

     USAGE:
	QEDERROR (F, CODE, LEVEL);

     INPUT: 

	F	   is the file to write the message is to be written.

	CODE       is  the QED error code for which the message is to
		   be printed.

	LEVEL      is the level of the message desired.  Level 1 is a
		   brief   message;   subsequent   levels  give  more
		   information.

     REQUIREMENTS: It is assumed that the  file  TTYOUTPUT  has  been
	opened before this program is called.

     ALGORITHM: If the level specified is 1 and the code appears in a
	special list (stored locally) then  '?'  is  printed  as  the
	message.   Otherwise,  a  file  is  searched  for  the  first
	message with matching code and level numbers.  A  code  of  *
	matches  any  code.  A  level  is  matched by any level value
	greater or equal to the value  specified.  So,  the  messages
	associated with the same code should be ordered by increasing
	level number.  The format of the file is this:  a  series  of
	messages  separated  by blank line(s).  The last message must
	be followed by a blank line.  A single  message  starts  with
	<code>,<level>.  The  rest is text which is parsed into words
	and output in fill mode to fit the width of the terminal.

     RESPONSIBLE: Software Tools

     CHANGES: 10/07/81 - djm - Added VAX code for opening QEDERR.MSG file.
              05/04/82 - djm - Added M68000 code for opening QEDERR.MSG file.

     ---------------------------------------------------------------- *)


module QEDERROR
  options special;

$PAGE QEDERROR
type qederrlevel = 1..10;

var errfile: text;

public procedure qederror (var f: text; code: qerrcode; level: qederrlevel);

 var msgcode: 0..255;				(* code read from file *)
     msglevel: qederrlevel;			(* level readfrom files *)
     word: string[32];				(* assembled word from message *)
     column: 0..255;				(* output column *)
     errfile_name: file_name;                   (* complete error file name *)
$IF P10 jobstuff: jobrec;                       (* for error file PPN *)
$IF VAX imgerr: image_error;                    (* image_file_name error *)


 (* writes text to TTY, filling to width of terminal *)

 procedure out;
  begin
   if (column + length (word)) > 72 (* typical terminal width *) then begin
     writeln (f);
     column := 0;
     if word  = '' then return
   end;
   column := column + length (word);
   write (f, word)
  end;


 begin
  column := 0;

  if (level = 1) and (code > qfatal) then begin	(* brief message *)
    writeln (f, '?'); break;
    return
  end;

$IF P10
  jobinfo (jobstuff);
  errfile_name := 'QEDERR.MSG' || jobstuff.progdir;
$END
$IF VAX
  image_file_name (errfile_name, imgerr);
  errfile_name := substr (errfile_name, 1, search (errfile_name, [']'] )) ||
                  'QEDERR.MSG';
$END
$IF M68 errfile_name := '1000..QEDERR.MS';
  open (errfile, errfile_name );                        (* get file *)
  if eof (errfile) then begin			(* fatal situation *)
    writeln (f, 'QED error file missing.'); break;
    return
  end;

  loop						(* search for message in file *)
    repeat readln (errfile) until not eoln (errfile) or eof(errfile);	(* skip blanks before start of msg *)
    if eof (errfile) then begin			(* code + level not found *)
      writeln (f, 'Error not found.', ord (code):5); break;
      return
    end;
    if errfile^ = '*'				(* get errcode from file *)
      then begin				(* '*' matches any code *)
	msgcode := ord (code);			(* force match *)
	get (errfile);          (* eat comma following asterisk *)
	get (errfile)		(* and get first digit of following number *)
      end
      else read (errfile, msgcode);
    read (errfile, msglevel);			(* get level from file *)
  exit if (msgcode = ord (code)) and (msglevel >= level);
    repeat readln (errfile) until eoln (errfile);   (* skip til blank line following msg *)
  end;

  repeat					(* output msg *)
    while errfile^ <= ' ' do get (errfile);	(* skip control chars at start of msg *)
    while not eoln (errfile) do begin		(* extract words from line *)
      if errfile^ > ' ' then begin
	word := '';
	repeat
	  word := word || errfile^;
	  get (errfile)
	until errfile^ <= ' ';
	out; word := ' '; out;			(* write word followed by blank *)
      end
      else get (errfile);			(* ignore control chars, i.e. white space *)
    end;
    readln (errfile);				(* go to next line *)
  until eoln (errfile);				(* message terminated by blank line *)
  writeln (f); break;				(* terminate msg *)
  close (errfile);				(* clean up our dirty laundry *)
 end.
 