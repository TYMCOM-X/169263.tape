Program qedmsg;

(*       This program makes the prompt files and the error message files
   which are used by QED.  QEDMSG is compiled with QLANG.TYP and with
   QEDERR.TYP.  It also uses both these files as input.  These files have
   been set up to contain the required messages in the required languages
   and formats.

        The type QLANGS is defined in QLANG.TYP.  Each language or format
   desired is represented by one entry in this enumerated type.  Each entry
   is followed by a comment.  Each comment contains a three-character mnemonic
   for its language and two file names.  The first file name is for prompts
   of the associated language, the second for error messages. *)

$SYSTEM qlang.typ
$SYSTEM qederr.typ

type
   textstring = string [80];
   cursors = record
      startmessage : integer;
      endmessage : integer;
      end;

var
   promptfile : array [qlangs] of record
      name : file_name;
      messages : 0..maximum (integer);
      pfile : text;
      end;

   errorfile : array [qlangs] of record
      name : file_name;
      messages : array [qederrlevels] of integer;
      efile : file of textstring;
      end;

   typefile : text;
   mnemonic : array [qlangs] of string [4];
   errindex : array [qlangs, qerrcode, qederrlevels] of cursors;
   idx : qlangs;
$PAGE utilities
procedure abort (msg : textstring);

(* Give a message to the terminal or log file and die. *)

begin
writeln (ttyoutput, msg, ' -- Aborting.');
stop;
end;

function readterm () : textstring;

(* Return the next tty or command file line. *)

begin
if eoln (tty) then readln (tty);
read (tty, readterm);
end;

function getsymbol (var whatfile : text) : textstring;

(* Return the next contiguous block of non-blank characters in whatfile. *)

begin
getsymbol := '';
if eof (whatfile) then
   abort ('Premature end of file on ' || filename (whatfile));
if eoln (whatfile) then readln (whatfile);
while (whatfile^ = ' ') and not eof (whatfile) do get (whatfile);
while (whatfile^ <> ' ') and not eof (whatfile) do
   begin
   getsymbol := getsymbol || whatfile^;
   get (whatfile);
   end;
end;
$PAGE gettypefile
procedure gettypefile;

var
   typefilename : textstring;

begin
writeln (ttyoutput, 'Default type file name is QLANG.TYP.');
write (ttyoutput, 'Enter override or <ret>: ');
break (ttyoutput);
typefilename := readterm;
if length (typefilename) = 0 then
   typefilename := 'QLANG.TYP';
reset (typefile, typefilename);
if iostatus (typefile) <> io_ok then
   abort ('Can''t open ' || typefilename);
end;
$PAGE getnames
procedure getnames;

var
   symbol : textstring;
   idx : qlangs;
   outfilename : textstring;

(* Fill mnemonic array, and put filenames in file arrays. *)

begin
for idx := minimum (qlangs) to maximum (qlangs) do
   begin

   (* Skip text up to next language mnemonic. *)

   repeat
      symbol := getsymbol (typefile);
   until symbol = '(*++';

   mnemonic [idx] := getsymbol (typefile);
   promptfile [idx].name := getsymbol (typefile);
   errorfile [idx].name := getsymbol (typefile);
   end;
end;
$PAGE outfileinit
procedure outfileinit;

var
   idx : qlangs;
   outfilename : textstring;

begin
for idx := minimum (qlangs) to maximum (qlangs) do
   begin
   writeln (ttyoutput, 'Default promptfile name for mnemonic ',
	    mnemonic [idx], ' is ', promptfile [idx].name);
   write (ttyoutput, 'Enter override or <ret>:  ');
   break (ttyoutput);
   outfilename := readterm;
   if length (outfilename) <> 0 then
      errorfile [idx].name := outfilename;
   rewrite (promptfile [idx].pfile, promptfile [idx].name);
   if (iostatus (promptfile [idx].pfile) <> io_ok) then
      abort ('Unable to open ' || promptfile [idx].name);
   end;
end;
$PAGE doprompts
procedure doprompts;

var
   symbol : textstring;
   idx : qlangs;

(* Get messages from typefile, put into language files. *)

begin
while not eof (typefile) do
   begin

   (* Skip text up to next message. *)

   repeat
      symbol := getsymbol (typefile);
      if symbol = '(*++' then
         abort ('Extra mnemonic ''' || getsymbol (typefile) ||
                ''' in type qlangs.');
   until (symbol = '%%') or eof (typefile);

   if not eof (typefile) then
      begin

      (* This is a prompt message line.  Get its mnemonic. *)

      symbol := getsymbol (typefile);
      for idx := minimum (qlangs) to maximum (qlangs) do
	 begin
	 if mnemonic [idx] = symbol then
	    begin
	    promptfile [idx].messages :=
		promptfile [idx].messages + 1;
	    readln (typefile, symbol);
            symbol := substr (symbol, 3, (length (symbol) -3));
	    writeln (promptfile [idx].pfile, symbol);
	    end;
	 end;
      end;
   end;
end;
$PAGE checkpromptstats
procedure checkpromptstats;

var
   numprompts : 0..maximum (integer);
   idx : qlangs;

(* Do prompt stats match up? *)

begin
numprompts := ord (maximum (qprompts)) - ord (minimum (qprompts)) + 1;
for idx := minimum (qlangs) to maximum (qlangs) do
   if promptfile [idx] .messages <> numprompts then
       writeln (ttyoutput, 'Should be ',
		 numprompts:0,
		 ' prompts.  For mnemonic ''',
		 mnemonic [idx],
		 ''' there were ',
		 promptfile [idx].messages:0, '.');
end;
$PAGE closepromptfiles
procedure closepromptfiles;

var
   idx : qlangs;

begin
close (typefile);
for idx := minimum (qlangs) to maximum (qlangs) do
   close (promptfile [idx].pfile);
end;
$PAGE geterrortypefile
procedure geterrortypefile;

var
   typefilename : textstring;

begin
writeln (ttyoutput, 'Default error tye file name is QEDERR.TYP.');
write (ttyoutput, 'Enter override or <ret>: ');
break (ttyoutput);
typefilename := readterm;
if length (typefilename) = 0 then
   typefilename := 'QEDERR.TYP';
reset (typefile, typefilename);
if iostatus (typefile) <> io_ok then
   abort ('Can''t open ' || typefilename);
end;
$PAGE erroroutfileinit
procedure erroroutfileinit;

var
   idx : qlangs;
   qederrlevel : levels;
   outfilename : textstring;

begin
for idx := minimum (qlangs) to maximum (qlangs) do
   begin
   writeln (ttyoutput, 'Default errorfile name for mnemonic ',
	    mnemonic [idx], ' is ', errorfile [idx].name);
   write (ttyoutput, 'Enter override or <ret>:  ');
   break (ttyoutput);
   outfilename := readterm;
   if length (outfilename) <> 0 then
      promptfile [idx].name := outfilename;
   rewrite (errorfile [idx].efile, errorfile [idx].name, [seekok]);
   if (iostatus (errorfile [idx].efile) <> io_ok) then
      abort ('Unable to open ' || errorfile [idx].name);
   seek (errorfile [idx].efile,
         (ord (maximum (qerrcode))) - (ord (minimum (qerrcode))) + 2);
   for qederrlevel := minimum (levels) to maximum (levels) do
      errorfile [idx].messages [qederrlevel] := 0;
   end;
end;
$PAGE doerrors
procedure doerrors;

var
   symbol : textstring;
   temp : string [5];
   idx : qlangs;
   qederrlevel : levels;
   whichmessage : qerrcode;

   procedure messagetofile (whichfile : qlangs;
                            whichmessage : qerrcode);

   var
      symbol : textstring;
      qederrlevel : levels;

   begin
   read (typefile, qederrlevel);
   errorfile [whichfile].messages [qederrlevel] := 
	   errorfile [whichfile].messages [qederrlevel] + 1; 
   errindex [whichfile, whichmessage, qederrlevel].startmessage :=
	   cursor (errorfile [whichfile].efile);
   readln (typefile, symbol);
   symbol := substr (symbol, 3);
   while symbol [length (symbol)] <> '''' do
      begin
      write (errorfile [whichfile].efile, symbol);
      readln (typefile, symbol);
      end;
   symbol := substr (symbol, 1, length(symbol) -1);
   write (errorfile [whichfile].efile, symbol);
   errindex [whichfile, whichmessage, qederrlevel].endmessage :=
	   cursor (errorfile [whichfile].efile);
   end;
$PAGE writeindices
   procedure writeindices;

   (* Write, to each error message file, the indices for its
      error messages. *)

   var
      idx : qlangs;
      qederrlevel : levels;
      symbol : textstring;
      whichmessage : qerrcode;
      whichfile : file of textstring;
      indices : array [qederrlevels] of cursors;

   begin
   for idx := minimum (qlangs) to maximum (qlangs) do
      begin
      whichfile := errorfile [idx].efile;
      seek (whichfile, 1);
      for whichmessage := minimum (qerrcode) to maximum (qerrcode) do
         begin
	 indices := errindex [idx, whichmessage];
	 symbol := '';
	 for qederrlevel := minimum (levels) to maximum (levels) do
            begin
            putstring (temp, indices [qederrlevel].startmessage);
            symbol := symbol || temp;
            putstring (temp, indices [qederrlevel].endmessage);
            symbol := symbol || temp;
	    end;
         write (whichfile, symbol);
	 end;
      end;
   end;
$PAGE doerrors body
(* Get messages from typefile, put into language files. *)
begin
for whichmessage := minimum (qerrcode) to maximum (qerrcode) do
   begin

   (* skip text up to next group of messages. *)

   repeat
      symbol := getsymbol (typefile);
   until (symbol = '(*++');

   repeat
      repeat
	 symbol := getsymbol (typefile);
      until (symbol = '%%') or (symbol = '*)');

      if symbol = '%%' then
         begin

         (* This is the start of an error message. *)

	 symbol := getsymbol (typefile);
	 for idx := minimum (qlangs) to maximum (qlangs) do
	    begin
	    if mnemonic [idx] = symbol then
	       messagetofile (idx, whichmessage);
	    end;
         end;
   until symbol = '*)';

   end;
writeindices;
end;
$PAGE checkerrorstats
procedure checkerrorstats;

var
   numerrors : 0..maximum (integer);
   idx : qlangs;
   count1, count2 : array [qederrlevels] of integer;
   qederrlevel : levels;

(* Do error stats match up? *)

begin
count1 := errorfile [minimum (qlangs)].messages;
for idx := succ (minimum (qlangs)) to maximum (qlangs) do
   begin
   count2 := errorfile [idx].messages;
   for qederrlevel := minimum (levels) to maximum (levels) do
      if count2 [qederrlevel] <> count1 [level] then
         writeln (ttyoutput,
                  'Mnemonic ''', mnemonic [minimum (qlangs)],
		  ''' has ', count1 [qederrlevel]:0,
                  ' error messages for qederrlevel ', level:0,
		  ':  mnemonic ''', mnemonic [idx], ''' has ',
                  count2 [qederrlevel]:0);
   end;
end;
$PAGE qedmsg
begin (* qedmsg *)

(* Initialize file arrays. *)

for idx := minimum (qlangs) to maximum (qlangs) do
   begin
   promptfile [idx].messages := 0;
   end;

gettypefile;
getnames;
outfileinit;
doprompts;
checkpromptstats;
closepromptfiles;

(* Prompt files done.  Now do error files. *)

geterrortypefile;
erroroutfileinit;
doerrors;
checkerrorstats;
end.
   