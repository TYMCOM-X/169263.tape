(* QEDERR.TYP - begun 9/8/82 by WNH *)

(*      This file is to be used by the compiler in compiling the types used
   in connection with error reporting in QED, and by a program which provides
   files of error messages in languags other than English. *)

TYPE
  qederrlevel = 1..10;

(*      The type QERRCODE contains permissible values of parameters to a
   procedure QEDERR, which displays, on a file which is also a parameter
   to QEDERR, a message which explains why QED balked at some action.
   A third parameter to QEDERR is a level (see type QEDERRLEVEL, above).
   The level is used by QEDERR to decide which of several possible
   messages to display.  In general, a message associated with a higher
   level is more verbose than one with a lower level.  Message texts may be
   in several languages and/or formats.  The formats are of type QLANGS.
   The file QLANGS.TYP contains a type definition for type QLANGS.  This
   type definition contains a user specified mnemonic to be associated
   with each value of QLANGS.  The message file generating program uses
   the same mnemonics in processing this file.

	In this file, each value of QERRCODE  is followed by a comment.
   The comment contains all message texts for this value of QERRCODE.
   The format of this comment is as follows:

	The initial comment delimiter is followed by two plus signs.
	Each message text is surrounded by single quotes.
	Each message text is preceded by two percent signs, its
		mnemonic, and its level.
   *)

 QERRCODE = (
  QOK,
   (*++ NORMAL RETURN
%% OPS: 1 'No error reported.'
%% ENG: 1 'Text not decided.'
%% FRA: 1 'Je ne sais pas.'
%% DEU: 1 'Ich weiss nicht.'
*)
  QFATAL,
   (*++ INTERNAL ERROR -- FATAL SITUTATION
%% OPS: 1 'Fatal error. You must reenter all edits made after the last write.
   Please save this output and contact your MDSI representative.'
%% ENG: 1 'Text not decided.'
%% FRA: 1 'Je ne sais pas.'
%% DEU: 1 'Ich weiss nicht.'
*)
  QBADLN,
   (*++ NON-EXISTENT LINE NUMBER PASSED
%% OPS: 2 'Line address out of range.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QBADLB,
   (*++ NON-EXISTENT LOWER BOUND PASSED
%% OPS: 2 'First address is out of range.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QBADUB,
   (*++ NON-EXISTENT UPPER BOUND PASSED
%% OPS: 2 'Second address is out of range.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QBADRN,
   (*++ LOWER BOUND > UPPER BOUND
%% OPS: 2 'Second address is before first.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'The line designator specified first in a range of lines is numerically
   greater than the second.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QBADARGNO,
   (*++  WRONG NUMBER OF LA'S IN LD
%% OPS: 2 'Wrong number of addresses.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QBADCMD,
   (*++  BAD COMMAND NAME PARSED
%% OPS: 2 'Invalid command.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOCMD,
   (*++  CMD INVALID IN CONTEXT USED
%% OPS: 2 'Command invalid in this context.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QBADFILE,
   (*++  BAD FILENAME PARSED
%% OPS: 2 'Missing file name.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'A file name given is incorrectly specified; the command used requires
   a file name and one was not specified; or a file was not specified,'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QNOFILE,
   (*++ NON-EXISTENT FILE
%% OPS: 2 'File not found.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOCLOSE,
   (*++ NO CLOSING DELIMITER IN STRING PATTERN
%% OPS: 2 'Closing delimiter not found for search string.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNODEFAULT,
   (*++ DEFAULT PATTERN HAS NOT BEED DEFINED
%% OPS: 2 '// not defined.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNO2NDLA,
   (*++ NO SECOND LINE ADDRESS AFTER COMMA
%% OPS: 2 'Missing line designator after comma.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOINPAREN,
   (*++ INVALID FIELD WITHIN PARENS OF STRING PREDICATE
%% OPS: 2 'Search string expected after parenthesis.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOCLOSPAREN,
   (*++ NO CLOSING PARENTHESIS
%% OPS: 2 'Missing closing parenthesis.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNONOT_OP,
   (*++ INVALID FIELD AFTER "NOT" OPERATOR
%% OPS: 2 'Missing operand after NOT.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNORTOP,
   (*++ INVALID FIELD AFTER "AND" OR "OR" OPERATOR
%% OPS: 2 'Missing operand after AND, OR.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QOFFIRST,
   (*++  SIGNED OFFSET FOUND AS FIRST PART OF LA
%% OPS: 2 'Negative offset must follow other address.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNONUMF,
   (*++ NO NUMERIC FIELD FOUND AFTER '+' OR '-'
%% OPS: 2 'Number expected after "+" or "-".'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QONLYFIRST,
   (*++   '*','$', OR '.' FOUND NOT AS FIRST PART OF LA
%% OPS: 2 '"*", "$", or "." must be first part of line designator.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOPREVIOUS,
   (*++   '*'ONLY ALLOWED IN SECOND HALF OF LD
%% OPS: 2 '"*"must be used after comma.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QSPNOTFND,
   (*++  STRING PREDICATE NOT FOUND IN RANGE
%% OPS: 2 'Search failed.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QOUTRANGE,
   (*++  EVALUATED LD OUT OF BUFFER OR SPECIAL RANGE
%% OPS: 2 'Address out of range.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QSTUPID,
   (*++  UNMATCHABLE PREDICATE DETECTED IN PARSE
%% OPS: 2 'Search will not match any line.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QLNLONG,
   (*++ LINE TOO LONG, FROM SPLIT, SUBSTITUTE
%% OPS: 2 'Line too long.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QQUIT,
   (*++  QUIT COMMAND GIVEN
%% OPS: 2 'Internal error in QED. A QUIT command has been issued. Please
   save this output and contact your MDSI representative.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOMARK,
   (*++  NO LINE MATCHING MARKSTRING WAS FOUND
%% OPS: 2 'Search failed. Section not found.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QBADSUBST,
   (*++  SYNTAX ERROR IN SUBSTITUTE PATTERNS
%% OPS: 2 'No closing delimiter for replacement string.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QBADOPT,
   (*++  BAD OPTION WAS PARSED
%% OPS: 2 'Invalid command option.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QEXTRATXT,
   (*++  EXTRANEOUS TEXT FOLLOWS COMMAND
%% OPS: 2 'Extraneous characters follow command.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'Characters follow the command which are not part of the command.
   If you are trying to enter multiple commands on a line, separate the
   commands with semicolons.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QTOOBIG,
   (*++  INTEGER TOO BIG TO BE PARSED AS LINE NUMBER
%% OPS: 2 'Line number too large.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOFINDPRED,
   (*++  NO SPATTERN GIVEN IN FIND COMMAND
%% OPS: 2 'No search specified for FIND.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOMOVELA,
   (*++  ONE LA EXPECTED AFTER MOVE, ZERO OR TWO FOUND
%% OPS: 2 'Incorrect destination specified for MOVE.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'No destination was specified for MOVE, or the Ld giving the destination
   specified a range of lines.  A single line address is required.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QBADMOVELA,
   (*++  LA WAS WITHIN TEXT BEING MOVED
%% OPS: 2 'Cannot move text into itself.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'The destination of a MOVE is within the range of lines being moved.
   For example, 1,5 MOVE 2.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QNOTOPEN,
   (*++  OUTPUT COMMAND WAS ISSUED WITH NO OPEN FILE
%% OPS: 2 'Output file not open.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'An OPEN command must be issued to designate the output file before
   attempting to OUTPUT text.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QBADPARAM,
   (*++  MISSING OR INVALID SET PARAMETER
%% OPS: 2 'Missing or invalid SET parameter.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOPARAMVAL,
   (*++  NO VALUE GIVEN FOR SET PARAMETER
%% OPS: 2 'No value given for a SET parameter.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QJOINTOOFEW,
   (*++  FEWER THAN TWO LINES GIVEN TO JOIN
%% OPS: 2 'Must join at least two lines.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QNOSPLITPAT,
   (*++  INVALID OR MISSING PATTERN FOR SPLIT
%% OPS: 2 'Invalid or missing pattern for SPLIT.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QEMPTY,
   (*++  BUFFER IS EMPTY
%% OPS: 2 'Buffer is empty.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QTOOSHORT,
   (*++  NOT ENOUGH LEADING SPACE TO INDENT
%% OPS: 2 'Not enough leading space to indent line.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QWRTERR,
   (*++  EOF(F) FALSE AFTER WRITE STATEMENT
%% OPS: 2 'A write to a file or to the terminal failed.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'The operating system returned an error indication when a write
   to a file or the the terminal was attempted.  Possible causes
   include no room on disk or hardware failures.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QLA1NOTFND,
   (*++  SPRED IN FIRST LA NOT FOUND
%% OPS: 2 'String match in first line address failed.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QLA2NOTFND,
   (*++  SPRED IN SECOND LA NOT FOUND
%% OPS: 2 'String match in second line address failed.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QLA1OUTRANGE,
   (*++  FIRST LA OUT OF RANGE
%% OPS: 2 'First line address out of range.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QLA2OUTRANGE,
   (*++  SECOND LA OUT OF RANGE
%% OPS: 2 'Storage capacity exceeded.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'The heap has overflowed while attempting to add to the buffer.
   The last line input was lost.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QBADNTH,
   (*++  BAD Nth OCCURRANCE PATTERN
%% OPS: 2 'An unmatchable Nth occurrence pattern was detected.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'Nth occurrence substitutes are not legal for :string:, @string@, or
   #string# patterns.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QNOINDNO,
   (*++  AMOUNT TO INDENT NOT SPECIFIED
%% OPS: 2 'The amount to indent the line was not specified.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
*)
  QMOVOUTRANGE,
   (*++  DESTINATION OF MOVE OUT OF RANGE
%% OPS: 2 'Line address of destination out of range.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'The line address specifying the destination is not in the currently
   bounded buffer.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QNOINFILE,
   (*++  FAILURE TO OPEN INPUT FILE
%% OPS: 2 'File not found or access not allowed.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'The specified file is either nonexistent or read access is not
   allowed.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QNOOUTFILE,
   (*++  FAILURE TO OPEN OUTPUT FILE
%% OPS: 2 'Output to the file is not allowed.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'The specified file, possibly due to protection codes, cannot be
   rewritten.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
  QNOCOPYLA);
   (*++  ONE LA EXPECTED AFTER COPY, ZERO OR TWO FOUND
%% OPS: 2 'Incorrect destination specified for COPY.'
%% ENG: 2 'Text not decided.'
%% FRA: 2 'Je ne sais pas.'
%% DEU: 2 'Ich weiss nicht.'
%% OPS: 3 'No destination was specified for COPY, or the Ld giving the destination
   specified a range of lines.  A single line address is required.'
%% ENG: 3 'Text not decided.'
%% FRA: 3 'Je ne sais pas.'
%% DEU: 3 'Ich weiss nicht.'
*)
   