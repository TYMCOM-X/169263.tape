MODULE exp_rpt_process;
$INC EXTYPE.INC
EXTERNAL CONST table_dir : ARRAY [directive] OF STRING;
EXTERNAL VAR data_save : BOOLEAN;
VAR found : BOOLEAN;
VAR s4    : STRING [4];
VAR dir   : directive;
VAR buffer: STRING [85];



PUBLIC FUNCTION command : directive;

  (*this procedure reads a command from the terminal and returns a 
   command name as an enumerated type directive.*)


     BEGIN;
          WRITE (TTY, 'COMMAND?');
          BREAK;
          READLN (TTY, buffer );
          buffer := buffer || 'badc '; (*Concat so always find something*)
          found := FALSE;
          s4 := SUBSTR (buffer, VERIFY (buffer,[' ']), 4);
          s4 := LOWERCASE (s4);
          FOR dir := MINIMUM (directive) TO MAXIMUM (directive) DO
               BEGIN;
                   EXIT IF table_dir [dir] = s4 DO
                        BEGIN;
                             command := dir;
                             found   := TRUE;
                        END;
               END;
          IF NOT found THEN command := badc;
     END;



PUBLIC FUNCTION y_n :CHAR;

   (*this function ask for a y or n, and loops until one or the other 
    is entered.*)

VAR c : CHAR;
BEGIN;
     WRITELN (TTY,''); (*Puts cursor on a new line*)
     WRITE   (TTY, 'Enter a y or n:');
     BREAK;
     READLN  (TTY, c);
     
     WHILE NOT (c IN ['Y', 'y', 'N', 'n']) DO
          BEGIN;
               WRITE (TTY, 'Y or N please:');
               BREAK;
               READLN (TTY, c);
          END;
     y_n := LOWERCASE (c);
END;

PUBLIC PROCEDURE quit_proc (VAR ok : BOOLEAN);

   (*this procedure checks to see if data has been displayed or saved
    and if not ask the person if this is an ok situration.*)


BEGIN;
     IF NOT data_save THEN
          BEGIN;
               WRITELN (TTY, 'No data has been printed or saved. Is'||
                              'this ok?');
               If y_n = 'y' THEN ok := TRUE ELSE ok := FALSE;
          END;
     ELSE
          ok := TRUE;
END;


PUBLIC PROCEDURE init_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'init_proc');
     BREAK;
END;

PUBLIC PROCEDURE entr_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'entr_proc');
     BREAK;
END;

PUBLIC PROCEDURE prnt_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'prnt_proc');
     BREAK;
END;

PUBLIC PROCEDURE save_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'save_proc');
     BREAK;
END;

PUBLIC PROCEDURE load_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'load_proc');
     BREAK;
END;

PUBLIC PROCEDURE help_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'help_proc');
     BREAK;
END;

PUBLIC PROCEDURE badc_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'badc_proc');
     BREAK;
END.
   