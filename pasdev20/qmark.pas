$PAGE includes
$INCLUDE qerr.typ		(* QED error codes *)
$INCLUDE qstr.typ		(* QED string definitions *)
$INCLUDE qspat.typ		(* search pattern definitions *)
$INCLUDE qspred.typ		(* string predicate definitions *)
$INCLUDE cmdutl.typ		(* file_id definitions, etc. *)
$INCLUDE qedln.typ
$INCLUDE QSPRED.INC
$INCLUDE QSPAT.INC
$INCLUDE QEDLN.INC
$PAGE qmarkmatch
public procedure qmarkmatch
(	var buffer: qbuffer;			(* working buffer *)
	mark: spred;				(* markstring to search for *)
	sect_name: spred;			(* in conjunction with mark *)
	start: qlineno;				(* where to start looking *)
	var fno, lno: qlineno;			(* limits of the marked section *)
        backward: boolean;                      (* backward search flag *)
	wrap: boolean;				(* wrap/no-wrap around flag *)
	var err: qerrcode);			(* error report *)

var
  lineno: qlineno;
  line: qstring;
  increment: -1..1;

begin
  lineno := start;
  if backward then increment := -1
              else increment := 1;
  loop
    line := qgetline (buffer, lineno, err);
    if err <> qok then return;
  exit if spredmatch (line, mark, err) andif spredmatch (line, sect_name, err);
    if err <> qok then return;
    lineno := lineno + increment;
    if (lineno > buffer.hbound) and wrap then lineno := buffer.lbound
      else if (lineno < buffer.lbound) and wrap then lineno := buffer.hbound;
    if lineno = start then
    begin
      err := qnomark;
      return
    end
  end;
  fno := lineno;
  if lineno <> buffer.hbound then
  begin
    loop
      lineno := lineno + 1;
      line := qgetline (buffer, lineno, err);
      if err <> qok then return;
    exit if spredmatch (line, mark, err) do lineno := lineno - 1;
    exit if lineno = buffer.hbound;
      if err <> qok then return
    end;
  end;
  lno := lineno
end.						(* qmarkmatch *)
    