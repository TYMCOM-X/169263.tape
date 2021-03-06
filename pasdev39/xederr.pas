module QEDERROR;
$SYSTEM qederr.typ
$SYSTEM qlang.typ
external var prompt : array [qprompts] of string [80];
type textstring = string [80];

public procedure qederror
		(var code : qerrcode; level : qederrlevel);

var
   errorfile : file of textstring;
   lvlidx : qederrlevel;
   startcursor : integer;
   endcursor : integer;
   cursors : textstring;
   strindex : 1..80;

function nxtcrsr () : integer;

begin
while cursors [strindex] <> ' ' do
   strindex := strindex + 1;
getstring (substr (cursors, strindex), nxtcrsr);
end;

begin
if (level = 0) and (code > qfatal) then
   begin
   ttwrite ('?'); ttwtln;
   ttbrk;
   end
else
   begin
   reset (errorfile, [seekok]);
   readrn (errorfile, ord (code) - ord (minimum (qederrlevel)) + 1, cursors);
   strindex := 1;
   if (code = qok) or (code = qfatal) then
      begin
      startcursor := nxtcrsr;
      endcursor := nxtcrsr;
      end
   else
      begin
      for lvlidx := minimum (qederrlevel) to level do
	 begin
	 startcursor := nxtcrsr;
	 endcursor := nxtcrsr;
	 end;
      end;
   if startcursor = 0 then
      begin
      ttwrite (prompt [errmess_not_found]);
      ttwtln;
      end
   else
      begin
      seek (errorfile, startcursor);
      repeat
         ttwrite (errorfile^);
         ttwtln;
	 get (errorfile);
      until cursor (errorfile) = endcursor;
      end;
   end;
end.
   