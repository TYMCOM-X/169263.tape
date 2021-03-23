PROGRAM SETDATE;
(* RUN FROM .MIC FILES PRIOR TO LINKING UP NEW COMPILER
   VERSIONS.  WRITES A NEW COPY OF VERSION.MAC WITH THE
   CURRENT DATE TO SERVE AS THE COMPILER'S "COMPDATE". *)

CONST TAB := CHR(11B);
BEGIN
  REWRITE (OUTPUT,'PASVER.MAC');
  WRITELN (TAB,'TITLE',TAB,'VERSION - PASCAL COMPILER "COMPDATE"/VERSION DATE');
  WRITELN;
  WRITELN (TAB,'SEARCH',TAB,'PASSYM');
  WRITELN;
  WRITELN (TAB,'ENTRY',TAB,'DATEOFCOMPILATION');
  WRITELN;
  WRITELN (TAB,'$RELOC');
  WRITELN;
  WRITELN ('; PUBLIC VAR DATEOFCOMPILATION: PACKED ARRAY[1..9] OF CHAR;');
  WRITELN (';');
  WRITELN ('DATEOFCOMPILATION:');
  WRITELN (TAB,'ASCIZ',TAB,'/',DATE:9,'/');
  WRITELN (TAB,'END');
END.
 