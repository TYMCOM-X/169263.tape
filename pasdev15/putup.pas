$LENGTH 44
(*$X-,M-,D-,O-*)
 
(*   +--------------------------------------------------------------+
     I                                                              I
     I                         P U T U P                            I
     I                         - - - - -                            I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 13-Jun-77

     PURPOSE: To provide procedural access and updating to a threaded
        binary tree symbol table such as the one in  USEPRO,  and  to
        walk  the  tree in order to detect undeclared symbols used in
        forward refef undeclared symbols used in  forward  references
        in the MODULES section of an MDL description of a program.

     USAGE:
        LOOKUP(    table : symptr;
                   wr
                   word : PACKED ARRAY[1..10] OF CHAR;
                   kind : kindofrec;
               VAR found : BOOLEAN;
               VAR vaatemp : integer;
               VAR where : symptr);
         
        PUT_ENT(VAR first, last : symptr;
                    which : kindofrec;
                    index : integer;
                    name : PACKED ARRAY[1..10] OF CHAR;
                VAR pointer : symptr);

     INPUT: 

        table      A  pointer to the first element in the binary tree
                   symbol table.

        word       The symbolic name of the symbol to be looked up in
                   the table.

        kind       In the case of areas and modules or symbols having
                   the same name,  which one to accept as found.

        found      Set to true if the described entry is found in the
                   table.

        where      A  pointer  to  the record of the symbol if found.
                   If not found,  a pointer to the node of  the  tree
                   to   which   the  symbol  should  be  attached  to
                   preservveordering.

        first, last   Pointers to the beginning and end of a chain to
                   be  run  through the tree,  for area list,  module
                   list,  symbol  list  for  a  module  or  for   the
                   resident,  and  the  master transfer vector symbol
                   list.

        index      Used to initialize certain values in a newly added
                   record--also  the  same  values returned by LOOKUP
                   through the parameter VALTEMP.

     REQUIREMENTS: All pointers must be initialized  to  NIL  by  the
        caller.  PUT_ENT  will  initialize  all  pointers  in the new
        record to NIL.

     EFFECTS: Will thread area records through  the  field  NEXTAREA;
        symbols with NEXTMODSYM,  and modules with NEXTMOD.

     NOTES: When  placing  the  first  entry  onto  the symbol table,
        POINTER will be NIL ( being  the  record  to  which  the  new
        symbol  should  be  attached).  The caller must be aware that
        this symbol must,  upon return,  be manually attached to  the
        symbol table as the first entry.

     RESPONSIBLE: Jerry Rosen


     CHANGES: NONE.

     ---------------------------------------------------------------- *)
 
$INCLUDE USETYP.INC[52250,247]
 
 
 
$PAGE
public procedure LOOKUP(    TABLE : SYMPTR;
		     WORD : SYMBOLIC;
		     KIND : KINDOFREC;
	         var FOUND : boolean;
		 var VALTEMP : INTEGER;
		 var WHERE : SYMPTR);
var WERE : SYMPTR;
 
begin
WHERE := TABLE;
FOUND := false;
WERE := TABLE;
while (FOUND = false) and (WHERE <> nil) do
     begin
     WERE := WHERE;
     if WHERE^.SYMNAME = WORD then
	  begin
	  if WHERE^.SYMTYPE = KIND then
	       FOUND := true
	  else WHERE := WHERE^.LEFTKIN
	  end
     else if WHERE^.SYMNAME < WORD then
	       WHERE := WHERE^.RIGHTKIN
	  else WHERE := WHERE^.LEFTKIN
     end;
if not FOUND then WHERE := WERE
else case KIND of
 
AREA	:VALTEMP := WHERE^.AINDEX;
OVMODULE	:VALTEMP := WHERE^.MODNUMBER;
SYMBOL	:VALTEMP := 0
	end
end;
 
$PAGE
public procedure PUT_ENT(    var FIRST, LAST : SYMPTR;
		         WHICH : KINDOFREC;
		         INDEX : INTEGER;
		         NAME : SYMBOLIC;
		     var POINTER : SYMPTR);
var NEWPTR: SYMPTR;
 
begin
case WHICH of
 
SYMBOL		:begin
		 NEW(NEWPTR, SYMBOL);
		 with NEWPTR^ do
		      begin
		      NEXTMODSYM := nil;
		      NEXTMAINSYM := nil;
		      MYMODULE := nil;
		      TVLOC := INDEX
		      end;
		 if LAST <> nil then LAST^.NEXTMODSYM := NEWPTR
		 end;
 
AREA		:begin
		 NEW(NEWPTR, AREA);
		 NEWPTR^.NEXTAREA := nil;
		 NEWPTR^.AINDEX := INDEX;
		 NEWPTR^.ORIGIN := 0;
		 if LAST <> nil then LAST^.NEXTAREA := NEWPTR
		 end;
 
OVMODULE		:begin
		 NEW(NEWPTR, OVMODULE);
		 with NEWPTR^ do
		      begin
		      STATORIG := 0;  STATICSIZE := 0;
		      NEXTMOD := nil;
		      FIRSTSYM := nil;
		      LASTSYM := nil
		      end;
		 NEWPTR^.MODNUMBER := INDEX;
		 if LAST <> nil then LAST^.NEXTMOD := NEWPTR
		 end
     end;
 
if POINTER <> nil then
     if POINTER^.SYMNAME < NAME then
		POINTER^.RIGHTKIN := NEWPTR
	   else POINTER^.LEFTKIN := NEWPTR;
POINTER := NEWPTR;
if FIRST = nil then
     FIRST := NEWPTR;
LAST := NEWPTR;
with NEWPTR^ do
     begin
     LEFTKIN := nil;
     RIGHTKIN := nil
     end;
NEWPTR^.SYMNAME := NAME
end;
 
public procedure ENDINORDERWALK(    TABLE : SYMPTR;
			  var TEST : boolean);
begin
if TABLE <> nil then
     begin
     ENDINORDERWALK(TABLE^.LEFTKIN, TEST);
     if (TABLE^.SYMTYPE = SYMBOL) and (TABLE^.TVLOC = 0) then
	  begin
	  writeln(tty,'UNDEF SYMBOL  ', TABLE^.SYMNAME);
	  TEST := false
	  end;
     (*$X
     WRITE(tty,'NAME--', TABLE^.SYMNAME);
     with TABLE^ do
	  case SYMTYPE of
	  AREA  :writeln(tty,'AREA  ', SIZE:6, INDEX:3);
	  OVMODULE:writeln(tty,'MODL  ', STATORIG:5, MODNUMBER:3, AREANUM:3);
	  SYMBOL:writeln(tty,'SYMB  ', MYMODNUM:3, SYMNUM:3)
	  end;    *)
     ENDINORDERWALK(TABLE^.RIGHTKIN, TEST)
     end
end.
