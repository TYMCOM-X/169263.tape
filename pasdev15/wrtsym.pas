$LENGTH 44
(*$M-,D-*)
 
$INCLUDE USETYP.INC
 
 
type 	FILENAME = string[30];


var	SYMFILE : text;   I : INTEGER;
 
 
 
public procedure BLDINIT(SYMFNAME : SYMBOLIC);
begin
rewrite(SYMFILE, '.SYM ' || SYMFNAME)
end;
 
public procedure BLDCLS;
begin
writeln(SYMFILE,'END');
close(SYMFILE)
end;
 
public procedure SETUPCODE;
begin
writeln(SYMFILE, 'TWOSEG');
writeln (SYMFILE, 'title PASTV.');
writeln(SYMFILE, 'OPDEF	RESCAL[33000,,0]')
end;
 
public procedure RELOC(WHERE : INTEGER);
begin
writeln(symfile,'RELOC   ', WHERE:6:O)
end;
 
public procedure CONSTANT(LEFT, RIGHT : INTEGER);
begin
writeln(SYMFILE, LEFT:6:O, ',,' , RIGHT:6:O)
end;
 
public procedure JRST(NAME : SYMBOLIC);
begin
writeln(SYMFILE, 'EXTERN	  ',NAME);
writeln(SYMFILE, 'JRST            ', NAME)
end;
 
public procedure DEFINE(NAME : SYMBOLIC;  LOCATION : INTEGER);
begin
writeln(SYMFILE, NAME,  '=:', LOCATION:6:O)
end;
 
public procedure PUSHJ(NAME : SYMBOLIC);
begin
writeln(SYMFILE, 'PUSHJ   17,     ', NAME || '##')
end;
 
public procedure CALLIT(NAME : SYMBOLIC);
begin
write(SYMFILE,NAME ||  '::')
end;
 
public procedure FNAME(NAME : FILENAME);
begin
writeln(SYMFILE, 'ASCII/'||NAME||'/')
end;
 
public procedure UUORES(NAME: SYMBOLIC);
begin
writeln(SYMFILE, 'RESCAL	0,	', NAME||'##')
end;

public procedure BLOCK(HOWBIG : INTEGER);
begin
writeln(SYMFILE, 'BLOCK   ^D', HOWBIG);
writeln(SYMFILE, 'RELOC		.-^D',HOWBIG)
end.
    