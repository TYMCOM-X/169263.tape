$OPTIONS MAIN, KI10, SPECIAL
TYPE
OPTYPE = (ANDOP,OROP,EOROP);
NODETYPE = (LEAF,OPER);
NODEPTR = ^NODE;
NODE = RECORD
SON,BROTHER: NODEPTR;
CASE ID:NODETYPE OF
OPER: (OP: OPTYPE);
LEAF: (NAME: CHAR; NOTFLG: BOOLEAN)
END;
VAR
CH: CHAR; TNODE: NODEPTR;
FUNCTION EXPR(VAR ENODE:NODEPTR; NF: BOOLEAN): BOOLEAN;
VAR LNODE,RNODE: NODEPTR; R: BOOLEAN;
PROCEDURE COMBINE(NOP:OPTYPE; VAR NNODE:NODEPTR; LNODE:NODEPTR; RNODE:NODEPTR);
(* CREATES NEW OPERATOR NODE (NOP), AND CHECKS FOR AND PERFORMS POSSIBLE
COMBINATION WITH LEFT NODE (LNODE) AND RIGHT NODE (RNODE) IF OPERATOR SAME*)
PROCEDURE APPEND(FATHER:NODEPTR; FIRSTSON:NODEPTR);  (*ADDS SONS*)
BEGIN
  IF FATHER^.SON = NIL THEN FATHER^.SON:= FIRSTSON
  ELSE  (*APPEND THESE SON(S) TO CURRENT SON(S)*) BEGIN
   FATHER:= FATHER^.SON;  (*FIND LAST SON, USING "FATHER" AS LOOP PTR*)
   WHILE FATHER^.BROTHER <> NIL DO FATHER:= FATHER^.BROTHER;
   FATHER^.BROTHER:= FIRSTSON
  END
 END  (*APPEND*);
BEGIN  (*COMBINE*)
  NNODE:= NIL;
  IF LNODE^.ID = OPER THEN (*CHECK FOR POSSIBLE COMBINATION*)
   IF LNODE^.OP = NOP THEN  (*MAKE NNODE INTO LNODE*)
    BEGIN NNODE:= LNODE; LNODE:= NIL END;
  IF RNODE^.ID = OPER THEN  (*CHECK FOR COMBINATION WITH RIGHT NODE*)
   IF RNODE^.OP = NOP THEN  (*COMBINE*)
   BEGIN
    IF NNODE = NIL THEN  (*DIDN'T USE LEFT NODE, MAKE NNODE INTO RNODE*)
 NNODE:= RNODE
    ELSE  (*SHOULD DISPOSE RNODE AFTER APPEND HERE*)
     APPEND(NNODE,RNODE^.SON);
    RNODE:= NIL  (*IN ANY CASE, CREAM RNODE*)
   END;
  IF NNODE = NIL THEN  (*DIDN'T USE EITHER, CREATE NEW NODE*)
  BEGIN
   NEW(NNODE,OPER);
   WITH NNODE^ DO
   BEGIN
    SON:= NIL; BROTHER:= NIL; OP:= NOP
   END
  END;
  (* APPEND UNUSED NODES (IF USED, PTRS ARE NIL, SO APPEND IS NULL *)
  APPEND(NNODE,LNODE);
  APPEND(NNODE,RNODE)
END  (*COMBINE*);
FUNCTION FACTOR(VAR FNODE:NODEPTR; NF:BOOLEAN): BOOLEAN;
VAR LNODE,RNODE: NODEPTR; R: BOOLEAN;
FUNCTION OPERAND(VAR ONODE:NODEPTR; NF:BOOLEAN): BOOLEAN;
VAR R: BOOLEAN;
BEGIN  (*OPERAND*)
  IF NOT EOLN(TTY) THEN READ(TTY,CH) ELSE CH:= ' ';
  CASE CH OF
 '(': BEGIN
   R:= EXPR(ONODE,NF);
   IF R THEN
   BEGIN
    IF CH<>')' THEN R:=FALSE ELSE IF NOT EOLN(TTY) THEN
     READ(TTY,CH) ELSE CH:= ' '
   END
 END  (*PAREN CASE*);
 '#': R:= OPERAND(ONODE,NOT NF);
 OTHERS: IF UPPERCASE(CH) IN ['A'..'Z','0'..'9'] THEN BEGIN
 NEW(ONODE,LEAF);  (*ALLOCATE LEAF NODE*)
 WITH ONODE^ DO
  BEGIN
  SON:= NIL; BROTHER:= NIL; NAME:= CH; NOTFLG:= NF
   END;
  IF NOT EOLN(TTY) THEN READ(TTY,CH) ELSE CH:= ' ';
  R:= TRUE
 END  (*GOOD LEAF*)
 ELSE R:= FALSE  (*BAD LEAF*)
 END  (*CASE*);
OPERAND:= R   (*RETURN RESULT*)
 END  (*OPERAND*);
BEGIN (*FACTOR*)
  R:= OPERAND(LNODE,NF);
 IF R THEN IF CH<>'&' THEN FNODE:= LNODE ELSE BEGIN
R:= FACTOR(RNODE,NF);
IF R THEN
  IF NF THEN COMBINE(OROP,FNODE,LNODE,RNODE)  (*INVERT OPERATION*)
  ELSE COMBINE(ANDOP,FNODE,LNODE,RNODE)
 END;
FACTOR:= R
END  (*FACTOR*);
BEGIN  (*EXPRESSION*)
  R:= FACTOR(LNODE,NF);
  IF R THEN IF CH<>'/' THEN ENODE:= LNODE
  ELSE
  BEGIN
  R:= EXPR(RNODE,NF);
  IF R THEN
   IF NF THEN COMBINE(ANDOP,ENODE,LNODE,RNODE)
   ELSE COMBINE(OROP,ENODE,LNODE,RNODE)
  END;
 EXPR:= R
END  (*EXPRESSION*);
PROCEDURE PRINT(ROOT: NODEPTR);  (*PRINTS EXPRESSION IN INFIX*)
 VAR SIBLING: NODEPTR;
BEGIN
IF ROOT^.ID = LEAF THEN
 BEGIN
  IF ROOT^.NOTFLG THEN WRITE(TTY,'#');
  WRITE(TTY,ROOT^.NAME)
END  (*LEAF*)
ELSE
BEGIN
WRITE(TTY,'(');
SIBLING:= ROOT^.SON;
WHILE SIBLING<>NIL DO
BEGIN
PRINT(SIBLING);
SIBLING:= SIBLING^.BROTHER;
IF SIBLING<>NIL THEN
IF ROOT^.OP=ANDOP THEN WRITE(TTY,'&')
ELSE WRITE(TTY,'/')
END;  (*WHILE*)
WRITE(TTY,')')
END  (*OPERATOR *)
END  (*PRINT*);
BEGIN  (*MAIN LINE PROGRAM*)
  REWRITE (TTY); OPEN (TTY);
LOOP
  WRITE (TTY,'*');
  BREAK (TTY);
  READLN (TTY);
  IF NOT EOLN (TTY) THEN BEGIN
    IF EXPR(TNODE,FALSE) THEN
  IF CH=' ' THEN BEGIN PRINT(TNODE); WRITELN(TTY) END
    ELSE WRITELN(TTY,'JUNK ON END OF LINE')
    ELSE WRITELN(TTY,'ERROR IN EXPRESSION');
  END
END
END.
