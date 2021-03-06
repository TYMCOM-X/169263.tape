
type
    directives = (include, pagelist, doptions, dwidth, dlength, dtitle, heading, system, baddir (* must be last *)
      );

static var
    drctvs: array[directives] of alfa := ( (* include  *)'INCLUDE',
    (* pagelist *)'PAGE',
    (* doptions *)'OPTIONS',
    (* dwidth   *)'WIDTH',
    (* dlength  *)'LENGTH',
    (* dtitle   *)'TITLE',
    (* heading  *)'HEADER',
    (* system   *)'SYSTEM',
    (* baddir   *)'  ');

const
    reserved = 53; (* number of reserved words *)
    reservedp1 = 54; (* same plus 1 *)


type
    rw_array = array[1..reserved] of alfa;
    frw_array = array[1..11 (* alfaleng+1 *)] of 1..reservedp1;
    rsy_array = array[1..reserved] of symbol;
    rop_array = array[1..reserved] of operator;
    ssy_array = array[' '..'_'] of symbol;
    sop_array = array[' '..'_'] of operator;

const
    rw: rw_array := ('IF','DO','OF','TO','IN', (*  1..5  *)
    'OR','ON','BY','END','FOR', (*  6..10 *)
    'VAR','DIV','MOD','SET','AND', (* 11..15 *)
    'NOT','THEN','ELSE','WITH','GOTO', (* 16..20 *)
    'LOOP','CASE','TYPE','FILE','EXIT', (* 21..25 *)
    'PREC','ORIF','ANDIF','BEGIN','UNTIL', (* 26..30 *)
    'WHILE','ARRAY','COBOL','CONST','LABEL', (* 31..35 *)
    'ALGOL','EXTERN','PASCAL','RECORD','DOWNTO', (* 36..40 *)
    'PACKED','OTHERS','REPEAT','PUBLIC','STATIC', (* 41..45 *)
    'MODULE','FORTRAN','FORWARD','PROGRAM','FUNCTION', (* 46..50 *)
    'EXTERNAL','PROCEDURE','INITPROCED'); (* 51..53 *)

    frw: frw_array := (1,1,9,17,28,37,47,50,52,53,54);
    rsy: rsy_array := (ifsy,dosy,ofsy,tosy,relop, (*  1..5  *)
    addop,onsy,bysy,endsy,forsy, (*  6..10 *)
    varsy,mulop,mulop,setsy,mulop, (* 11..15 *)
    notsy,thensy,elsesy,withsy,gotosy, (* 16..20 *)
    loopsy,casesy,typesy,filesy,exitsy, (* 21..25 *)
    precsy,addop,mulop,beginsy,untilsy, (* 26..30 *)
    whilesy,arraysy,cobolsy,constsy,labelsy, (* 31..35 *)
    algolsy,externsy,pascalsy,recordsy,downtosy, (* 36..40 *)
    packedsy,otherssy,repeatsy,publicsy,staticsy, (* 41..45 *)
    modulesy,fortransy,forwardsy,programsy,functionsy, (* 46..50 *)
    externalsy,proceduresy,initprocsy); (* 51..53 *)

    rop: rop_array := (noop, noop, noop, noop, inop, (*  1..5  *)
    orop, noop, noop, noop, noop, (*  6..10 *)
    noop, idiv, imod, noop, andop, (* 11..15 *)
    noop, noop, noop, noop, noop, (* 16..20 *)
    noop, noop, noop, noop, noop, (* 21..25 *)
    noop, orifop, andifop, noop, noop, (* 26..30 *)
    noop, noop, noop, noop, noop, (* 31..35 *)
    noop, noop, noop, noop, noop, (* 36..40 *)
    noop, noop, noop, noop, noop, (* 41..45 *)
    noop, noop, noop, noop, noop, (* 46..50 *)
    noop, noop, noop); (* 51..53 *)

    ssy: ssy_array := (othersy, addop, relop, relop, othersy, (*  space!"#$ *)
    othersy, mulop, othersy, lparent, rparent, (* percent&() *)
    mulop, addop, comma, addop, period, (* *+,-. *)
    mulop, othersy, othersy, othersy, othersy, (* /0123 *)
    othersy, othersy, othersy, othersy, othersy, (* 45678 *)
    othersy, colon, semicolon, relop, relop, (* 9:;<= *)
    relop, notsy, atsign, othersy, othersy, (* >?@AB *)
    othersy, othersy, othersy, othersy, othersy, (* CDEFG *)
    othersy, othersy, othersy, othersy, othersy, (* HIJKL *)
    othersy, othersy, othersy, othersy, othersy, (* MNOPQ *)
    othersy, othersy, othersy, othersy, othersy, (* RSTUV *)
    othersy, othersy, othersy, othersy, lbrack, (* WXYZ[ *)
    othersy, rbrack, arrow, othersy); (* backslash]^_ *)

    sop: sop_array := (noop, orop, geop, neop, noop, (* space!"#$ *)
    noop, andop, noop, noop, noop, (* percent&'() *)
    mul, plus, noop, minus, noop, (* *+,-. *)
    rdiv, noop, noop, noop, noop, (* /0123 *)
    noop, noop, noop, noop, noop, (* 45678 *)
    noop, noop, noop, ltop, eqop, (* 9:;<= *)
    gtop, noop, noop, noop, noop, (* >?@AB *)
    noop, noop, noop, noop, noop, (* CDEFG *)
    noop, noop, noop, noop, noop, (* HIJKL *)
    noop, noop, noop, noop, noop, (* MNOPQ *)
    noop, noop, noop, noop, noop, (* RSTUV *)
    noop, noop, noop, noop, noop, (* WXYZ[ *)
    noop, noop, noop, noop); (* backslash]^_ *)

    