$TITLE pasval -- Pascal Compiler Value Node Creation Routines
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S V A L                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  PASCAL Compiler
     
     STARTED:  11/14/78
     
     PURPOSE:  This module contains  the  utility  routines  for  the
        creation of value nodes in the symbol table.
     
     ENTRY POINTS:
     
        makestring  is called with a length, and returns a value node
                    (VAL node) which  has  been  created  to  hold  a
                    string constant of the specified length.
     
        makeset     is  called with a pair of integers, and returns a
                    value node (VAL node) which has been  created  to
                    hold  a  set  constant  with elements between the
                    specified lower and upper bounds.
     
        makearray   is called with an element count,  and  returns  a
                    value  node  (VAL node) which has been created to
                    hold an array constant with the specified  number
                    of elements.
     
        makerecord  is  called  with  an element count, and returns a
                    value node (VAL node) which has been  created  to
                    hold  a record constant with the specified number
                    of fields.
     
        cst_scalar  is called with an integer, and  returns  a  value
                    node  (VAL  node)  containing  that  integer as a
                    scalar constant.
     
        mkreal      is called with a real number and a precision, and
                    returns  a value node (VAL node) whose value is a
                    real value node with the specified value.
     
     NOTES:  These  routines  were  originally  part  of  the  PASUTL
        module.  They  were  split  out  because they are used by the
        constant expression folding code in the  second  pass,  while
        the  rest  of  the PASUTL routines are only used in the first
        pass.
     
     RESPONSIBLE:  Compiler Group
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE pascal.inc
$INCLUDE pasist.inc
$PAGE makestring

(*  MAKESTRING returns a string constant table node which is big enough
    to store a string of size 'len'.  *)

public function makestring ( len: char_range ): val;

begin
  makestring.kind := string_cst;
  new (makestring.valp,string_cst,len);
  with makestring.valp^ do begin
    def_addr := nil;
    str_varying_ref := false;			(* No need for length field known yet. *)
  end;
end (* makestring *);
$PAGE makeset

(*  MAKESET returns a set constant table node which is big enough
    to store a set with elements ranging from 'minbit' to 'maxbit'.  *)

public function makeset ( minbit, maxbit: integer ): val;

var size: bit_range;

begin
  if maxbit >= minbit
    then size := maxbit - minbit + 1
    else size := 0;
  makeset.kind := set_cst;
  new (makeset.valp,set_cst,size-1);
  with makeset.valp^ do begin
    def_addr := nil;
    set_origin := minbit;
  end;
end (* makeset *);
$PAGE makearray


(*  MAKEARRAY returns an array constant table node which is big enough to
    hold 'n_comps' components.  *)

public function makearray ( n_comps: integer ): val;

begin
  makearray.kind := array_cst;
  new (makearray.valp,array_cst,n_comps);
  makearray.valp^.def_addr := nil;
end (* makearray *);
$PAGE makerecord

(*  MAKERECORD returns a record constant table node which is big enough to
    hold 'n_fields' fields.  *)

public function makerecord ( n_fields: integer ): val;

begin
  makerecord.kind := record_cst;
  new (makerecord.valp,record_cst,n_fields);
  makerecord.valp^.def_addr := nil;
end (* makerecord *);
$PAGE cst_scalar
(*  CST_SCALAR returns a scalar constant table node containing the
    specified scalar value.  *)

public function cst_scalar ( svalue: integer ): val;

begin
  cst_scalar.kind := scalar_cst;
  cst_scalar.ival := svalue;
end (* cst_scalar *);
$PAGE mkreal
(*  MKREAL returns a real constant table node containing the specified real
    value.  *)

public function mkreal ( rval: real_type; rprec: prec_type ): val;

begin
  mkreal.kind := real_cst;
  new (mkreal.valp, real_cst);
  with mkreal.valp^ do begin
    def_addr := nil;
    real_val := rval;
    real_prec := rprec;
  end;
end (* mkreal *).
