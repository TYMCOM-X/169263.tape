$TITLE passet - Pascal Compiler Bit Set Manipulation Module
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S S E T                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  PASCAL Compiler
     
     STARTED:  9/11/78
     
     PURPOSE:  This is the bit-set manipulation  module.  Vectors  of
        bit  sets  are  used for a number of functions throughout the
        optimizer.  Since these vectors and sets must be  dynamically
        created,  and  their manipulation is repetitive, the creation
        and manipulation of bit set vectors has been  centralized  in
        this module.
     
        Each  bit  set vector is referred to with a descriptor, which
        must be declared with type "svector".  Such a  descriptor  is
        initialized  by NewSvector, and must be passed as a parameter
        to all the other routines in this module.
     
     ENTRY POINTS:
     
        new_svector (n_sets, n_elems)
                    will return a vector  of  bit  sets  (actually  a
                    descriptor  for  such  a vector), containing sets
                    numbered  0  through  n_sets,   each   containing
                    elements numbered 0 through n_elems.
     
        clr_set (v, i)
                    will  set  the  i-th set of vector v to the empty
                    set.
     
        unv_set (v, i)
                    will  set  the  i-th  set  of  vector  v  to  the
                    universal set.
     
        add_elem (v, i, e)
                    will add the specified element to the i-th set of
                    vector v.
     
        del_elem (v, i, e)
                    will delete the specified element from  the  i-th
                    set of vector v.
     
        cpy_set (v, i, j)
                    will  copy the i-th set of vector v into the j-th
                    set.
     
        mov_set (v, i, w, j)
                    will copy the i-th set of vector v into the  j-th
                    set  of vector w.  It is assumed that v and w are
                    vectors of sets over the same range.
     
        union (v, i, j)
                    will set the j-th set of vector v to the union of
                    the i-th and j-th sets.
     
        intersect (v, i, j)
                    will  set  the  j-th  set  of  vector  v  to  the
                    intersection of the i-th and j-th sets.
     
        subtract (v, i, j)
                    will set the j-th set of vector v to the  set  of
                    all  the  elements  which are in the j-th set and
                    are not in the i-th set.
     
        set_eq (v, i, j)
                    will return true if the i-th  and  j-th  sets  of
                    vector v are identical.
     
        in_set (v, i, e)
                    will  return true if the specified element occurs
                    in the i-th set of vector v.
     
        is_empty (v, i)
                    will return true if the i-th set of vector  v  is
                    the empty set.
     
        del_svector (v)
                    will  dispose  of  all the heap storage which has
                    been allocated for vector v.
     
     RESPONSIBLE:  Compiler Group
     
     ---------------------------------------------------------------- *)
$PAGE declarations
$INCLUDE passet.typ

var
    i_set: set_number;
    i_word: word_number;
$PAGE set routines

(*********   NEW SVECTOR   *********)

public function new_svector ( n_sets: set_number; n_elems: elem_number ): svector;
begin
  new_svector.n_sets := n_sets;
  new_svector.n_words := n_elems div 72;
  new (new_svector.sets, n_sets);
  for i_set := 0 to n_sets do
    new (new_svector.sets^[i_set], new_svector.n_words);
end;


(*********   CLR SET   *********)

public procedure clr_set ( v: svector; i: set_number );
begin
  with v do
    for i_word := 0 to n_words do
      sets^[i]^[i_word] := [];
end;

(*********   UNV SET  *********)

public procedure unv_set ( v: svector; i: set_number );
begin
  with v do
    for i_word := 0 to n_words do
      sets^[i]^[i_word] := [0..71];
end;


(*********   ADD ELEM   *********)

public procedure add_elem ( v: svector; i: set_number; e: elem_number );
begin
  i_word := e div 72;
  v.sets^[i]^[i_word] := v.sets^[i]^[i_word] + [e mod 72];
end;


(*********   DEL ELEM   *********)

public procedure del_elem ( v: svector; i: set_number; e: elem_number );
begin
  i_word := e div 72;
  v.sets^[i]^[i_word] := v.sets^[i]^[i_word] - [e mod 72];
end;


(*********   CPY SET   *********)

public procedure cpy_set ( v: svector; i, j: set_number );
begin
  with v do
    for i_word := 0 to n_words do
      sets^[j]^[i_word] := sets^[i]^[i_word];
end;


(*********   MOV_SET   *********)

public procedure mov_set ( v: svector; i: set_number; w: svector; j: set_number );
begin
  with v do
    for i_word := 0 to n_words do
      w.sets^[j]^[i_word] := sets^[i]^[i_word];
end;


(*********   UNION   *********)

public procedure union ( v: svector; i, j: set_number );
begin
  with v do
    for i_word := 0 to n_words do
      sets^[j]^[i_word] := sets^[i]^[i_word] + sets^[j]^[i_word];
end;


(*********   INTERSECT   *********)

public procedure intersect ( v: svector; i, j: set_number );
begin
  with v do
    for i_word := 0 to n_words do
      sets^[j]^[i_word] := sets^[i]^[i_word] * sets^[j]^[i_word];
end;


(*********   SUBTRACT   *********)

public procedure subtract ( v: svector; i, j: set_number );
begin
  with v do
    for i_word := 0 to n_words do
      sets^[j]^[i_word] := sets^[j]^[i_word] - sets^[i]^[i_word];
end;


(*********   SET EQ   *********)

public function set_eq ( v: svector; i, j: set_number ): boolean;
begin
  set_eq := true;
  with v do
    for i_word := 0 to n_words do
      exit if sets^[i]^[i_word] <> sets^[j]^[i_word] do
	set_eq := false;
end;


(*********   IN SET   *********)

public function in_set ( v: svector; i: set_number; e: elem_number ): boolean;
begin
  in_set := (e mod 72) in v.sets^[i]^[e div 72];
end;


(*********   IS EMPTY   *********)

public function is_empty ( v: svector; i: set_number ): boolean;
begin
  is_empty := true;
  with v do
    for i_word := 0 to n_words do
      exit if sets^[i]^[i_word] <> [] do
	is_empty := false;
end;


(*********   DEL SVECTOR   *********)

public procedure del_svector ( v: svector );
begin
  for i_set := 0 to v.n_sets do
    dispose (v.sets^[i_set]);
  dispose (v.sets);
end.
   