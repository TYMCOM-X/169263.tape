(********************************************************************* *)

                      (* QED type definitions *)
                      (* -------------------- *)


const qmaxlines = 99999;                    (* maximum line number *)

type
  qlinep = ^qline;                          (* ptr to line descriptor *)
  qlineno = 0..qmaxlines;                   (* QED line number *)
  qlnoffset = -99999..qmaxlines;            (* line offset *)

  (* BUFFER DESCRIPTOR - callers should only access the following fields:
     curlineno, lbound, hbound, lastlineno, curfile, curfileok, mark, changes.
     All other fields are implementation specific. *)

  qbuffer = record
    firstlinep,                             (* ptr to dummy 0th line desc *)
    lastlinep,                              (* to last line desc *)
    getlinep: qlinep;                       (* ptr to last line gotten *)
    getlineno: qlineno;			    (* line number of last line gotten *)
    curlineno,                              (* current line number *)
    lastlineno: qlineno;                    (* last line number *)
    garblinep: qlinep;			    (* lines to be deleted *)
    lbound,
    hbound: qlineno;                        (* bounded range *)
    lboundp,
    hboundp: qlinep;                        (* pointers to bounds *)
    offset: qlineno;                        (* ANC line bounding offset *)
    oldoffset: qlineno;                     (* offset previous to above *)
    mark: spred;	                    (* boundary mark *)
    curfile: file_id;                       (* name of defaul file *)
    s940: boolean;			    (* true if curfile is 940 *)
    curfileok: boolean;			    (* valid filename in above ? *)
    changes: boolean			    (* unwritten changes in buffer *)
  end;

(********************************************************************* *)
   