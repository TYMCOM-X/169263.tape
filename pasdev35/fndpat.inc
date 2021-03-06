$PAGE pattern manipulation declarations
(*  All patterns and regular expressions are represented by PATTERN pointers.  *)

type
    pattern = ^ pattern_node;


(*  InitPatterns creates the predefined pattern constants.  *)

external procedure initre;


(*  These are the predefined pattern constants.  *)

external var
    lambda, (* Matches the empty string ( "" ). *)
    phi, (* Doesn't match any string. *)
    anychar, (* Matches any single character ( . ). *)
    anystring: pattern; (* Matches any string ( .* ). *)


(*  These are the functions for constructing patterns.  *)

external function literal ( char ): pattern; (* "x" *)
external function lit_range ( char; char ): pattern; (* "x".."y" *)
external function catenate ( pattern; pattern ): pattern; (* X Y *)
external function alternate ( pattern; pattern ): pattern; (* X | Y *)
external function iterate ( pattern ): pattern; (* X* *)
external function conjoin ( pattern; pattern ): pattern; (* X & Y *)
external function negate ( pattern ): pattern; (* X' *)


(*  This function tests whether a pattern represents a simple literal (i.e.,
    a single character), and returns the character value if it does.  *)

external function litval ( pattern; var char ): boolean;


(*  These are the procedures for manipulating reference counts.  *)

external procedure free_re ( pattern );
external procedure use_re ( pattern );
    