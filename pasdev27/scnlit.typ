(**********   Literal Strings   **********

    Literal strings are character strings from the input file.  They are
    represented internally by pointers of type 'str_ptr', which point to
    strings.  Literal string nodes are created only by Getsymbol in SCNLEX,
    the lexical scanner module.

    Lists of strings are used in several contexts.  A list of strings is
    represented by a 'str_list', which points to a 'str_list_node', which
    contains a 'str_ptr' and a pointer to the rest of the list.

							*********)

type
    str_ptr = ^ packed array [1..*] of char;

    str_list = ^ str_list_node;

    str_list_node = packed record
	str: str_ptr;	(* The string. *)
	next: str_list;	(* The rest of the list. *)
    end;
   