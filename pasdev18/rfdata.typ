(* Font definition.  Characters may be in one of several fonts, which need
   not have a fixed width. *)

type fonts =
      (	standard,			(* clear text *)
	underlined,			(* underlined text *)
	boldface    );			(* boldface (diablo only) *)

type points = 0..65535;			(* for type widths, per char, line *)

type character =	(* extended, multi-font character *)
      record
	ch: char;			(* actual letter *)
	font: fonts;			(* font to print in *)
	width: points			(* printing width *)
      end;



(* Line definition.  Text is contructed out of lists of line records which
   contain arrays of extended characters, and other special information. *)

const max_line_length = 254;
type
  line_index = 0..255;			(* line length + 1 *)
  line_number = 0..99999;		(* input line number *)
  line_type = ^line_node;

  line_node =
      record
	next: line_type;		(* for chaining records *)
	indentation: points;		(* printing position of 1st char *)
	spacing: points;		(* vertical spacing in lines *)
	total_width: points;		(* total width of all text *)
	number: line_number;		(* input line number *)
	textlen: line_index;		(* number of text characters *)
	text: array [1..255] of character
      end;

  line_list =		(* for building lists of lines *)
       packed record
	first, last: line_type
       end;



(* Reserved word definition.  This includes both command names and command
   keywords.  This list also includes three dummy codes: one indicating a
   non-reserved word, one indicating a bad command, and last inidicating the
   end of file. *)

type
  command_words =
      ( justifywrd, center, verbatim, pagewrd, skip, paragraph,
	indent, spacing, margin, need, picture, title, footnotes,
	tabs, number,
	left, right, top, bottom, terminal,
	endoffile, nonword, badcmd );

type word_set = set of command_words;	(* words which are commands *)
const command_set: word_set := [justifywrd..number];
type commands = justifywrd..number;


(* States of input text processing. *)

type state =
      (  alignmode, centermode, fillmode, literalmode, rightalignmode );


(* Page format information.  Two copies of this information are kept:
   (1) the current format values which is that actually used to format
   the output into pages, and (2) the values to be become the current
   values at the start of the next page of output. *)

type 
  line_count = 0..120;				(* number of lines per page *)
  page_number = 0..99999;			(* output page count *)

  page_format =
      record
	length: points;		(* length of page in lines *)
	width: points;		(* width of page in characters *)
	top_margin: points;			(* # lines at top of page *)
	bottom_margin: points;		(* # lines at bottom of page *)
	left_margin: points;		(* # columns at left of page *)
	right_margin: points;		(* # columns at right of page *)
	number: page_number		(* number of the page *)
      end;
 