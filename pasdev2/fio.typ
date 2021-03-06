const
  max_width = 254;			(* maximum line width *)
  max_plength = 254;			(* maximum page length *)

type
  fio_width = 0..max_width;

  fio_string = string[max_width];

  file_block =
      record
	file_var: text;		(* file variable to operate on *)
	file_title: file_name;		(* true name of file, set on open *)
	pageno: 0..999999;		(* page number of current page *)
	lineno: 0..999999;		(* line number within page (local) *)
	column: 1..max_width;		(* current printing column position. *)
        width: 0..max_width;		(* maximum width of output line *)
	c_column: 0..max_width;	(* continuation column; 0 => truncate *)
	plength: 0..max_plength;		(* maximum length of output page; 0 => no limit *)
	new_page: procedure (var file_block);		(* subroutine to call to perform a "page eject" *)
	page_header: procedure (var file_block)	(* subroutine to call to write a page header *)
      end;
  