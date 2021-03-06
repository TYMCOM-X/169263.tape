$INCLUDE fio.inc[31024,320156]

type title_string = string[80];

(* Lists of files used in compilation and logical sections within those files. *)

type
  source_ptr = ^src_id;
  page_ptr = ^page_id;
  src_id =
      packed record
        file_no: file_range;            (* number assigned to file *)
        pages: page_ptr;                    (* links pages in file *)
        next_file: source_ptr;              (* sequential list *)
        incl_level: 0..max_include_level;   (* inclusion level *)
        incl_page: page_range;              (* physical page on which included *)
        system_file: boolean;
        file_name: packed array [1..*] of char   (* from filename *)
      end;

  page_id =
    packed record
      left_page, right_page: page_ptr;    (* for xref after compilation *)
      next_page: page_ptr;                (* sequential list of pages in a file *)
      following_page: page_ptr;         (* next page in listing *)
      in_file: source_ptr;                (* link to enclosing file *)
      page_number: page_range;            (* "section" number *)
      incl_page: page_range;              (* physical page no for xref *)
        subtitle: packed array [1..*] of char  (* page subtitle if any *)
    end;

(* Defines states of output files which are "open" across passes. *)

type
  file_status =
     (  unopened, (* has never been opened *)
        prev_opened, (* opened in a previous pass, but not in this one *)
        now_open  ); (* file is now open *)

external var
  main_file: file_name;   (* names of important files *)
  list_file: file_name;
  rel_file: file_name;

  list_explicit: boolean; (* list file name from ",list=source" *)

  file_list: source_ptr;                (* start of list of files referenced *)
  no_files: file_range;                 (* number of current file *)

  fin_source: source_id;                (* last line read prior to abort *)

  line: line_string;                    (* uppercased input line + 2 blanks *)
  literal_line: line_string;            (* input line as read *)
  src_on: boolean;                      (* line should be listed *)
  ln_enabled: boolean;                     (* line is enabled for compilation *)
  end_of_file: boolean;                 (* true => last line has been read *)

  src_selected: boolean;                (* true => source on somewhere in program *)
  main_title: title_string;             (* first title which appears in input *)
  global_title: title_string;           (* current title *)
  page_title: title_string;             (* currently applicable $PAGE title *)

  df_status: file_status;               (* indicates current state of dump file *)
  lf_status: file_status;               (* ... of list file *)
  elf_status: file_status;              (* ... of error log file *)

  dumpfb: file_block;                   (* FIO control blocks *)
  listfb: file_block;                   (* .width is current page_width
                                           .plength is current page_length *)
    