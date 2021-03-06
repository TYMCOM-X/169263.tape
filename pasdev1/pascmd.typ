(*  Pending Command List Declarations  *)

type
    pending_command = ^ cmd_list_entry;

    cmd_list_entry = packed record
	next: pending_command;
	eof: boolean; (* dummy entry following the last line *)
	too_long: boolean; (* if the line from the file wouldn't fit *)
	len: line_index;
	text: packed array [1..upperbound(line_string)] of char
    end;

    cmd_stack = ^ cmd_stack_entry;

    cmd_stack_entry = record
	next: cmd_stack; (* links the stack nodes *)
	cmd_file_name: file_id (* the pathname of the command file *)
    end;

    tmp_buf_array= packed array [1..40] of char;
    tmp_buffer= ^ tmp_buf_array;
  