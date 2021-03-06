type
    line_index = 0 .. 32767;

    eval_pointer = ^ eval_block;
    eval_block = packed record
		   last: eval_pointer;
		   next: eval_pointer;
		   text: eval_text
		 end;
    eval_cursor = packed record
		    block: eval_pointer;
		    index: eval_index
		  end;

    arg_pointer = ^ arg_block;
    arg_block = packed record
		  base: eval_cursor;
		  length: string_index;
		  link: arg_pointer
		end;

    call_pointer = ^ call_block;
    call_block = packed record
		   def: definition;
		   up: call_pointer;
		   down: call_pointer;
		   chain: call_pointer;
		   n_args: arg_index
		 end;
    