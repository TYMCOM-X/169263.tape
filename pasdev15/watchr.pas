$options special, nocheck

program watchr;
$length 44
$include rlb:cmdutl.typ
$include rlb:lookup.typ
$include rlb:query.inc

external procedure print_heap;
type
foobar=packed array[1..30] of char;
external procedure hpsave(foobar;integer);
external procedure hpload(foobar; var integer);

type
    commands = (c_dump,c_collapse,c_hold,c_nodispose,c_dispose,c_alternate,
      c_mixed,c_iterate,c_random,c_minimum,c_maximum,c_loop,c_stack,c_unstack,
	c_mark,c_release,c_status,c_save,c_load);
    command_list = array[commands] of cmdlist;

const
    cmd_list: command_list := ( ('DUMP      ',2), ('COLLAPSE  ',1)
      , ('HOLD      ',1), ('NODISPOSE ',1), ('DISPOSE   ',1), ('ALTERNATE ',1)
	, ('MIXED     ',3), ('ITERATE   ',1), ('RANDOM    ',2), ('MINIMUM   ',3)
	  , ('MAXIMUM   ',3), ('LOOP      ',1), ('STACK     ',2)
	    ,('UNSTACK   ',1),('MARK      ',3),('RELEASE   ',2),('SUMMARY   ',3)
	,('SAVE      ',3),('LOAD      ',3)
	      );

public var
    ch: char;
    skyhook: integer;
    foofname: string[30];
    dispose_blocks, mixed_disposal, alternate_disposal, collapse_watch,
      dump_heap, hold_disposes, nodisposes, random_blocks, loop_seen, iter_seen:
	boolean;
    seed: real;
    current_command: commands;
    loop_count, iter_count, min_size, max_size, i, iter_ix, loop_ix, ix: integer
      ;
    line: cmdline;
    idx: cmdlineidx;

type
    set_of_char = set of char;

const
    digits: set_of_char := ['0'..'9'];
    alpha: set_of_char := ['A'..'Z'];
    alphanumeric: set_of_char := ['A'..'Z','0'..'9'];
    def_min_size:= 1;
    def_max_size := 50;
    def_loop_count := 1;
    def_iter_count := 100;
    min_stack := 1;
    max_stack := 10;
    min_mark := 1;
    max_mark := 10;
    prompt:= '>';
    command_error:= '?Error in command line.  Try again.';

type
    new_ptr = ^new_record;
    new_record = packed record
      previous_block: new_ptr;
      next_block: new_ptr;
      packing: array[1..1] of integer
    end;
    stack_range = min_stack..max_stack;
    mark_range = min_mark..max_mark;

label
    1;

public var
    head_record: new_record := (nil,nil,(0));
    head_block: new_ptr := ^head_record;
    last_block: new_ptr := ^head_record;
    cur_stack: new_ptr := ^head_record;
    fake_ptr: ^integer;
    ix_mark: 0..max_mark := 0;
    ix_stack: 0..max_stack := 0;
    cur_ptr, p: new_ptr;
    size: integer;
    stack_ptrs: array[stack_range] of new_ptr;
    mark_table: array[mark_range] of integer;

external function lookup_command ( cmdline; var cmdlineidx; var command_list;
  commands; var commands): boolean;

function width (i: integer): integer;

begin
  width := 0;
  repeat
    width := width + 1;
    i := i div 10;
  until i = 0;
end;

public procedure pr_status;

begin
  write (tty,'Currently ');
  if ix_mark < min_mark then
    write (tty,'no marks, ')
  else if ix_mark = min_mark then
    write (tty,'one mark, ')
  else
    write (tty,ix_mark:width(ix_mark), ' marks, ');
  if ix_stack < min_stack then
    write (tty,'no stacks, ')
  else if ix_stack = min_stack then
    write (tty,'one stack, ')
  else
    write (tty,ix_stack:width(ix_stack),' stacks, ');
  write (tty,'and ');
  if head_block=last_block then
    writeln (tty,'no allocated blocks.')
  else begin
    i := 0; (* count blocks *)
    p := head_block^.next_block;
    size := 0; (* total block size *)
    while p <> nil do begin
      i := i + 1;
      fake_ptr := ptr(ord(p)-1);
      size := size - fake_ptr^;
      p := p^.next_block;
    end;
    if i = 1 then
      write (tty,'one allocated block of ')
    else
      write (tty,i:width(i),' allocated blocks totaling ');
    writeln (tty,size:width(size),' words.');
  end;
    if random_blocks then write (tty,' Random distribution, ');
    writeln (tty,'MINIMUM= ',min_size:width(min_size),' MAXIMUM= ',max_size:width(max_size));
  break;
end;

public procedure forward_delete (count: integer);

var
    p, p1: new_ptr;
    counter: integer;

begin
  p := cur_stack^.next_block;
  while p <> nil do begin
    counter := count;
    while (p^.next_block <> nil) andif (counter > 0) do begin
      p := p^.next_block;
      counter := counter - 1;
    end;
    with p^ do begin
      previous_block^.next_block := next_block;
      if next_block <> nil then
	next_block^.previous_block := previous_block
      else
	last_block := previous_block;
      p1:= next_block;
    end;
    dispose (p);
    p := p1;
  end;
end;

public procedure backward_delete (count: integer);

var
    p, p1: new_ptr;
    counter: integer;

begin
  p := last_block;
  while p <> cur_stack do begin
    counter := count;
    while (p^.previous_block <> head_block) andif (counter > 0) do begin
      p := p^.previous_block;
      counter := counter - 1;
    end;
    with p^ do begin
      previous_block^.next_block := next_block;
      if next_block <> nil then
	next_block^.previous_block := previous_block
      else
	last_block := p;
      p1 := previous_block;
    end;
    dispose (p);
    p := p1;
  end;
end;

public procedure free_blocks;

var
    p1: new_ptr;

begin
  if mixed_disposes then begin
    if alternate_disposes then begin
      backward_delete (3);
      forward_delete (2);
      backward_delete (1);
      forward_delete (0);
    end
    else begin
      forward_delete (3);
      backward_delete (2);
      forward_delete (1);
      backward_delete (0);
    end;
  end
  else begin
    if alternate_disposes then begin
      p := last_block;
      while p <> cur_stack do begin
	p1 := p^.previous_block;
	dispose (p);
	p := p1;
      end;
    end
    else begin
      p := cur_stack^.next_block;
      while p <> nil do begin
	p1 := p^.next_block;
	dispose (p);
	p := p1
      end;
    end;
  end;
  last_block := cur_stack;
  cur_stack^.next_block := nil;
end;

public function get_number: integer;

begin
  get_number := 0;
  while line[idx] in digits do begin
    get_number := get_number * 10 + ord (line[idx]) - ord ('0');
    idx := idx + 1;
  end;
end;

public procedure initialize;

begin
  dump_heap := false;
  hold_disposes := false;
  dispose_blocks := false;
  mixed_disposal := false;
  alternate_disposal := false;
  nodisposes := false;
  loop_count := 0;
  iter_count := def_iter_count;
  collapse_watch := false;
  iter_seen := false;
  loop_seen := false;
end;

begin
  rewrite (tty);
  open (tty);
  if query ('Do you wish instructions') then begin
    writeln (tty,'That''s too bad.');
    break;
  end;
  min_size := def_min_size;
  max_size := def_max_size;
  loop
    1:
      writeln (tty);
    write (tty,prompt);
    break;
    readln (tty);
  exit if eoln (tty);
    line := '';
    while not eoln (tty) do begin
      read (tty,ch);
      line := line || uppercase (ch);
    end;
    line := line || ' ';
    initialize;
    idx := 1;
    loop
      idx := verify (substr (line,idx),[' ',','],1)+idx-1;
    exit if substr (line,idx)='';
      if not (line[idx] in alphanumeric) then begin
	writeln (tty,command_error);
	goto 1;
      end;
      if line[idx] in digits then
	goto 2; (* iter count only *)
      if lookup_command (line,idx,cmd_list,maximum(commands),current_command)
	then begin
	  idx := verify (substr (line,idx),[' '],1)+idx-1;
	  case current_command of
	    c_status: begin
	      pr_status;
	    end;
	    c_stack: begin
	      if ix_stack = max_stack then begin
		writeln (tty,'Maximum stacking exceeded--command ignored.');
		goto 1;
	      end;
	      ix_stack := ix_stack+1;
	      stack_ptrs[ix_stack] := cur_stack;
	      cur_stack := last_block;
	    end;
	    c_unstack: begin
	      if ix_stack < min_stack then begin
		writeln (tty,'There is no previous stack--command ignored.');
		goto 1;
	      end;
	      cur_stack := stack_ptrs[ix_stack];
	      ix_stack := ix_stack - 1;
	    end;
	    c_mark: begin
	      if ix_mark = max_mark then begin
		writeln (tty,'Maximum mark depth exceeded--command ignored.');
		goto 1;
	      end;
	      ix_mark := ix_mark+1;
	      mark (mark_table[ix_mark]);
	    end;
	    c_release: begin
	      if ix_mark < min_mark then begin
		writeln (tty,'No previous mark--command ignored.');
		goto 1;
	      end;
	      release (mark_table[ix_mark]);
	      ix_mark := ix_mark - 1;
	      (* should check for undisposed pointers and "stack"s in the
		  heap being release *)
	    end;
	    c_dump: begin
	      dump_heap := true;
	    end;
	    c_collapse: begin
	      collapse_watch := true;
	      writeln (tty,'Heap collapse watch not yet implemented.');
	      break;
	    end;
	    c_hold: begin
	      hold_disposes := true;
	    end;
	    c_nodispose: begin
	      nodisposes := true;
	    end;
	    c_dispose: begin
	      dispose_blocks := true;
	    end;
	    c_alternate: begin
	      alternate_disposal := true;
	    end;
	    c_save: begin
	    foofname:='';write(tty,'FNAME:');break(tty);readln(tty);
	    while not eoln(tty) do begin
	      foofname:=foofname||tty^;
	      get(tty)
	      end;
	    write(tty,'HOOK:');break(tty);readln(tty);
	    read(tty,skyhook);
	    hpsave(foofname,skyhook)
	    end;

	    c_load: begin
	    foofname:='';write(tty,'FNAME:');break(tty);readln(tty);
	    while not eoln(tty) do begin
	      foofname := foofname||tty^;
	      get(tty)
	      end;
	    skyhook:=0;
	    hpload(foofname,skyhook);
	    writeln(tty,'skyhook=',skyhook)
	    end;
	    c_mixed: begin
	      mixed_disposal := true;
	    end;
	    c_iterate: begin
	      2:
		iter_seen := true;
	      iter_count := get_number;
	      if iter_count <= 0 then
		iter_count := def_iter_count;
	    end;
	    c_random: begin
	      ix := verify (substr(line,idx),digits+['.','e','E'],1)+idx - 1;
	      if ix > idx then
		seed := cv_real(substr(line,idx,ix-idx))
	      else seed := 0.0;
	      idx := ix;
(*$X10	      writeln(tty,'seed:= ',seed); *)
	      random_blocks := (seed > 0.0)
	    end;
	    c_minimum: begin
	      min_size := get_number;
	      if min_size <= 0 then
		min_size := def_min_size;
	    end;
	    c_maximum: begin
	      max_size := get_number;
	      if max_size <= 0 then
		max_size := def_max_size;
	    end;
	    c_loop: begin
	      loop_seen := true;
	      loop_count := get_number;
	      if loop_count <= 0 then
		loop_count := def_loop_count;
	    end
	  end; (* case *)
	  if min_size > max_size then begin
	    writeln (tty,'MINIMUM may not be greater than MAXIMUM.');
	    min_size := def_min_size;
	    max_size := def_max_size;
	    goto 1;
	  end;
	end (* if lookup ok *)
	else begin
	  writeln (tty,command_error);
	  goto 1;
	end;
    end;
    if iter_seen and not loop_seen then
      loop_count := def_loop_count;
    if dispose_blocks then
      free_blocks;
    for loop_ix := 1 to loop_count do begin
      size := min_size;
      for iter_ix := 1 to iter_count do begin
	if random_blocks then
	  begin
	  seed := random(seed);
	  size := round ((max_size-min_size)*seed)+min_size
	  end
	else
	  size := size+1;
	size := max (size,min_size);
	size := min (size,max_size);
	new (p:size);
	last_block^.next_block := p;
	p^.previous_block := last_block;
	p^.next_block := nil;
	last_block := p;
      end;
      if not (hold_disposes or nodisposes) then
	free_blocks;
    end;
    if hold_disposes and not nodisposes then
      free_blocks;
    if dump_heap then
      print_heap;
  end; (* loop until eoln *)
end (* new *).
   