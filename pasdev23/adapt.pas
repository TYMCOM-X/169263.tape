$TITLE adapt
$WIDTH (106)

program adapt;

(* The current and past states of the adaptive process are preserved in a data base.
   The layout of this disk file is:

     - number of last generation in the data base
     - vector of cursor values specifying where the generation record for each
       generation is located in the file
     - check pattern, an aid in verifying that the data base is intact, and that
       things are where they out to be
     - generation record for first generation in data base, followed by check pattern
	.
	.
	.
     - generation record for last generation in data base, followed by check pattern

   For example, on the PDP 10 where "storage units" are 36 bit words, assuming
   generation_range = 0..1000, and check pattern is two words:

     PDP 10 word        contents
     -----------        --------
	1		last generation's number
	2		cursor for generation 0's extent and record (1005)
	3		cursor for generation 1's extent and record
	.
	.
     1002		cursor for generation 1000's extent and record
     1003		check pattern (first word)
     1004		check pattern (second word)
     1005		extent for generation 0's record (call it ext0)
     1006		generation 0's record
	.		   .
	.		   .
     1005 + ext0	   v
     1006 + ext0        alt. check pattern (first word)
     1007 + ext0        alt. check pattern (second word)
     1008 + ext0	extent for generation 1's record
     1009 + ext0	generation 1's record
	.		   .
	.		   .
	.		   .							*)
$PAGE includes, types, and constants

$INCLUDE x4move.typ

$ENABLE ADAPT
$SYSTEM x4move.inc
$DISABLE ADAPT

type
  generation_range = 0..1000;
  check_pattern_type = packed array [1..10] of char;

  structure_type = record
    coef: packed_coef_array;			(* member of population ... *)
    fitness: structure_fitness_type		(* ... and its utility *)
  end;

  (* Parameters specific to adaptive process of producing the next generation. *)

  adaptation_parameter_record = record
    crossover_probability,			(* prob. of a pair of members being crossed *)
    mutation_probability,			(* prob. of mutating a given member *)
    observed_crossover_rate,			(* actual rate for comparison with prob. *)
    observed_mutation_rate,			(*   "     "    "       "      "    "    *)
    exploitation_coef: real;			(* magnification of difference from average *)
    nominal_pop_size: 1..maximum (integer)	(* desired population size *)
  end;

  (* The population at a given generation, and parameters used to obtain it. *)

  generation_record = record
    check_pattern: check_pattern_type;
    generation_number: generation_range;
    adaptation_parameters: adaptation_parameter_record;
    fitness_parameters: fitness_parameter_record;
    average_fitness: real;
    population: array [1..*] of structure_type
  end;

const
  adapt_db_name = 'adapt.db';
  ints_in_gen_table = 1 + (maximum (generation_range) - minimum (generation_range) + 1);
		  (* the last generation's number, plus cursor values for each generation *)
  gen_table_size = ints_in_gen_table * size (integer);
  check_pattern_value: check_pattern_type := '*DJW*djw* ';
  alt_check_pattern_value: check_pattern_type := '#DJW#djw# ';
$PAGE fatal_error, cvt_rt_to_string

procedure fatal_error (msg: packed array [1..*] of char);

  begin
    writeln (ttyoutput, msg);
    close (ttyoutput);
    stop
  end (* fatal_error *);



function cvt_rt_to_string (rtime: real): string;
  
$IF P10
  begin
    putstring (cvt_rt_to_string, round (rtime / 10000.0, -1):0:1)
  end;
$ENDIF

$IF VAX
  var
    seconds, minutes, hours: 0..maximum (integer);
  begin
    seconds := round (rtime / 1000.0);
    minutes := seconds div 60;
    seconds := seconds mod 60;
    hours := minutes div 60;
    minutes := minutes mod 60;
    putstring (cvt_rt_to_string, hours:3, ':', minutes:2, ':', seconds:2)
  end;
$ENDIF
$PAGE initialize_db

(* INITIALIZE DB: Determine if data base already exists.  If not, create and initialize
   it.  Otherwise, verify that it seems intact. *)

procedure initialize_db (var adapt_db: file of * );

  var
    check_pattern: check_pattern_type;
    i: 1..ints_in_gen_table;
    int: integer;

  begin
    reset (adapt_db, adapt_db_name, [seekok]);

    case iostatus (adapt_db) of

      io_ok: begin
	if extent (adapt_db) < gen_table_size then
	  fatal_error ('?data base exists, but its size isn''t enough for gen. table.');
	readrn (adapt_db, gen_table_size + 1, check_pattern);
	if iostatus (adapt_db) <> io_ok then
	  fatal_error ('?error while reading check pattern after generation table.');
	if check_pattern <> check_pattern_value then
	  fatal_error ('?check pattern following generation table is bad.');
	close (adapt_db);
	writeln (ttyoutput, 'Data base exists.'); break (ttyoutput)
      end;

      io_opnf: begin
	rewrite (adapt_db, adapt_db_name);
	if iostatus (adapt_db) <> io_ok then
	  fatal_error ('?unable to rewrite database for initialization.');
	if extent (adapt_db) <> 0 then
	  fatal_error ('?extent isn''t 0 after rewrite of adapt_db.');
	if cursor (adapt_db) <> 1 then
	  fatal_error ('?cursor isn''t 1 after rewrite of adapt_db.');
	int := minimum (generation_range) - 1;
	for i := minimum (i) to maximum (i) do begin
	  write (adapt_db, int);
	  if iostatus (adapt_db) <> io_ok then
	    fatal_error ('?error while writing initial generation table.');
	end;
	if extent (adapt_db) <> gen_table_size then
	  fatal_error ('?extent after writing initial gen. table isn''t as expected.');
	if cursor (adapt_db) <> (gen_table_size + 1) then
	  fatal_error ('?cursor after writing initial gen. table isn''t as expected.');
	write (adapt_db, check_pattern_value);
	if iostatus (adapt_db) <> io_ok then
	  fatal_error ('?error while writing check pattern after generation table.');
	if extent (adapt_db) <> (gen_table_size + size (check_pattern_type)) then
	  fatal_error ('?extent of db after writing gen. table and check pattern isn''t right.');
	close (adapt_db);
	writeln (ttyoutput, 'Data base created.'); break (ttyoutput)
      end;

      others:
	fatal_error ('?odd error in trying to reset adapt_db for initialization.')
    end
  end (* initialize_db *);
$PAGE read_last_gen_number

(* READ LAST GEN NUMBER: Read the number of the last generation presently in the data base. *)

procedure read_last_gen_number (var adapt_db: file of *;
				var last_gen_num: integer);

  begin
    reset (adapt_db, adapt_db_name);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?unable to reset adapt_db to read the last generation''s number.');
    read (adapt_db, last_gen_num);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?failure on attempt to read last generation''s number.');
    close (adapt_db);

    if (last_gen_num < (minimum (generation_range) - 1)) or
       (last_gen_num > maximum (generation_range)) then
      fatal_error ('?last generation''s number is garbage.')
    else if last_gen_num = maximum (generation_range) then
      fatal_error ('?last generation was all that the data base can accomodate.')
  end (* read_last_gen_number *);
$PAGE reset_random_seed

(* RESET RANDOM SEED: Reinitialize the psuedo random number generator.  We ensure
   that production of a given generation will always begin at the same point in
   the psuedo random sequence (for repeatability), but each generation starts
   at a different point so localized "nonuniformities" won't effect every generation. *)

procedure reset_random_seed (next_gen_number: generation_range);

  var
    equiv: record
      case boolean of
	true: (int: integer);
	false: (rl: real)
    end;
    x: real;
    i: 1..maximum (generation_range) * 500;

  begin

$IF P10
    equiv.int := 1777777b;
    x := random (equiv.rl);
$ENDIF
$IF VAX
    x := random (0.5);
$ENDIF

    for i := 1 to next_gen_number * 500 do
      x := random
  end;
$PAGE prompt_for_real, prompt_for_int, prompt_for_coef_array, query

(* Utilities for communicating with the user. *)

procedure write_msg (msg: packed array [1..*] of char);
  begin
    write (ttyoutput, msg);
    break (ttyoutput);
    readln (tty)
  end;

function prompt_for_real (msg: packed array [1..*] of char): real;

  begin
    write_msg (msg);
    read (tty, prompt_for_real)
  end;

function prompt_for_int (msg: packed array [1..*] of char): integer;

  begin
    write_msg (msg);
    read (tty, prompt_for_int)
  end;

function prompt_for_coef_array (msg: packed array [1..*] of char): packed_coef_array;

  var
    i: coef_index_type;
    str: string [3];
    coef_temp: integer;

  begin
    writeln (ttyoutput, msg);
    for i := minimum (i) to maximum (i) do begin
      putstring (str, i:3);
      write_msg ('      coefficient' || str || ' = ');
      read (tty, coef_temp);
      prompt_for_coef_array [i] := coef_temp + coef_excess
    end
  end;

function query (msg: packed array [1..*] of char): string [3];

  var
    str: string [3];

  begin
    repeat
      write_msg (msg);
      read (tty, str);
      str := uppercase (str);
      if str = 'N' then
	str := 'NO'
      else if str = 'Y' then
	str := 'YES'
    until (str = 'YES') or (str = 'NO');
    query := str
  end;
$PAGE create_first_gen

(* CREATE FIRST GEN: Query user for initial values of parameters to control the adaptive
   process and for structure fitness evaluation, the create the initial population. *)

procedure create_first_gen (var first_gen: ^generation_record);

  var
    adapt_pars: adaptation_parameter_record;
    fitness_pars: fitness_parameter_record;
    i: 1..maximum (integer);
    j: coef_index_type;

  begin
    writeln (ttyoutput);
    writeln (ttyoutput, 'enter initial values for parameters controlling the adaptive process:');

    with adapt_pars do begin
      crossover_probability := prompt_for_real ('   probability of crossover = ');
      mutation_probability := prompt_for_real ('   probability of mutation = ');
      observed_crossover_rate := 0.0;
      observed_mutation_rate := 0.0;
      exploitation_coef := prompt_for_real ('   exploitation coefficient = ');
      nominal_pop_size := prompt_for_int ('   desired population size = ')
    end;

    writeln (ttyoutput);
    writeln (ttyoutput, 'enter initial values for fitness parameters:');
    with fitness_pars do begin
      opponents_primary_limit := 
	    prompt_for_int ('   primary lookahead limit for machine''s opponent = ');
      opponents_critical_limit := 
	    prompt_for_int ('   critical lookahead limit for machine''s opponent = ');
      opponents_strategy := prompt_for_coef_array ('   enter opponents strategy vector:');
      primary_limit := prompt_for_int ('   primary lookahead limit for machine = ');
      critical_limit := prompt_for_int ('   critical lookahead limit for machine = ')
    end;

    new (first_gen, adapt_pars.nominal_pop_size);
    with first_gen^ do begin
      check_pattern := check_pattern_value;
      generation_number := minimum (generation_range);
      adaptation_parameters := adapt_pars;
      fitness_parameters := fitness_pars;
      for i := 1 to upperbound (population) do
	with population [i] do
	  for j := minimum (j) to maximum (j) do
	    coef [j] := trunc (random * (maximum (coef_type) - minimum (coef_type) + 1))
			  + minimum (coef_type)
    end;
    writeln (ttyoutput, 'First generation created.'); break (ttyoutput)
  end (* create_first_gen *);
$PAGE create_next_gen

(* CREATE NEXT GEN: Applies a genetic algorith to the last generation to derive the next. *)

procedure create_next_gen (var adapt_db: file of *;
			       last_gen_num: integer;
			   var next_gen: ^generation_record);


  var
    last_gen: ^generation_record;
    last_gen_cursor,
    last_gen_extent: integer;
    alt_check_pattern: check_pattern_type;
    adapt_pars: adaptation_parameter_record;
    fitness_pars: fitness_parameter_record;
    temp_pop: ^array [1..*] of structure_type;
    taken: ^array [1..*] of boolean;
    temp_pop_size,
    temp_pop_maxsize,
    pop_size,
    cross_point,
    cross_cell,
    crossover_count,
    mutation_count,
    bits_per_structure: 0..maximum (integer);
    member,
    first_pick,
    second_pick: 1..maximum (integer);
    offspring,
    cent_force: real;
    c1, c2: coef_type;
    cell: coef_index_type;



  function picked_member: 1..maximum (integer);

    var
      pick, member, count: 0..maximum (integer);

    begin
      pick := trunc (random * temp_pop_size) + 1;   (* choose one of remaining structures *)
      member := 0;
      count := 0;
      repeat
	member := member + 1;
	if not taken^ [member] then
	  count := count + 1			(* count through remaining structures *)
      until count = pick;
      picked_member := member;
      taken^ [member] := true;			(* sampling is w/o replacement *)
      temp_pop_size := temp_pop_size - 1
    end (* picked_member *);



  procedure mutate (target_mem: 1..maximum (integer));

    var
      mut_point, mut_cell: 0..maximum (integer);
      low, high: coef_type;
      mut_bit_value: 0..1;

    begin
      mutation_count := mutation_count + 1;
      mut_point := trunc (random * bits_per_structure);	(* bits before bit to change *)
      mut_cell := mut_point div bits_per_coefficient;	(* whole cells before mut. point *)
      mut_point := mut_point - mut_cell * bits_per_coefficient;
      low := next_gen^.population [target_mem].coef [mut_cell + minimum (coef_index_type)]
		mod 2 ** mut_point;
      high := next_gen^.population [target_mem].coef [mut_cell + minimum (coef_index_type)]
		div 2 ** mut_point;
      mut_bit_value := high mod 2;

      next_gen^.population [target_mem].coef [mut_cell + minimum (coef_index_type)] :=
		(high - mut_bit_value + 1 - mut_bit_value) * 2 ** mut_point + low
    end (* mutate *);
$PAGE
  begin						(* create_next_gen *)

    (* read last generation *)

    reset (adapt_db, adapt_db_name, [seekok]);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?unable to reset adapt_db to real last generation.');

    readrn (adapt_db,
	    (1 + (last_gen_num - minimum (generation_range) + 1)) * size (integer),
	    last_gen_cursor);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?error reading last generation''s cursor.');
    readrn (adapt_db, last_gen_cursor, last_gen_extent);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?error reading last generation''s extent.');
    allocate (last_gen, last_gen_extent);
    read (adapt_db, last_gen^: last_gen_extent);
    if iostatus <> io_ok then
      fatal_error ('?error reading last generation.');
    read (adapt_db, alt_check_pattern);
    if iostatus <> io_ok then
      fatal_error ('?error reading check pattern after last generation.');
    if alt_check_pattern <> alt_check_pattern_value then
      fatal_error ('?check pattern following last generation''s record isn''t right.');
    close (adapt_db);

    if (last_gen^.check_pattern <> check_pattern_value) or
       (last_gen^.generation_number <> last_gen_num) then
      fatal_error ('?last generation''s check pattern and number don''t check out.');
    writeln (ttyoutput, 'Generation ', last_gen_num:3, ' read from data base.'); break(ttyoutput);

    adapt_pars := last_gen^.adaptation_parameters;
    fitness_pars := last_gen^.fitness_parameters;

    if query ('change parameters before next generation? ') = 'YES' then begin
      writeln (ttyoutput, 'not implemented yet')
    end;
$PAGE

    (* reproduction phase *)

    temp_pop_maxsize := round (1.10 * max (adapt_pars.nominal_pop_size,
					   upperbound (last_gen^.population)));

    new (temp_pop, temp_pop_maxsize);
    temp_pop_size := 0;
    cent_force := 0.75 * (adapt_pars.nominal_pop_size - upperbound (last_gen^.population)) /
			adapt_pars.nominal_pop_size;	(* centering force on pop. size *)

    for member := 1 to upperbound (last_gen^.population) do begin
      offspring := last_gen^.population [member].fitness / last_gen^.average_fitness;
      offspring  := 1.0 + adapt_pars.exploitation_coef * (offspring - 1.0);
						(* emphasize distance from average ... *)
      offspring := offspring + cent_force;	(* ... and apply centering force *)
      while (offspring >= 1.0) orif (offspring > random) do begin
       exit if temp_pop_size = temp_pop_maxsize;
	temp_pop_size := temp_pop_size + 1;
	temp_pop^ [temp_pop_size] := last_gen^.population [member];
	offspring := offspring - 1.0
      end
    end;

    dispose (last_gen);
    new (next_gen, temp_pop_size);
    with next_gen^ do begin
      check_pattern := check_pattern_value;
      generation_number := last_gen_num + 1;
      adaptation_parameters := adapt_pars;
      fitness_parameters := fitness_pars
    end;
$PAGE

    (* cross over phase *)

    pop_size := 0;
    new (taken, temp_pop_size);
    for member := 1 to temp_pop_size do
      taken^ [member] := false;

    bits_per_structure := bits_per_coefficient *
			       (maximum (coef_index_type) - minimum (coef_index_type) + 1);

    crossover_count := 0;			(* count of actual crosses *)
    mutation_count := 0;			(* count of actual mutations *)

    while temp_pop_size > 1 do begin
      first_pick := picked_member;
      second_pick := picked_member;
      next_gen^.population [pop_size + 1] := temp_pop^[first_pick];
      next_gen^.population [pop_size + 2] := temp_pop^[second_pick];

      if next_gen^.adaptation_parameters.crossover_probability > random then begin
	crossover_count := crossover_count + 1;
	cross_point := trunc (random * bits_per_structure); (* bits before cross point *)
	cross_cell := cross_point div bits_per_coefficient; (* whole cells before cross point *)
	for cell := minimum (cell) to minimum (cell) + cross_cell - 1 do begin
	  next_gen^.population [pop_size + 1].coef [cell] :=
		temp_pop^[second_pick].coef [cell];
	  next_gen^.population [pop_size + 2].coef [cell] :=
		temp_pop^[first_pick].coef [cell]
	end;
	cross_point := cross_poinross_cell * bits_per_coefficient;
	if cross_point > 0 then begin
	  c1 := next_gen^.population [pop_size + 1].coef [minimum (cell) + cross_cell];
	  c2 := next_gen^.population [pop_size + 2].coef [minimum (cell) + cross_cell];
	  next_gen^.population [pop_size + 1].coef [minimum (cell) + cross_cell] :=
		(c1 div 2**cross_point) * 2**cross_point + c2 mod 2**cross_point;
	  next_gen^.population [pop_size + 2].coef [minimum (cell) + cross_cell] :=
		(c2 div 2**cross_point) * 2**cross_point + c1 mod 2**cross_point
	end
      end;

      if next_gen^.adaptation_parameters.mutation_probability > random then
	mutate (pop_size + 1);
      if next_gen^.adaptation_parameters.mutation_probability > random then
	mutate (pop_size + 2);

      pop_size := pop_size + 2
    end (* while *);

    if temp_pop_size > 0 then begin
      first_pick := picked_member;
      pop_size := pop_size + 1;
      next_gen^.population [pop_size] := temp_pop^[first_pick];
      if next_gen^.adaptation_parameters.mutation_probability > random then
	mutate (pop_size)
    end;

    dispose (temp_pop);
    with next_gen^.adaptation_parameters do begin
      observed_crossover_rate := crossover_count / (pop_size div 2);
      observed_mutation_rate := mutation_count / pop_size
    end;
    writeln (ttyoutput, 'Generation ', next_gen^.generation_number:3, ' created.');
    break (ttyoutput)
  end (* create_next_gen *);
$PAGE evaluate_pop_members

(* EVALUATE POP MEMBERS: evaluates the fitness of each member of the population,
   keeping track of the best, worst, and average. *)

procedure evaluate_pop_members (new_gen: ^generation_record);

  var
    total_fitness: real;
    member: 1..maximum (integer);
    best, worst: structure_fitness_type;
    cpu: integer;
    best_structure: structure_type;
    row: 1..6;
    col, last_col: 0..maximum (integer);

  begin
    with new_gen^ do begin
      prep_structure_evaluation (fitness_parameters);	(* initialize move module *)
      total_fitness := 0.0;
      for member := 1 to upperbound (population) do
	with population [member] do begin
	  cpu := runtime;
	  fitness := evaluate_structure (coef);	(* run off a game *)
	  cpu := runtime - cpu;
	  writeln (ttyoutput, 'member', member:4, ' evaluated in ', cvt_rt_to_string (cpu));
	  total_fitness := total_fitness + fitness;
	  if member = 1 then begin
	    best := fitness;
	    best_structure := population [member];
	    worst := fitness
	  end
	  else if fitness > best then begin
	    best := fitness;
	    best_structure := population [member]
	  end
	  else if fitness < worst then
	    worst := fitness
	end;
      average_fitness := total_fitness / upperbound (population);
      writeln (ttyoutput, 'generation:', generation_number:4, '; best, worst, ave.:', best:4,
			  ', ', worst:4, ', ', average_fitness:7:2,
			  '; pop. size:', upperbound (population):5);
      with adaptation_parameters do
	writeln (ttyoutput, 'actual crossover and mutation rates:', observed_crossover_rate:6:3,
			    observed_mutation_rate:6:3);
      writeln (ttyoutput, 'best structure''s fitness: ', best_structure.fitness);
      writeln (ttyoutput, 'best structures''s coeficients:');
      last_col := ((maximum (coef_index_type) - minimum (coef_index_type) + 1) + 5) div 6;
      for row := 1 to 6 do begin
	for col := 0 to last_col - 1 do
	  if (row + 6 * col) <= maximum (coef_index_type) then
	    write (ttyoutput, row + 6 * col:8,
			      best_structure.coef [row + 6 * col] - coef_excess:6);
	writeln (ttyoutput)
      end;
    end
  end (* evaluate_pop_members *);
$PAGE update_db

(* UPDATE DB: adds the new generation to the data base. *)

procedure update_db (var adapt_db: file of *;
			 new_gen: ^generation_record);

  var
    last_gen, gen_cursor, gen_ext, table_cursor: integer;

  begin
    update (adapt_db, adapt_db_name);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?unable to update adapt_db.');

    gen_cursor := extent (adapt_db) + 1;
    readrn (adapt_db, 1, last_gen);
    if last_gen < minimum (generation_range) then begin
      if gen_cursor <= (gen_table_size + size (check_pattern_type)) then
	fatal_error ('?in update_db, file extent prior to write of first gen. too small.')
    end;
    if last_gen <> (new_gen^.generation_number - 1) then
      fatal_error ('?generation number doesn''t check in update_db.');

    table_cursor := (1 + (new_gen^.generation_number - minimum (generation_range) + 1))
		     * size (integer);
    writern (adapt_db, table_cursor, gen_cursor);
    gen_ext := extent (new_gen);
    writern (adapt_db, gen_cursor, gen_ext, new_gen^: gen_ext);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?error while writing current generation to db.');

    write (adapt_db, alt_check_pattern_value);
    if iostatus (adapt_db) <> io_ok then
      fatal_error ('?error while writing alt. check pattern after current gen.''s record to db.');

    last_gen := last_gen + 1;
    writern (adapt_db, 1, last_gen);

    close (adapt_db);
    dispose (new_gen);
    writeln (ttyoutput, 'Generation ', last_gen:3, ' written to data base.')
  end (* update_db *);
$PAGE mainline

var
  adapt_db: file of *;
  last_gen_number: integer;
  current_generation: ^generation_record;
  total_cpu: integer;

begin
  total_cpu := runtime;
  rewrite (ttyoutput);
  open (tty);

  (* If database doesn't exist, create and initialize it. *)

  initialize_db (adapt_db);

  (* Obtain the next generation of structures. *)

  read_last_gen_number (adapt_db, last_gen_number);
  reset_random_seed (last_gen_number + 1);
  if last_gen_number < minimum (generation_range) then
    create_first_gen (current_generation)
  else
    create_next_gen (adapt_db, last_gen_number, current_generation);

  (* Compute the fitness of the population's members. *)

  evaluate_pop_members (current_generation);

  (* Add the new generation to the database. *)

  update_db (adapt_db, current_generation);

  total_cpu := runtime - total_cpu;
  writeln (ttyoutput, 'total cpu time: ', cvt_rt_to_string (total_cpu))

end.
0 `