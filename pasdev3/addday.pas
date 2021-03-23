$Title ADD_DAYS - add the specified number of days to the given dtime record.

module add_days;

$system dtime.typ

external function add_secs ( time1: dtime_int; numsecs: seconds ): dtime_int;
$PAGE ADD_DAYS

public function add_days ( oldtime : dtime_int; num_days: days ): dtime_int;

begin

  (* the algorithm is as follows; convert the days to the respective number
     of seconds and then call add seconds. *)

  add_days := add_secs ( oldtime , num_days * ( 24*60*60 ) )

end (* add_days *) .
