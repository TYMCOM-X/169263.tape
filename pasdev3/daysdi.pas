$Title DAYS_DIFF - Difference in days between two dtime records.

module days_diff;

$system dtime.typ

external function secs_diff ( time1: dtime_int; time2: dtime_int ): seconds;
$PAGE days_diff
public function days_diff ( time1 : dtime_int; time2: dtime_int ): days;

begin

  (* The algorithm is simple, find the seconds differnence and then convert
     the seconds to days. *)

  days_diff := secs_diff ( time1 , time2 ) div ( 24*60*60 )

end (* days_diff *) .
