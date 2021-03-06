module ecsecs
  options nolibrary;

$HEADER ecsecs.hdr

$SYSTEM idtime.typ

public function ec_secs ( secs: seconds ): time_int
  options special (word);

const
  secs_per_day: seconds = 86400;

var
  temp: machine_word;

begin
  ec_secs.d := 0;
  temp := secs * (2**18);
  temp := temp + secs_per_day div 2;	(* so following DIV will round *)
  ec_secs.t := temp div secs_per_day;
end.
  