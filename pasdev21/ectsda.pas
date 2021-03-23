(* EC_TSDATE - converts a standard TYMSHARE date to an internal date.
   The standard TYMSHARE date is the *exact* number of days since
   January 1, 1964.  *)

module ec_tsdate;

$SYSTEM dtime.typ

public function ec_tsdate ( ts_date: tym_date ): date_int;

begin
  ec_tsdate := (ts_date + 112773b) * 1000000b;
end.
