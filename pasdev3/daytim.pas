(*   +--------------------------------------------------------------+
     |                                                              |
     |                        D A Y T I M E                         |
     |                        - - - - - - -                         |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  DTIME - day/time manipulation routines
     
     STARTED:  11/01/78
     
     PURPOSE:  Return current local day/time in internal form.
     
     USAGE:
     
        EXTERNAL FUNCTION DAYTIME: DTIME_INT;
     
     OUTPUT:
     
        <return value>
                    the  current  local  day/time  is   returned   in
                    internal form.
     
     INCLUDE FILES REQUIRED:
     
        DTIME.TYP
        DTIME.INC
     
     RESPONSIBLE:  S.  M.  ROUSH
     
     CHANGES:
	1/07/80	 smr	created Pascal version of DAYTIME for the VAX.
     ---------------------------------------------------------------- *)

module daytime;

$SYSTEM idtime.typ

external procedure sys$gettim ( var dtime_int );

public function daytime: dtime_int;

begin
  sys$gettim ( daytime );
end.
  