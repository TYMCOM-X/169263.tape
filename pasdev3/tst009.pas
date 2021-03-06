(* TST009 - storage class addressing tests.  *)

module tst009;

type
  vstr = string[ 10 ];
  rec1 = packed record
    f1: boolean;
    f2: 0.0..maximum( real ) prec 14
  end;
  ch_set = set of char;
  double = 0..maximum( real ) prec 14;

public function a ( ap_b: boolean; ap_vs: vstr ): char;

var
  al_ch: char;
  al_d: double;
  
function a1 ( var a1p_ch: char; var a1p_rec: rec1 ): rec1;

  var
    a1l_r: real;
    a1l_cs: ch_set;
    a1l_i: integer;

  function a11 ( var a11p_i: integer; a11p_cs: ch_set ): double;

    var
      a11l_b: boolean;
      a11l_ch: char;
      a11l_d: double;

    begin
      a11l_b := ap_b;
      a11l_ch := ap_vs[ 2 ];
      a11l_ch := a;
      
      al_ch := a1p_ch;
      a11l_b := a1p_rec.f1;
      a11l_d := a1.f2;
      al_d := a1l_r;
      a1l_cs := a11p_cs;
      a1l_i := a11p_i;
      al_d := a11;
    end;

  begin	(* proc a1 *)
  end;

begin	(* proc a *)
end.
   