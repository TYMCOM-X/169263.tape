program djw5 options dump(noro);

const
  cs1: set of 0..31 = [0,1,2];

var
  s1: set of 1..31;
  s2: set of 0..30;
  s3: set of 2..31;
  s4: set of 0..29;
  s5: set of 15..16;
  s6: set of 16..16;
  s7: set of 15..15;
  s8: set of 0..85*16-3;
  r8: set of 16*16..85*16-1;
  s9: set of 0..22*16-1;
  r9: set of 10*16..11*16-1;

begin
  s1 := cs1;
  s2 := cs1;
  s3 := cs1;
  s4 := cs1;
  s5 := cs1;
  s6 := cs1;
  s7 := cs1;
  s8 := r8;
  s8 := s8;
  s9 := r9;
end.
    