program barth
  options optimize, dump (bblks);

label 100;

var flag: boolean;

procedure s;
  label 200;

  procedure q;
    label 300;

    procedure p;
    begin
      s;
    end;

    procedure r;
    begin
      if flag then goto 300;
      if not flag then goto 200;
    end;

  begin (* q *);
    r;
    p;
    if flag then goto 100;
300:
  end;

begin (* s *);
  q;
  flag := false;
200:
end;

begin
  s;
  flag := true;
100:
end.
   