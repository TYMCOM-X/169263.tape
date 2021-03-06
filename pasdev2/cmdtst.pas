$TITLE CMDTST -- Command Utility Test Program

program cmdtst;

$SYSTEM cmdutl

var l: string [256];
    c: integer;
    cmd: packed array [1..3] of char;
    chset: set of char;
    n1, n2, n3: integer;
    b1, b2, b3: boolean;
    rs: string [10] := '';
    rs1: string [20] := '';

begin
  rewrite (tty);
  open (tty);
  loop
    write (tty, 'COMMAND: ');
    break;
    readln (tty);
    read (tty, l);
    if length (l) < 3 then
      l := '   ';
    cmd := uppercase (substr (l, 1, 3));
    if (cmd = 'EXI') or (cmd = 'STO') or (cmd = 'QUI') or (cmd = 'END') then
      stop
    else if cmd = 'SKI' then begin
      c := 4;
      cmd_skip_blanks (l, c);
      writeln (tty, '  Cursor =', c);
    end
    else if cmd = 'EOL' then begin
      c := 4;
      b1 := cmd_eol (l, c);
      writeln (tty, 'Eol =', b1, ', cursor =', c);
    end
    else if cmd = 'CHE' then begin
      c := 5;
      b1 := cmd_check_punct (l, c, l[4]);
      writeln (tty, '  Check =', b1, ', cursor =', c);
    end
    else if cmd = 'TOK' then begin
      chset := [];
      c := 5;
      while l[c] <> l[4] do begin
	chset := chset + [l[c]];
	c := c + 1;
      end;
      c := c + 1;
      b1 := cmd_token (l, c, chset, rs);
      writeln (tty, 'Token =', b1, ', value = "', rs, '", cursor =', c);
    end
    else if cmd = 'NUM' then begin
      b1 := (l[4] = 'T') or (l[4] = 't');
      c := 5;
      b2 := cmd_number (l, c, b1, n1);
      writeln (tty, 'Number =', b2, ', value =', n1, ', cursor =', c);
    end
    else if cmd = 'STR' then begin
      c := 5;
      b1 := cmd_string (l, c, l[4], rs);
      writeln (tty, 'String =', b1, ', value = "', rs, '", cursor =', c);
    end
    else if cmd = 'DQS' then begin
      c := 5;
      b1 := cmd_dqstring (l, c, l[4], rs);
      writeln (tty, 'Dqstring =', b1, ', value = "', rs, '", cursor =', c);
    end
    else if cmd = 'FIL' then begin
      c := 5;
      b1 := (l[4] = 'T') or (l[4] = 't');
      b2 := cmd_file_name (l, c, b1, rs1);
      writeln (tty, 'Filename =', b2, ', value = "', rs1, '", cursor =', c);
    end
    else begin
      writeln (tty, '  EXIt, SKIp_blanks, EOL, NUMber, TOKen, STRing');
      writeln (tty, '  DQString, FILe_name, CHEck_punct');
    end;
  end;
end.
  