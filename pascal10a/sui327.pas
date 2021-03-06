
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*       This program writes 15 records to a file of 'rec' and then
        reads them in backwards (to test seek) and compares what was
        read with what was written.  Program deviates if eof is
        encountered or if any of the records read is different from
        what was written.
*)
program sui327;

const str_val : array[io_status] of string := ('io_ok',',io_novf',
        'io_povf', 'io_dgit', 'io_govf', 'io_intr', 'io_rewr',
        'io_eof', 'io_outf', 'io_inpf', 'io_seek', 'io_illc',
        'io_nepf', 'io_opnf');
      Max = 15;
      Filnam = 'fyle.tmp';

type rec = record
        int : integer;
        rea : real;
        boo : boolean
     end;       

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


var f : file of rec;
    r : rec;
    i : integer;
    a : array[1 .. Max] of rec;
        
begin
        rewrite(output,'suite.txt',[preserve]);

        rewrite(f,Filnam);
        for i := 1 to Max do begin
                with a[i] do begin
                        int := i;
                        rea := sqrt(i);
                        boo := (i mod 2 = 0)
                end;
                write(f,a[i])
        end;
        close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        reset(f,Filnam,[seekok,retry]);
        if extent(f) <> Max then
                writeln('deviates: sui327 (extent)');
        i := Max;
        loop
                seek(f,i);
                exit if iostatus(f)<> io_ok do
                        writeln('deviates: sui327 iostatus = ',
                                str_val[iostatus(f)]);
                exit if (f^.int <> a[i].int) or (f^.rea <> a[i].rea)
                        or (f^.boo <> a[i].boo) do
                        writeln('deviates: sui327  (seek)');
                exit if i = 1 do writeln('conforms: sui327');
                i := i - 1;
        end
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  