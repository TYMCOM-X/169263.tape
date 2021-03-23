program test65 options dumpst, dumpif;

type r = record
    a: integer;
    case b: integer of
	1,3,5..10: (
	    c1, d1: char );
	2,4,11..20: (
	    c2,d2: integer;
	    e2: packed array [1..*] of char );
	21: (
	    case c3: char of
		'A'..'M': (
		    );
		'N'..'Z': (
		    d3: array ['A'..*] of integer ) )
    end (* p *);

var p: ^ r;
    i: integer;

begin
  i := size (r);
  i := size (r,1);
  i := size (r,6);
  i := size (r,2);
  i := size (p^,2,20);
  i := size (p^,21);
  i := size (p^,21,'A');
  i := size (p^,21,'Z');
  i := size (p^,21,'Z','F');
  i := size (p);

  i := size (p^,'A');
  i := size (p^,1,1);
  i := size (p^,2,'A');
  i := size (p^,2,20,1);
  i := size (p^,21,'A',1);
  i := size (p^,21,'Z',1);
  i := size (p^.e2[1]);
  i := size (20);
end.
    