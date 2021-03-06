program test60 options dumpst, dumpif;

type r = record
    a: integer;
    case b: integer of
	1,3,5..10: (
	    c1, d1: char );
	2,4,11..20: (
	    c2,d2: integer;
	    e2: array [1..*] of char );
	21: (
	    case c3: char of
		'A'..'M': (
		    );
		'N'..'Z': (
		    d3: array ['A'..*] of integer ) )
    end (* p *);

var p: ^ r;

begin
  new (p);
  new (p,1);
  new (p,6);
  new (p,2);
  new (p,2,20);
  new (p,21);
  new (p,21,'A');
  new (p,21,'Z');
  new (p,21,'Z','F');

  new (p,'A');
  new (p,1,1);
  new (p,2,'A');
  new (p,2,20,1);
  new (p,21,'A',1);
end.
  