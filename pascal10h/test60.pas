module test99;
type
  ech = string [12];

public function return_string: ech;
begin
return_string := ';lsdf';
return_string := return_string || 'y';
return_string := return_string () || 'y'
end.
