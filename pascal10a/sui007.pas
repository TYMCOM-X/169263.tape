
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number   7*)
(*TEST 6.1.3-3, CLASS=QUALITY*)
(* Although the Standard places no limit on the LENGTH of
  identifiers, they must be UNIQUE in at least the first 8
  characters. This program will determine the significance
  of identifiers from 3 characters to 20 characters.
  This program is of course non-standard, and relies on scope
  masquerades. It assumes a naive significance limit exists,
  or that there is none. Some compilers may violate the assumption
  by hashing an identifier tail or preserving the real length. *)
program t6p1p3d3;
const
   i3i = 3;
   i4ii = 1;
   i5iii = 1;
   i6iiii = 1;
   i7iiiii = 1;
   i8iiiiii = 1;
   i9iiiiiii = 1;
   i10iiiiiii = 1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   i11iiiiiiii = 1;
   i12iiiiiiiii = 1;
   i13iiiiiiiiii = 1;
   i14iiiiiiiiiii = 1;
   i15iiiiiiiiiiii = 1;
   i16iiiiiiiiiiiii = 1;
   i17iiiiiiiiiiiiii = 1;
   i18iiiiiiiiiiiiiii = 1;
   i19iiiiiiiiiiiiiiii = 1;
   i20iiiiiiiiiiiiiiiii = 1;
procedure signif;
const
   i3j = 0;
   i4ij = 0;
   i5iij = 0;
   i6iiij = 0;
   i7iiiij = 0;
   i8iiiiij = 0;
   i9iiiiiij = 0;
   i10iiiiiij = 0;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   i11iiiiiiij = 0;
   i12iiiiiiiij = 0;
   i13iiiiiiiiij = 0;
   i14iiiiiiiiiij = 0;
   i15iiiiiiiiiiij = 0;
   i16iiiiiiiiiiiij = 0;
   i17iiiiiiiiiiiiij = 0;
   i18iiiiiiiiiiiiiij = 0;
   i19iiiiiiiiiiiiiiij = 0;
   i20iiiiiiiiiiiiiiiij = 0;
var
   x : integer;
begin
   x:=i3i + i4ii + i5iii + i6iiii + i7iiiii + i8iiiiii +
      i9iiiiiii + i10iiiiiii + i11iiiiiiii + i12iiiiiiiii +
      i13iiiiiiiiii + i14iiiiiiiiiii + i15iiiiiiiiiiii +
      i16iiiiiiiiiiiii + i17iiiiiiiiiiiiii + i18iiiiiiiiiiiiiii +
      i19iiiiiiiiiiiiiiii + i20iiiiiiiiiiiiiiiii;
   if x = 20 then
      writeln(' NUMBER OF SIGNIFICANT CHARACTERS >= 20')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else
      writeln(' NUMBER OF SIGNIFICANT CHARACTERS = ', x)
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #007');
   signif;
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   