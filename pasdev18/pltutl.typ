
type
  xyz =
    record
      x, y, z: real			(* 3d coordinates, scaling factors, rotations, etc. *)
    end;

  point = xyz;

  matrix = array [1..4, 1..4] of real;	(* transformation matrices *)

type
  plotters = (ASK, HPA, HPB, TEK);

  pen_colors = (none, black, red, green, blue);

  plt_state =
      record
	window: matrix;			(* window matrix *)
	transform: matrix;		(* current transformation matrix *)
	cursor: point;			(* logical position of pen *)
	plotter_type: plotters;		(* plotter to use *)
	color: pen_colors		(* color being used *)
      end;
 