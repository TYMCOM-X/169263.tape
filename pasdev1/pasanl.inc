external var       ptree: parse_node;
external procedure initparse;			(* INITIALIZE THE PARSER *)
external procedure get_ptree; (* GET THE NEXT PARSE TREE *)
external procedure del_ptree ( parse_node );	(* DELETE A PARSE TREE *)
external procedure endparse;			(* TERMINATE PARSING *)
 