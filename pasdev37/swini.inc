type sw_ini_name = string [6];
    sw_ini_string = string [128];

external function sw_ini ( prog: sw_ini_name;
			   opt: sw_ini_name;
			   var switches: sw_ini_string ): boolean;
  