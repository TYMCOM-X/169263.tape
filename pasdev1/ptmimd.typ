type
   extname = string [40];

   condtype = ( cnmath_error, cnio_error, cnuser_error, cnattention,
		cnstorage_overflow, cnstack_overflow, cnspecial_error );

   condnames = array [condtype] of extname;
   