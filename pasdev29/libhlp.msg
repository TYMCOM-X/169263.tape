                              File Stash Facility
     Command Set -

-OPEN library-file name
        Open an existing or create a new library file.

-ADD filename [,filename]*
        Add the specified file(s) to the library.

-EXTRACT filename [,filename]*
        Copy the specified library segments to  external  files.  The  file
        names may use wildcarding.

-DELETE filename [,filename]*
        Delete  the  specified library segments from the library.  The file
        names may use wildcarding.

-DIRECTORY [/A] [ filename [,filename]*]
	List the specified library segments. If none is specified, list all
	segments. Wildcarding may be used.
	The /A option causes DIR to give more information about each of the
	listed segments.

-PACK 
        Pack the library file.

-HELP [ <command> ]
        Print helpful information about the specified command. With
        no command specified, print information about all commands.

-QUIT 
        Close the library file and exit.
-
    