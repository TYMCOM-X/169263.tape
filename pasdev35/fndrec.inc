$PAGE recognizer declarations
(*  ClrRecognizer records that no recognizer has been constructed yet.
    SetRecognizer constructs a recognizer for a specified pattern, making
    it the current recognizer.  *)

external procedure clr_recognizer;
external procedure set_recognizer ( pattern );


(*  Match determines whether a text string is matched by the current recognizer.  *)

external function match ( packed array [1..*] of char ): boolean;
 