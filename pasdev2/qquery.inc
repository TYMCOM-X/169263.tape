
(* QUERY asks a caller supplied question, and checks for a yes or no reply.
   Y, YES, NO, N, or <eoln> meaning yes are accepted as replys. Also,
   REPEAT is accepted as a request to repeat the question.  If no such 
   replies are received, another response is requested and processed. Notes:
   This routine appends a question mark to the question. It is assumed that
   the terminal is in the desired column (i.e. one) when called. *)

TYPE QUERY_STRING = STRING[256];

EXTERNAL FUNCTION QUERY (QUESTION: QUERY_STRING): BOOLEAN;

   