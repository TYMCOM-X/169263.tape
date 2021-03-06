(* QLANG.TYP - begun 8/26/82 by WNH*)

(*      This file is to be used by the compiler in compiling the types used
   in connection with prompts, and by a program which will provide files
   of prompts in languages other than English. *)

type
   qlangs = (

(*      The type QLANGS determines how many files of prompt messages will be
   generated.  One file will be generated for each permissible value of a
   variable of the type QLANGS.  In this file, each value of QLANGS is followed
   by a comment containing three things:  First, a mnemonic for the language as-
   sociated with it; Second, a file name for the resulting prompt message
   file; Third, a file name for the resulting error message file.
   The leading comment delimiter for this comment is followed by two plus signs.
   A translator or anyone else may change either the mnemonics or the
   file names in these comments to whatever is wanted.  The message file
   generating program has provisions for overriding the file names from the
   keyboard or command file.

        In this sample type file, English language messages to be used by
   the Development Software release version of QED are to be prefaced by
   OPS:, promts are to be placed in the file QPROMTS.ENG, and error messages
   are to be placed in the file QEDERR.ENG.  English language messages used by
   ANC are prefaced by ENG:, using files APROMTS.ENG and AEDERR.ENG, etc.
   It must be remembered that 
   new environments may need new file names.  VERSAdos, for instance, will
   not support three-character extensions. *)

      opsenglish, (*++ OPS: qpromt.eng qederr.eng *)
      ANCenglish, (*++ ENG: apromt.eng aederr.eng *)
      French, (*++ FRA: apromt.fra aederr.fra *)
      German (*++ DEU: apromt.deu aederr.deu *)
         );

   qprompts = (

(*      The type qprompts contains permissible values of parameters to a
   (possibly hypothetical) function prompt, which returns a string to be
   displayed on a terminal.  For each value, a text must be provided to
   correspond with each language mnemonic in the type QLANGS.  This text 
   is to be enclosed in single quotes, and is to be prefaced by two
   percent signs and its mnemonic.
       Note that some of the prompt messages require a leading or trailing
   space.  The messages preceded by OPS: show which ones these are. *)

      changes,
(* The buffer has been changed since the last write, and the changes will
   be lost if they are not written out ot a file.
%% OPS: 'Unwritten changes, OK'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      lines,
(* plural noun 'lines' as in 'lines of text'.
%% OPS: ' lines'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      qptime,
(* Singular noun 'time', in the sense 'instance'.
%% OPS: ' time'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      qptimes,
(* Plural noun 'times', as in 'several times'.
%% OPS: ' times'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      gone,
(* Command is no longer implemented.
%% OPS: 'No longer implemented - use SUBSTITUTE'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      qpconfirm,
(* First person singular interrogative:  does the operator want to have
   QED ask for permission to perform the requested operation before each
   performance?
%% OPS: 'Confirm'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      f_and_c,
(* QED found a target string and asked for confirmation (some number
   of times).
%% OPS: 'Found and confirmed '
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      found,
(* Same, but no confirmation.
%% OPS: 'Found '
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      hp_ovflw1,
(* A long and self explanatory prompt.
%% OPS: '?Error -- the heap has overflowed'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      hp_ovflw2,
(* Previous prompt continued.
%% OPS: 'Save any unwritten changes in a new file.'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      hp_ovflw3,
(* Same song, third verse.
%% OPS: 'The next heap overflow will be fatal.'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      errfil_lost,
(* The file of error messages is not available.
%% OPS: 'QED error file missing.'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
      err_notfnd,
(* The error message called for was not found in the error message file.
%% OPS: 'Error not found.'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
   bad_write,
(* Self-explanatory.
%% OPS: 'Warning--output file write incomplete'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
   new_file,
(* Is this a new file?
%% OPS: 'New file: '
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
   old_file
(* Is this an old file?
%% OPS: 'Old file: '
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
$IF new
          ,
   errmess_not_found
(* No error message is available for the error and level in question
%% OPS: 'No further information is available.'
%% ENG: 'Text not decided'
%% FRA: 'Je ne sais pas'
%% DEU: 'Ich weiss nicht'
*)
$END
      );

   prompt_text = string [50];
   