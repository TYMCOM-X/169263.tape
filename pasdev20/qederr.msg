0,1	No error reported.

1,1	Fatal error. You must reenter all edits made after the last write.
	Please save this output and contact your MDSI representative.

2,2	Line address out of range.

3,2	First address is out of range.

4,2	Second address is out of range.

5,2	Second address is before first.

6,2	Wrong number of addresses.

7,2	Invalid command.

8,2	Command invalid in this context.

9,2	Missing file name.

10,2	File not found.

11,2	Closing delimiter not found for search string.

12,2	// not defined.

13,2	Missing line designator after comma.

14,2	Search string expected after parenthesis.

15,2	Missing closing parenthesis.

16,2	Missing operand after NOT.

17,2	Missing operand after AND, OR.

18,2	Negative offset must follow other address.

19,2	Number expected after '+' or '-'.

20,2	Must be first part of line designator.

21,2	'*' must be used after comma.

22,2	Search failed.

23,2	Address out of range.

24,2	Search will not match any line.

25,2	Line too long.

26,2	Interal error in QED. A QUIT command has been issued. Please
	save this output and contact your MDSI representative.

27,2	Search failed. Section not found.

28,2	No closing delimiter for replacement string.

29,2	Invalid command option.

30,2	Extraneous characters follow command.

31,2	Line number too large.

32,2	No search specified for FIND.

33,2	Incorrect destination specified for MOVE.

34,2	Cannot move text into itself.

35,2	Output file not open.

36,2	Missing or invalid SET parameter.

37,2	No value given for a SET parameter.

38,2	Must join at least two lines.

39,2	Invalid or missing pattern for SPLIT.

40,2	Buffer is empty.

41,2	Not enough leading space to indent line.

42,2	A write to a file or to the terminal failed.

43,2    String match in first line address failed.

44,2    String match in second line address failed.

45,2    First line address out of range.

46,2    Second line address out of range.

47,2    Storage capacity exceeded.

48,2	An unmatchable Nth occurrance pattern was detected.

5,3	The line designator specified first in a range of lines is numerically
	greater than the second.

9,3	A file name given is incorrectly specified; the command used requires
	a file name and one was not specified; or a file was not specified,
	and there is no default file.

30,3	Characters follow the command which are not part of the command.
	If you are trying to enter multiple commands on a line, separate the
	commands with semicolons.

33,3	No destination was specified for MOVE, or the Ld giving the destination
	specified a range of lines.  A single line address is required.

34,3	The destination of a MOVE is within the range of lines being moved.
	For example, 1,5 MOVE 2.

35,3	An OPEN command must be issued to designate the output file before
	attempting to OUTPUT text.

42,3	The operating system returned an error indication when a write
	to a file or the the terminal was attempted.  Possible causes
	include no room on disk or hardware failures.

47,3    The heap has overflowed while attempting to add to the buffer.
	The last line input was lost.

48,3	Nth occurrance substitutes are not legal for :string:, @string@, or
	#string# patterns.

*,10	No further information is available.

