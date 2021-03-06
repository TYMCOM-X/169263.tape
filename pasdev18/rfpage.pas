$TITLE rfpage
$LENGTH 42
$OPTIONS special
$PAGE includes and externals
$include rfdata.inc

external procedure wrline (line_type);
$PAGE static data and initialization
const wl = 1;					(* acceptable number of "widow" lines *)

static var
  buffer: line_list := (nil, nil);		(* lines buffer up for this page *)
  buffercnt: line_count;		(* physical number of lines in buffer *)
  blockcnt: line_count;			(* physical number of lines in last block of text,
					   i.e. since last $skip, $page, $need *)
  linecnt: line_count;			(* number of printing lines assembled , includes
					   spacing count *)
  extra: line_count;			(* number of physical lines more than required
					   to fill out page *)
  effective_page_length: points;	(* number of physical lines which can appear on
					   one page, i.e. length less margins, titles etc. *)

public procedure page_fmt_init;
 var t,dp: line_type;
 begin
  t := buffer.first;	(* discard any left over from last run *)
  while t <> nil do begin
    dp := t;
    t := t^.next;
    dispose (dp);
  end;
  buffer.first := nil;
  buffer.last := nil;
  buffercnt := 0;
  blockcnt := 0;
  linecnt := 0;
  extra := 0;
  effective_page_length := page.length - page.top_margin - page.bottom_margin;
 end;
$PAGE page_out

procedure page_out;
 var
   line, t: line_type;
   i: points;

 begin
  (* Check if paging at this point will produce unacceptable "widows" at the
     top of the next page or at the bottom of the current page.  If so move
     text in the last block on the page to the next page. *)

  if (extra > 0) and (blockcnt > 0) then begin	(* make sure we have a split block *)
    if extra <= wl			(* too few lines at top of next page *)
      then extra := extra + min (wl-extra, blockcnt-extra);
    if (blockcnt-extra) <= wl		(* too few lines at bottom of this page *)
      then extra := blockcnt;		(* put entire block on next page *)
  end;


  (* Output the page.  Only buffercnt-extra lines are used; the last line is
     padded with enough spacing lines to fill out the page. *)

  for i := 1 to page.top_margin do writeln (output);

  linecnt := 0;				(* to count how many lines output *)
  line := buffer.first;
  for i := 1 to (buffercnt-extra-1) do begin	(* output all but last line *)
    wrline (line);
    linecnt := linecnt + 1 + line^.spacing;	(* see how many lines written *)
    t := line;					(* advance to the next line, and dispose *)
    line := line^.next;
    dispose (t);
  end;
  line^.spacing := effective_page_length - linecnt - 1;	(* output last line, padding to end of page *)
  wrline (line);
  buffer.first := line^.next;			(* truncate buffer to "extra" lines *)
  dispose (line);				(* discard the last line on the page *)

  for i := 1 to page.bottom_margin do writeln (output);

  (* The only lines in the buffer now are the "extra" lines, if any. Update
     the pagination information to reflect only those lines. *)

  line := buffer.first;				(* get print count for extra lines *)
  linecnt := 0;
  while line <> nil do begin
    linecnt := linecnt + 1 + line^.spacing;
    line := line^.next;
  end;
  buffercnt := extra;				(* buffersize is that of remaining lines *)
  extra := 0;
  blockcnt := 0;

  (* Finally, update the pagination parameters for the next page. *)

  page := next_page;
  effective_page_length := page.length - page.top_margin - page.bottom_margin;
 end;
$PAGE put_page

public procedure put_page;
 begin
  if linecnt > 0 then page_out;		(* only page, if not at the top of a page *)
 end;
$PAGE put_skip

public procedure put_skip (cnt: line_count);
 begin
  (* A skip directive terminates a block of text.  Therefore, it is possible at
     this point to check if the block fits on the current and next pages without
     leaving "widows".  So, if the last block crosses a page boundary, flush the
     output buffer.  This will cause a page to be output including only the
     acceptable number of lines from the block.  Note that there will always be
     lines left in the buffer after the operation at this point. *)

  if extra > 0 then page_out;			(* if we have more than enough .... *)
  blockcnt := 0;				(* terminate the block, even if we didn't page *)

  (* A skip directive should never leave blank lines at the top of a page,
     except when it follows a $page directive (or $need directive which forces
     a paging).  As the above operation always leaves lines on the (next) page,
     we cannot be at the top of a page unless forced by a $page or $need. So,
     if honoring this skip request would cross a page boundary, simulate a $page
     instead, to move to the top of the page.  Otherwise, continue and perform
     the skip operation. *)

  if (linecnt + cnt) > effective_page_length then begin
    page_out;
    return;
  end;

  (* Perform the skip.  If there are lines in the buffer, append the spacing
     to the last line, otherwise, create a null first line and add the spacing
     to it. *)

  if buffer.first = nil then begin
    new (buffer.first);
    with buffer.first^ do begin			(* fill in the info *)
      textlen := 0;
      total_width := 0;
      indentation := 0;
      spacing := cnt - 1;		(* one line had from null line *)
      number := 0;
      next := nil;
    end;
    buffer.last := buffer.first;
  end
  else with buffer.last^ do spacing := spacing + cnt;
  linecnt := linecnt + cnt;			(* add to partial page length *)
 end;
$PAGE put_need

public procedure put_need (cnt: line_count);
 begin
  if extra > 0 then page_out;		(* logic follows that of skip *)
  blockcnt := 0;
  if (linecnt + cnt) > effective_page_width then page_out;	(* page if needed length, not
								   available *)
 end;
$PAGE put_line

public procedure put_line (line: line_type);
 begin
  if buffer.first = nil		(* add line to buffer *)
    then buffer.first := line
    else buffer.last^.next := line;
  buffer.last := line;
  line^.next := nil;
  buffercnt := buffercnt + 1;

  blockcnt := blockcnt + 1;	(* add this line to current block *)

  (* Check whether the line fits on the current page, or if it will go on
     the next page.  In the latter case, it is charged against the "extra"
     count.  Note that if spacing > 1, we only charge the line, and not the
     additional spacing against the current page before checking for page
     length overflow.  This is because the blank lines are not printing at
     the end of a page. *)

  linecnt := linecnt + 1;
  if linecnt > effective_page_length then begin	(* line should really go on next page *)
    extra := extra + 1;
    if extra > wl then page_out;	(* force out page when we know that we do not
					   need extra lines from previous page *)
  end
  else linecnt := linecnt + line^.spacing;	(* line will appear on the current page,
						   charge the spacing agains the page *)
 end.
  