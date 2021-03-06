$TITLE PASLOG

module paslog;

$INCLUDE dtime.typ[31024,320156]
$INCLUDE paslog.typ
$INCLUDE prgdir.inc[31024,320156]

external var log_record: log_file_record;

public procedure log_write;

var log_file: file of log_file_record;

begin
  log_record.run_time := runtime - log_record.run_time;
  reset (log_file, 'DSK:PASCAL.LOG' || prgm_dir ());
  if iostatus = io_ok then begin
    close (log_file);
    rewrite (log_file, 'DSK:PASCAL.LOG' || prgm_dir (), [preserve]);
    if iostatus = io_ok then
      write (log_file, log_record);
  end;
  close (log_file);
end.
   