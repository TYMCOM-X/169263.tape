module mapimg options special(ptr,coercions);

$INCLUDE mapimg.typ

type 
  address_limits_array = array [1..2] of integer;

external function sys$crmpsc ( address_limits_array; var address_limits_array;
  integer; integer; var integer; var integer; integer; integer;  integer;
  integer; integer; integer ): integer;

external function sys$deltva ( address_limits_array; var address_limits_array;
		integer ) : integer;

external procedure user_file_open ( file_name; var integer; var integer );

external procedure user_file_close ( integer; var integer );

type address_list = record
	limits : address_limits_array;
	next   : ^address_list
     end;
const fake_channel : integer := -1;
$PAGE ovl$main_filler_err, ovl$link_filler_err, ovl$plot_filler_err

(* These routines are error routines which are called when a filler transfer
   vector entry is called. *)

public procedure ovl$main_filler_err;
begin
  writeln ( tty , '?OVLERR - Main transfer vector filler entry called.' )
end;

public procedure ovl$link_filler_err;
begin
  writeln ( tty , '?OVLERR - Link transfer vector filler entry called.' )
end;

public procedure ovl$plot_filler_err;
begin
  writeln ( tty , '?OVLERR - Plot transfer vector filler entry called.' )
end;
$PAGE dispose_addr_list

(* This procedure simply disposes the storage associated with an address
   list, and sets the value of the address list pointer to nil. *)

procedure dispose_addr_list ( var list : ^address_list );

var temp : ^address_list;

begin

  while list <> nil do begin
    temp := list;
    list := list^.next;
    dispose ( temp )
  end

end;		(* dispose_addr_list *)
$PAGE map_image_file

(* MAP_IMAGE_FILE is given the name of a VAX image (i.e., '.EXE')
   file and maps the file into the address space of the current image.
   Those image sections which are physically contained in the file
   are mapped at the virtual addresses specified in the image 
   section descriptors following the image header in the EXE file.
   Parameter NAME is the image file name.  Parameter STATUS is an
   enumerated type indicating the success or failure of the mapping.
   parameter CHANNEL indicates the channel that is associated with the image file.
   Parameter LIST contains a pointer chain containing the mapped virtual
   addresses used during this virtual address map. This list of addresses
   must be dmapped before the channel to the image file can be disassociated. *)

procedure map_image_file ( name: file_name; var status: mif_status;
			channel: integer; var list : ^address_list );

label
  900;

(* ERROR sets the status variable and branches to the return point of
   MAP_IMAGE_FILE.  *)

procedure error ( error_status: mif_status );
  begin
    dispose_addr_list ( list );		(* Because the addresses are invalid *)
    status := error_status;
    goto 900;
  end;

type
  uns_word = 0..177777b;
  packed_uns_word = packed [16] minimum(uns_word)..maximum(uns_word);

const
  vbn_offset: packed_uns_word = 12;	(* Offset in ISD of file virtual block number field *)
  zero: packed_uns_word = 0;
  page_count_offset = 2;		(* offset in ISD of count of pages in image section *)
  vpn_offset = 4;			(* offset in ISD of image virtual page field *)
  flags_offset = 8;			(* offset in ISD of flags field *)
  sec$m_wrt = 8;			(* writable flag for map system call *)
  sec$m_crf = 2;			(* copy-on-reference flag for map system call *)
  wrt_flag = 3;				(* writable flag bit in ISD flags *)
  crf_flag = 1;				(* copy-on-reference flag in ISD flags *)

var
  f: file of *;
  image_header_size: packed_uns_word;
  isd_size: packed_uns_word;
  current_isd_base: uns_word;
  file_vbn: 0..maximum(integer);
  page_count: packed_uns_word;
  image_vpn: integer;
  isd_flags: set of 0..23;
  virtual_address_limits: address_limits_array;
  mapping_flags: integer;
  open_error: integer;
  dummy_arg: integer;
  map_status: integer;
  temp : ^address_list;

begin

  (* Open the image file.  *)

  status := mif_ok;
  list := nil;			(* No addresses mapped at the beginning *)
  reset ( f, name, [seekok, image] );
  if iostatus ( f ) <> io_ok then error ( mif_open_failure );

  (* Open the file again with a macro routine which will return
     the channel number allocated.  *)


  (* Read the first word of the file - the image header size.  The first
     image section descriptor (ISD) begins immediately after the image
     header.  *)

  read ( f, image_header_size );
  current_isd_base := image_header_size + 1;
  readrn ( f, current_isd_base, isd_size );	(* read size of first ISD *)

  (* The following loop reads and processes one ISD per iteration.
     The last ISD is followed by a zero word.  *)

  while isd_size <> zero do begin

    (* If the ISD size is less than or equal to the virtual block number (VBN)
       offset then it does not include the file virtual block number
       field and thus cannot describe an image section actually in the file
       and thus may be ignored.  *)

    if isd_size > vbn_offset then begin
    
      (* Read virtual block number of start of the image section within
	 the image file.  If it is zero, we can ignore the image section.  *)

      readrn ( f, current_isd_base + vbn_offset, file_vbn );

      if file_vbn <> 0 then begin

	(* Read those additional fields necessary for the create and
	   map section system call: the section page count, the base
	   virtual page number of the section within the image and the
	   image section flags.  *)

	readrn ( f, current_isd_base + page_count_offset, page_count );
	image_vpn := 0;
	readrn ( f, current_isd_base + vpn_offset, image_vpn:3 );
	readrn ( f, current_isd_base + flags_offset, isd_flags );

  	(* Set up the parameters for the system call: the images
	   virtual address limits and the mapping flags.  *)

	virtual_address_limits[ 1 ] := image_vpn * 512;
	virtual_address_limits[ 2 ] := ( (image_vpn + page_count) * 512) - 1;
	mapping_flags := 0;
	if wrt_flag in isd_flags then mapping_flags := mapping_flags + sec$m_wrt;
	if (crf_flag in isd_flags)
	  then mapping_flags := mapping_flags + sec$m_crf;
        dummy_arg := 0;

	map_status := sys$crmpsc ( virtual_address_limits, virtual_address_limits, dummy_arg,
	  mapping_flags, dummy_arg, dummy_arg, dummy_arg, channel,
	  page_count, file_vbn, dummy_arg, dummy_arg );
	temp := list;
	new ( list );
	(* The virtual address array is reversed because DELTVA wants the
	   opposite that was returned from CRMPSC. See system services *)

	list^ := ( (virtual_address_limits[2],virtual_address_limits[1]), temp );
	if not odd ( map_status ) then error ( mif_mapping_failure );
      end  (* if file_vbn <> 0 ... *);
    end  (* if isd_size > vbn_offset.... *) ;
    
    (* Read the size field of the next ISD.  *)

    current_isd_base := current_isd_base + isd_size;
    readrn ( f, current_isd_base, isd_size );

  end  (* while *) ;

  (* Close that open file *)

  close ( f );
  if iostatus ( f ) <> io_ok then error ( mif_close_failure );

900:

end  (* proc map_image_file *) ;
$PAGE free_channel

(* This routine frees the virtual address (releases?) associated and then 
   deassigns the channel that is associated with the given area. The
   addresses that are mapped are stored in teh variable, ADDR_LIST.
   The channel is in parameter CHANNEL. If an error occurs during the
   "un-mapping" or during the deassign, return the value MIF_CLOSE_FAILURE
   in the STATUS parameter. If all goes according to plan, return MIF_OK *)

procedure free_channel ( channel : integer;
		     var addr_list : ^address_list;
		     var status : mif_status );

var temp : ^address_list;
    err  : integer;
    dummy_arg : integer;

begin

  status := mif_ok;

  if channel <> fake_channel
    then begin
      temp := addr_list;

      while ( temp <> nil ) and ( status = mif_ok ) do begin
	err := sys$deltva ( temp^.limits , temp^.limits , dummy_arg );
	temp := temp^.next;
	if not odd ( err )
	  then status := mif_close_failure
      end;

      dispose_addr_list ( addr_list );

      if status = mif_ok
	then begin
	  user_file_close ( channel , err );
	  if err <> 0
	    then status := mif_close_failure
	end
  end

end;		(* free_channel *)
$PAGE open_and_map_image

(* This procedure opens and maps an image file. It opens the
   file through a call to user_file_open. Once the file
   os opened successfully, the procedure then calls map_image_file
   to have the file mapped. This routine returns parameters
   indicating the channel associated with the image, CHANNEL, the
   list of addresses that were mapped, ADDR_LIST, and the sstatus
   whether the map was successful, STATUS. If everything worked properly,
   this routine returns STATUS = mif_ok. If there was a problem
   in getting the file open, this routine returns STATUS = mif_open_failure.
   If this routine opens the file successfully, then this routine
   will return the status value that map_image_file returned to it. *)

procedure open_and_map_image (
	name : file_name;
    var channel : integer;
    var addr_list : ^address_list;
    var status : mif_status );

var err : integer;

begin

  user_file_open ( name , channel , err );

  if err <> 0
    then begin
      channel := fake_channel;
      status := mif_open_failure;
      addr_list := nil
    end
  else begin
    map_image_file ( name , status , channel , addr_list )
  end

end;		(* open_and_map_image *)
$PAGE map_plot_image

public procedure map_plot_image ( name: file_name; var status: mif_status );

static var channel : integer := fake_channel;
static var addr_list : ^address_list := nil;

begin

  free_channel ( channel , addr_list , status );

  if status = mif_ok
    then open_and_map_image ( name , channel , addr_list , status )

end;	(* map_plot_image *)
$PAGE map_link_image

public procedure map_link_image ( name: file_name; var status: mif_status );

static var channel : integer := fake_channel;
static var addr_list : ^address_list := nil;


begin

  free_channel ( channel , addr_list , status );

  if status = mif_ok
    then open_and_map_image ( name , channel , addr_list , status )

end.		(* map_link_image *)
