open Unix

type in_channel =
  { in_buffer: string
  ; in_fd: file_descr
  ; mutable in_pos: int
  ; mutable in_end: int }

exception End_of_file

let buffer_size = 8192

let open_in filename =
  { in_buffer= Bytes.create buffer_size
  ; in_fd= openfile filename [O_RDONLY] 0
  ; in_pos= 0
  ; in_end= 0 }

let input_char chan =
  if chan.in_pos < chan.in_end then (
    let c = chan.in_buffer.[chan.in_pos] in
    chan.in_pos <- chan.in_pos + 1 ;
    c )
  else
    match read chan.in_fd chan.in_buffer 0 buffer_size with
    | 0 -> raise End_of_file
    | r ->
        chan.in_end <- r ;
        chan.in_pos <- 1 ;
        chan.in_buffer.[0]

let close_in chan = close chan.in_fd

type out_channel =
  {out_buffer: string; out_fd: file_descr; mutable out_pos: int}

let open_out filename =
  { out_buffer= Bytes.create 8192
  ; out_fd= openfile filename [O_WRONLY; O_TRUNC; O_CREAT] 0o666
  ; out_pos= 0 }

let output_char chan c =
  if chan.out_pos < String.length chan.out_buffer then (
    chan.out_buffer.[chan.out_pos] <- c ;
    chan.out_pos <- chan.out_pos + 1 )
  else (
    ignore (write chan.out_fd chan.out_buffer 0 chan.out_pos) ;
    chan.out_buffer.[0] <- c ;
    chan.out_pos <- 1 )

let close_out chan =
  ignore (write chan.out_fd chan.out_buffer 0 chan.out_pos) ;
  close chan.out_fd

let output_string chan s =
  let len = String.length s in
  if chan.out_pos + len < String.length chan.out_buffer then (
    Bytes.blit_string s 0 chan.out_buffer chan.out_pos len ;
    chan.out_pos <- chan.out_pos + len )
  else if chan.out_pos = 0 then ignore (write chan.out_fd s 0 len)
    (* the string is longer than the buffer *)
  else (
    (* this can be improved by writing as much of the string as we can, then emptying the buffer, and filling the buffer again with the remainder of the string *)
    ignore (write chan.out_fd chan.out_buffer 0 chan.out_pos) ;
    Bytes.blit_string s 0 chan.out_buffer 0 len ;
    chan.out_pos <- len )
