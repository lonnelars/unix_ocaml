open Unix

let buffer_size = 8192

let buffer = Bytes.create buffer_size

let file_copy input_name output_name append =
  let fd_in = openfile input_name [O_RDONLY] 0 in
  let fd_out =
    openfile output_name
      [O_WRONLY; O_CREAT; (if append then O_APPEND else O_TRUNC)]
      0o666
  in
  let rec copy_loop () =
    match read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r ->
        ignore (write fd_out buffer 0 r) ;
        copy_loop ()
  in
  copy_loop () ; close fd_in ; close fd_out

let copy () =
  let append = ref false in
  let files = ref [] in
  let opt_list =
    [ ( "-a"
      , Arg.Set append
      , "appends the contents of <input_file> to <output_file>" ) ]
  in
  let usage = "Usage: file_copy [-a] <input_file> <output_file>" in
  Arg.parse opt_list (fun file -> files := file :: !files) usage ;
  if List.length !files == 2 then (
    files := List.rev !files ;
    file_copy (List.hd !files) (List.hd (List.tl !files)) !append ;
    exit 0 )
  else ( prerr_endline usage ; exit 1 )

let () = handle_unix_error copy ()
