open Lib
open Unix

let main () =
  let action p infos =
    let git_dir = infos.st_kind = S_DIR && Filename.basename p = ".git" in
    if not git_dir then print_endline p ;
    not git_dir
  in
  let errors = ref false in
  let error (e, _, b) =
    errors := true ;
    prerr_endline (b ^ ": " ^ error_message e)
  in
  Findlib.find error action false max_int ["."]

;;
handle_unix_error main ()
