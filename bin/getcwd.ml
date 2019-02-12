(* Move up the tree while constructing the path. When . and .. is equal, we are at the root. *)
let file_id file =
  let stats = Unix.lstat file in
  (stats.st_dev, stats.st_ino)

let current_dir_name () =
  let name = ref "" in
  let () =
    Misc.iter_dir
      (fun x ->
        if file_id (Filename.concat ".." x) = file_id Filename.current_dir_name
        then name := x )
      ".."
  in
  !name

let is_root () =
  file_id Filename.current_dir_name = file_id Filename.parent_dir_name

let main () =
  let dirs = ref [] in
  let () =
    while not (is_root ()) do
      dirs := current_dir_name () :: !dirs ;
      Unix.chdir ".."
    done
  in
  let cwd = List.fold_left Filename.concat "/" !dirs in
  print_endline cwd

let () = Unix.handle_unix_error main ()
