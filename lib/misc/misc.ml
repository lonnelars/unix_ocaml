open Unix

let try_finalize f x finally y =
  let res = try f x with exn -> finally y ; raise exn in
  finally y ; res

let iter_dir f dirname =
  let d = opendir dirname in
  try
    while true do
      f (readdir d)
    done
  with End_of_file -> closedir d
