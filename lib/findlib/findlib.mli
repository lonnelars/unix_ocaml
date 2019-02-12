val find :
     (Unix.error * string * string -> unit)
  -> (string -> Unix.stats -> bool)
  -> bool
  -> int
  -> string list
  -> unit
