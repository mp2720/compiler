type loc = { lnum : int; cnum : int }

type loc2 = loc * loc
(** Start and end loc. *)

type diagn_msg = Err of loc2 option * string | Warn of loc2 option * string
