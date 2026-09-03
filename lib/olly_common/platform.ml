type handle = int

external olly_is_process_alive : int -> bool = "olly_is_process_alive"

let is_process_alive ~pid = olly_is_process_alive pid

external olly_pid_of_handle : int -> int = "olly_pid_of_handle"

let pid_of_handle handle =
  let pid = olly_pid_of_handle handle in
  if pid = 0 then None else Some pid

let create_process_env = Unix.create_process_env

let waitpid flags handle =
  match Unix.waitpid flags handle with 0, _ -> None | _, status -> Some status

let terminate h = try Unix.kill h Sys.sigkill with Unix.Unix_error _ -> ()

external olly_get_rss_kb : int -> int = "olly_get_rss_kb"

let get_rss_kb ~pid = olly_get_rss_kb pid
