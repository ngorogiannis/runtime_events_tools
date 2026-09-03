type handle
(** A process we spawned: a Win32 [HANDLE] on Windows, a pid elsewhere. Produced
    by [create_process_env], consumed by [waitpid] and [terminate]. *)

val pid_of_handle : handle -> int option
(** The real OS pid, which is what the child names its ring file after. [None]
    if the handle is invalid or not queryable. *)

val is_process_alive : pid:int -> bool
(** For a process we did not spawn, and so have no handle for. *)

val create_process_env :
  string ->
  string array ->
  string array ->
  Unix.file_descr ->
  Unix.file_descr ->
  Unix.file_descr ->
  handle

val waitpid : Unix.wait_flag list -> handle -> Unix.process_status option
(** [None] means still running, and only arises under [WNOHANG]. On [Some] the
    handle has been closed and must not be used again. *)

val terminate : handle -> unit
(** SIGKILL, or TerminateProcess on Windows. Errors are swallowed. *)

val get_rss_kb : pid:int -> int
(** Peak resident set size; 0 where unsupported, which includes Windows. *)
