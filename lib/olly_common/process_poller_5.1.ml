external rss_and_ring_kb : int -> string -> int * int = "olly_rss_and_ring_kb"

(* Sample the RSS of [pid], excluding the resident pages of its
   runtime_events ring buffer [ring_file], and fold it into [peak_rss].

   The ring is mapped into the traced process, so its resident pages —
   routinely two orders of magnitude more than the program's own live
   memory — are charged to the process's RSS.  Where the platform can
   attribute them to the ring we subtract them; where it cannot,
   [rss_and_ring_kb] reports -1 and we record that the peak still
   includes the ring rather than silently reporting an inflated
   footprint. *)
let sample_peak_rss ~pid ~ring_file ~peak_rss ~excludes_ring =
  let rss_kb, ring_kb = rss_and_ring_kb pid ring_file in
  if ring_kb < 0 then Atomic.set excludes_ring false;
  (* The ring is shared with this process, so in principle it could hold
     resident pages the traced process never faulted in; clamp rather
     than report a negative footprint. *)
  let rss_kb = if ring_kb > 0 then max 0 (rss_kb - ring_kb) else rss_kb in
  Atomic.set peak_rss (max rss_kb (Atomic.get peak_rss))

(* [stop_rd]/[stop_wr] form a self-pipe, used for early termination of the 
   [select] in the polling domain. *)
type t = {
  stop_rd : Unix.file_descr;
  stop_wr : Unix.file_descr;
  domain : unit Domain.t;
  alive : bool Atomic.t;
  peak_rss : int Atomic.t;
  excludes_ring : bool Atomic.t;
}

let poller : t option Atomic.t = Atomic.make None

(** sleeps at least [interval] seconds, or until one of [read_fds] is ready for
    reading. returns [false] iff it returns because of the latter condition and
    [true] otherwise. *)
let rec sleep_until_write read_fds interval =
  if interval > 0.0 then
    let start_time = Unix.gettimeofday () in
    try
      let ready_fds, _, _ = Unix.select read_fds [] [] interval in
      List.is_empty ready_fds
    with Unix.Unix_error (EINTR, _, _) ->
      let elapsed = Unix.gettimeofday () -. start_time in
      sleep_until_write read_fds (interval -. elapsed)
  else true

let start ~alive_check ~pid ~ring_file ~interval ~sample_rss =
  if Option.is_some (Atomic.get poller) then
    failwith "Process poller already started";
  if interval <= 0.0 then invalid_arg "interval must be positive";
  (* [socketpair] used for Windows compatibility *)
  let stop_rd, stop_wr =
    Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let alive = Atomic.make true in
  let peak_rss = Atomic.make 0 in
  let excludes_ring = Atomic.make true in
  let rec start_loop read_fds =
    let still_alive = alive_check () in
    Atomic.set alive still_alive;
    if still_alive then (
      if sample_rss then
        sample_peak_rss ~pid ~ring_file ~peak_rss ~excludes_ring;
      (* wait for [interval], or until signalled to stop *)
      if sleep_until_write read_fds interval then start_loop read_fds)
  in
  let domain =
    Domain.spawn (fun () ->
        Fun.protect
          ~finally:(fun () -> Atomic.set alive false)
          (fun () -> start_loop [ stop_rd ]))
  in
  Atomic.set poller
    (Some { stop_rd; stop_wr; domain; alive; peak_rss; excludes_ring })

let is_alive () =
  match Atomic.get poller with
  | None -> failwith "Process poller not started"
  | Some t -> Atomic.get t.alive

let peak_rss () =
  match Atomic.get poller with
  | None -> failwith "Process poller not started"
  | Some t -> Atomic.get t.peak_rss

let peak_rss_excludes_ring () =
  match Atomic.get poller with
  | None -> failwith "Process poller not started"
  | Some t -> Atomic.get t.excludes_ring

let stop () =
  match Atomic.get poller with
  | None -> failwith "Process poller not running"
  | Some { stop_wr; stop_rd; domain; _ } ->
      (* wake up [domain] *)
      ignore (Unix.write stop_wr (Bytes.make 1 '\000') 0 1 : int);
      Domain.join domain;
      Unix.close stop_wr;
      Unix.close stop_rd;
      ()
