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
  let rss_kb =
    if ring_kb > 0 then
      (* The ring is shared with this process, so in principle it could hold
         resident pages the traced process never faulted in; clamp rather
         than report a negative footprint. *)
      max 0 (rss_kb - ring_kb)
    else rss_kb
  in
  Atomic.set peak_rss (max rss_kb (Atomic.get peak_rss))

type t = {
  stop_flag : bool Atomic.t;
  domain : unit Domain.t;
  alive : bool Atomic.t;
  peak_rss : int Atomic.t;
  excludes_ring : bool Atomic.t;
}

let poller : t option Atomic.t = Atomic.make None

let rec sleep_at_least stop_flag interval =
  if interval > 0.0 then
    let start_time = Unix.gettimeofday () in
    try Unix.sleepf interval
    with Unix.Unix_error (Unix.EINTR, _, _) ->
      if not @@ Atomic.get stop_flag then
        let elapsed = Unix.gettimeofday () -. start_time in
        sleep_at_least stop_flag (interval -. elapsed)

let start ~alive_check ~pid ~ring_file ~interval ~sample_rss =
  if Option.is_some (Atomic.get poller) then
    failwith "Process poller already started";
  if interval <= 0.0 then invalid_arg "interval must be positive";
  let stop_flag = Atomic.make false in
  let alive = Atomic.make true in
  let peak_rss = Atomic.make 0 in
  let excludes_ring = Atomic.make true in
  let rec start_loop () =
    if not @@ Atomic.get stop_flag then (
      let still_alive = alive_check () in
      Atomic.set alive still_alive;
      if still_alive then (
        if sample_rss then
          sample_peak_rss ~pid ~ring_file ~peak_rss ~excludes_ring;
        sleep_at_least stop_flag interval;
        start_loop ()))
  in
  let domain =
    Domain.spawn (fun () ->
        Fun.protect ~finally:(fun () -> Atomic.set alive false) start_loop)
  in
  Atomic.set poller (Some { stop_flag; domain; alive; peak_rss; excludes_ring })

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
  | Some { stop_flag; domain; _ } ->
      Atomic.set stop_flag true;
      Domain.join domain
