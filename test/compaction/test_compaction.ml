module IntMap = Map.Make (Int)

(* will create a map with [2^exponent] elements *)
let exponent = 23
let n_domains = 2

let rec build i map =
  if i <= 0 then map else IntMap.add i (Int.to_string i) map |> build (i - 1)

let rec mutate i map =
  if i <= 0 then map else IntMap.remove i map |> mutate (i - 1)

let test_compaction elements n_domains =
  Printf.eprintf "Creating map.\n%!";
  let map = build elements IntMap.empty in
  Printf.eprintf "Creating domains.\n%!";
  let domains =
    List.init n_domains (fun _ -> Domain.spawn (fun () -> mutate elements map))
  in
  Printf.eprintf "Compacting.\n%!";
  Gc.full_major ();
  Printf.eprintf "Joining domains.\n%!";
  List.iter (fun domain -> Domain.join domain |> ignore) domains

let () = test_compaction (1 lsl exponent) n_domains
