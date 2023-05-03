open Eio.Std
open Benchmark_result

(* The main domain spawns two other domains, connected to each by a stream.
   It keeps reading from whichever stream is ready first, cancelling the other read.
   This tests the time needed to set up and tear down cancellation contexts and
   tests that cancellation can happen in parallel with success. *)

let n_iters = 100_000

let run_sender stream =
  for i = 1 to n_iters do
    Eio.Stream.add stream i
  done

let run_bench ?domain_mgr ~clock () =
  let stream1 = Eio.Stream.create 1 in
  let stream2 = Eio.Stream.create 1 in
  let run_sender stream () =
    match domain_mgr with
    | Some dm -> Eio.Domain_manager.run dm (fun () -> run_sender stream)
    | None -> run_sender stream
  in
  let name str =
    match domain_mgr with
    | Some _ -> str ^ "_domain"
    | None -> str
  in
  Gc.full_major ();
  let _minor0, prom0, _major0 = Gc.counters () in
  let t0 = Eio.Time.now clock in
  try
    Switch.run (fun sw ->
        Fiber.fork ~sw (run_sender stream1);
        Fiber.fork ~sw (run_sender stream2);
        for _ = 1 to n_iters do
          ignore @@
          Fiber.first
            (fun () -> Eio.Stream.take stream1)
            (fun () -> Eio.Stream.take stream2)
        done;
        raise Exit
      )
  with Exit ->
    let t1 = Eio.Time.now clock in
    let time_total = t1 -. t0 in
    let time_per_iter = time_total /. float n_iters in
    let _minor1, prom1, _major1 = Gc.counters () in
    let prom = prom1 -. prom0 in
    (* Printf.printf "%11b, %7.2f, %13.4f\n%!" (domain_mgr <> None) (1e9 *. time_per_iter) (prom /. float n_iters) *)
    Metric.create (name "time_per_iter") (`Numeric (1e9 *. time_per_iter)) "ns/iter" "Measures time taken per iter" ::
    Metric.create (name "promotions") (`Numeric prom) "no" "Number of promotions" :: []

let main ~domain_mgr ~clock =
  (* Printf.printf "use_domains,  ns/iter, promoted/iter\n%!"; *)
  let m1 = run_bench ~clock () in
  let m2 = run_bench ~domain_mgr ~clock () in
  m1 @ m2


let bench_cancel () =
  Eio_main.run @@ fun env ->
  let metrics = (main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)) in
  let results = {name = "cancel_bench"; metrics} in
  (* Printf.printf "%s\n" res; *)
  results
  (* let output =
    Printf.sprintf {| {"name": "Bench_cancel", "results": [%s]}|} res in
  Printf.printf "%s\n" (Yojson.Basic.prettify output) *)
