open Eio.Std
open Benchmark_result
(* Simulate other work in the domain, and also prevent it from going to sleep.
   Otherwise, we're just measuring how long it takes the OS to wake a sleeping thread. *)
let rec spin () =
  Fiber.yield ();
  spin ()

let run_bench ~domain_mgr ~clock ~use_domains ~n_iters ~n_resources =
  let n_workers = 4 in
  let sem = Eio.Semaphore.make n_resources in
  let n_pending = Atomic.make n_workers in
  let all_started, set_all_started = Promise.create () in
  let t0 = ref 0.0 in
  let run_worker ~n_iters sem =
    Switch.run @@ fun sw ->
    Fiber.fork_daemon ~sw spin;
    if Atomic.fetch_and_add n_pending (-1) = 1 then (
      Promise.resolve set_all_started ();
      t0 := Eio.Time.now clock;
    ) else (
      Promise.await all_started
    );
    for _ = 1 to n_iters do
      Eio.Semaphore.acquire sem;
      Fiber.yield ();
      Eio.Semaphore.release sem
    done
  in
  let run () =
    if use_domains then (
      Eio.Domain_manager.run domain_mgr @@ fun () ->
      run_worker ~n_iters sem
    ) else (
      run_worker ~n_iters sem
    )
  in
  Gc.full_major ();
  Fiber.all (List.init n_workers (Fun.const run));
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. !t0 in
  let time_per_iter = time_total /. float n_iters in
  Metric.create ("time_per_iter/" ^ (string_of_int n_iters) ^ "_" ^ (string_of_int n_resources)) (`Numeric (1e9 *. time_per_iter)) "ns/iter" "Time taken per iteration"
  (* Printf.printf "%11b, %8d, %11d, %8.2f, %13.4f\n%!" use_domains n_iters n_resources (1e9 *. time_per_iter) (prom /. float n_iters) *)

let main ~domain_mgr ~clock =
  let metrics =
  [false, 100_000, 2;
   false, 100_000, 3;
   false, 100_000, 4;
   true,   10_000, 2;
   true,   10_000, 3;
   true,   10_000, 4]
  |> List.map (fun (use_domains, n_iters, n_resources) ->
      run_bench ~domain_mgr ~clock ~use_domains ~n_iters ~n_resources
    ) in
  metrics

let bench_semaphore () =
  Eio_main.run @@ fun env ->
    let metrics = main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env) in
    let res = {name = "bench_semaphore"; metrics} in
    res