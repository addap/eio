open Eio.Std
open Benchmark_result
let v = ref 0

let run_sender ~iters_per_thread mutex =
  for _ = 1 to iters_per_thread do
    Eio.Mutex.lock mutex;
    let x = !v in
    v := x + 1;
    Fiber.yield ();
    assert (!v = x + 1);
    v := x;
    Eio.Mutex.unlock mutex;
  done

let run_bench ~domain_mgr ~clock ~use_domains ~iters_per_thread ~threads =
  let mutex = Eio.Mutex.create () in
  Gc.full_major ();
  let _minor0, prom0, _major0 = Gc.counters () in
  let t0 = Eio.Time.now clock in
  Switch.run (fun sw ->
      for _ = 1 to threads do
        Fiber.fork ~sw (fun () ->
            if use_domains then (
              Eio.Domain_manager.run domain_mgr @@ fun () ->
              run_sender ~iters_per_thread mutex
            ) else (
              run_sender ~iters_per_thread mutex
            )
          )
      done
    );
  assert (!v = 0);
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. t0 in
  let n_iters = iters_per_thread * threads in
  let time_per_iter = time_total /. float n_iters in
  let _minor1, prom1, _major1 = Gc.counters () in
  let prom = prom1 -. prom0 in
  Metric.create ("time_per_iter/" ^ (string_of_int n_iters) ^ "/" ^ (string_of_int threads)) (`Numeric (1e9 *. time_per_iter)) "ns/iter" "Time taken per iteration" ::
  Metric.create ("promoted_per_iter/" ^ (string_of_int n_iters) ^ "/" ^ (string_of_int threads)) (`Numeric (prom /. float n_iters)) "promoted/iter" "No. of promotions" :: []
  (* Printf.printf "%11b, %12d, %8d, %8.2f, %13.4f\n%!" use_domains n_iters threads (1e9 *. time_per_iter) (prom /. float n_iters) *)

let main ~domain_mgr ~clock =
  let metrics =
  [false, 1_000_000, 1;
   false, 1_000_000, 2;
   false,   100_000, 8;
   true,    100_000, 1;
   true,     10_000, 2;
   true,     10_000, 8]
  |> List.map (fun (use_domains, iters_per_thread, threads) ->
      run_bench ~domain_mgr ~clock ~use_domains ~iters_per_thread ~threads
    ) in
  List.flatten metrics

let bench_mutex () =
  Eio_main.run @@ fun env ->
let metrics = main
~domain_mgr:(Eio.Stdenv.domain_mgr env)
~clock:(Eio.Stdenv.clock env) in
let res = {name = "bench_mutex"; metrics} in
res