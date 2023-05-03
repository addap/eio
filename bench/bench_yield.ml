open Eio.Std
open Benchmark_result

let n_fibers = [1; 2; 3; 4; 5; 10; 20; 30; 40; 50; 100; 500; 1000; 10000]

let main ~clock =
  (* Printf.printf "n_fibers, ns/iter, promoted/iter\n%!"; *)
   let metrics = n_fibers |> List.map (fun n_fibers ->
      let n_iters = 1000000 / n_fibers in
      Gc.full_major ();
      let _minor0, prom0, _major0 = Gc.counters () in
      let t0 = Eio.Time.now clock in
      Switch.run (fun sw ->
          for _ = 1 to n_fibers do
            Fiber.fork ~sw (fun () ->
                for _ = 1 to n_iters do
                  Fiber.yield ()
                done
              )
          done
        );
      let t1 = Eio.Time.now clock in
      let time_total = t1 -. t0 in
      let n_total = n_fibers * n_iters in
      let time_per_iter = time_total /. float n_total in
      let _minor1, prom1, _major1 = Gc.counters () in
      let prom = prom1 -. prom0 in
      Metric.create ("time_per_iter/" ^ (string_of_int n_fibers)) (`Numeric (1e9 *. time_per_iter)) "ns/time_per_iter" "Time taken per iteration" ::
      Metric.create ("promoted/" ^ (string_of_int n_fibers)) (`Numeric (prom /. float n_total)) "no. of promotions" "Total promoted objects" :: []
      (* Printf.printf "%8d, % 7.2f, % 13.4f\n%!" n_fibers (1e9 *. time_per_iter) (prom /. float n_total) *)
    ) in
  List.flatten metrics


let bench_yield () =
  Eio_main.run @@ fun env ->
  let metrics = main ~clock:(Eio.Stdenv.clock env) in
  let results = {name = "bench_yield"; metrics} in
  results
