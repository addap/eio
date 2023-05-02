open Benchmark_result

module R = Eio.Buf_read

let test_data = String.init 100_000_000 (fun _ -> 'x')

let buf_read_bench () =
  let r = R.of_string test_data in
  let t0 = Unix.gettimeofday () in
  let i = ref 0 in
  try
    while true do
      assert (R.any_char r = 'x');
      incr i
    done
  with End_of_file ->
    let t1 = Unix.gettimeofday () in
    let m = Metric.create "read_bytes" (`Numeric (t1 -. t0)) "s" "Time taken to read 100_000_000 bytes" in
    let res = {name = "Bench_read"; metrics = [m]} in
    res
    (* let output =
      Printf.sprintf {| {"name": "Eio", "results": [%s]}|} (to_json res) in
    Printf.printf "%s\n" (Yojson.Basic.prettify output);
    Printf.printf "Read %d bytes in %.3fs\n" !i (t1 -. t0) *)

(* let () = buf_read_bench () *)