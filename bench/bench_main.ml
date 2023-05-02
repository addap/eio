
(* let bench_buf = Bench_buf_read.buf_read_bench *)
let bench_cancel = Bench_cancel.bench_cancel

let bench_fd = Bench_fd.bench_fd

let bench_condition = Bench_condition.bench_condition

let bench_list = [bench_cancel; bench_fd; bench_condition ]

let () =
  let results =
    List.map (fun f -> f ()) bench_list |>
    List.map Benchmark_result.to_json |>
    String.concat ", "
    in
  let output = Printf.sprintf {| {"name": "Eio", "results": [%s]}|} (results) in
  Printf.printf "%s" (Yojson.Basic.prettify output)
