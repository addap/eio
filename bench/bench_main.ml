
let bench_buf = Bench_buf_read.buf_read_bench
let bench_cancel = Bench_cancel.bench_cancel

let bench_fd = Bench_fd.bench_fd

let bench_condition = Bench_condition.bench_condition

let bench_yield = Bench_yield.bench_yield

let bench_mutex = Bench_mutex.bench_mutex

let bench_semaphore = Bench_semaphore.bench_semaphore

let bench_stream = Bench_stream.bench_stream

let bench_promise = Bench_promise.bench_promise

let bench_http = Bench_http.bench_http
let bench_list =
  [bench_cancel; bench_fd; bench_condition; bench_buf;
   bench_yield; bench_mutex; bench_semaphore; bench_stream;
   bench_promise; bench_http]

let () =
  let results =
    List.map (fun f -> f ()) bench_list |>
    List.map Benchmark_result.to_json |>
    String.concat ", "
    in
  let output = Printf.sprintf {| {"name": "Eio", "results": [%s]}|} (results) in
  Printf.printf "%s" (Yojson.Basic.prettify output)
