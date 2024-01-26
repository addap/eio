include Eio__core

module Debug = Private.Debug
let traceln = Debug.traceln

module Std = Std

module ThinStd = struct
  module Promise = Eio__thin.Promise
  module Fiber = Eio__thin.Fiber
  let traceln = Eio__thin.Private.Debug.traceln
end

module Thin = struct 
  include Eio__thin

  open ThinStd

  (* Run below examples with Eio_mock.Thin_backend.run *)
  (* Test forking two fibers and ignoring them completely. *)
  let main0 () = 
    let (_c1, _p1) = Fiber.fork_promise
        (fun () -> for x = 1 to 3 do traceln "x = %d" x; Fiber.yield () done) in
    let (_c2, _p2) = Fiber.fork_promise
        (fun () -> for y = 1 to 3 do traceln "y = %d" y; Fiber.yield () done) in
    traceln "after both are forked"

  (* Test forking two fibers and awaiting them both. *)
  let main1 () = 
    let (_c1, p1) = Fiber.fork_promise
        (fun () -> for x = 1 to 3 do traceln "x = %d" x; Fiber.yield () done) in
    let (_c2, p2) = Fiber.fork_promise
        (fun () -> for y = 1 to 3 do traceln "y = %d" y; Fiber.yield () done) in
    Promise.await_exn p1;
    Promise.await_exn p2;
    traceln "after both are forked"

  (* Test forking two fibers, awaiting one and cancelling the other. *)
  let main2 () = 
    let (_c1, p1) = Fiber.fork_promise
        (fun () -> for x = 1 to 3 do traceln "x = %d" x; Fiber.yield () done) in
    let (c2, _p2) = Fiber.fork_promise
        (fun () -> for y = 1 to 100 do traceln "y = %d" y; Fiber.yield () done) in
    Promise.await_exn p1;
    traceln "awaited fiber 1";
    Eio__thin.Cancel.cancel c2 (Failure "too long");
    traceln "after both are forked"

  (* Test forking two fibers, awaiting both but one cancels the other. *)
  let main3 () = 
    let (c1, p1) = Fiber.fork_promise
        (fun () -> for x = 1 to 100 do traceln "x = %d" x; Fiber.yield () done) in
    let (_c2, p2) = Fiber.fork_promise
        (fun () -> for y = 1 to 3 do traceln "y = %d" y; Fiber.yield () done; Eio__thin.Cancel.cancel c1 (Failure "too long")) in
    Promise.await_exn p2;
    traceln "awaited fiber 2";
    Promise.await_exn p1;
    traceln "awaited fiber 1";
end

module Semaphore = Semaphore
module Mutex = Eio_mutex
module Condition = Condition
module Stream = Stream
module Lazy = Lazy
module Pool = Pool
module Executor_pool = Executor_pool
module Exn = Exn
module Resource = Resource
module Buf_read = Buf_read
module Flow = struct
  include Flow

  let read_all flow =
    Buf_read.(parse_exn take_all) flow ~max_size:max_int
end
module Buf_write = Buf_write
module Net = Net
module Process = Process
module Domain_manager = Domain_manager
module Time = Time
module File = File
module Fs = Fs
module Path = Path

module Stdenv = struct
  let stdin  (t : <stdin  : _ Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : _ Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : _ Flow.sink;   ..>) = t#stderr
  let net (t : <net : _ Net.t; ..>) = t#net
  let process_mgr (t : <process_mgr : _ Process.mgr; ..>) = t#process_mgr
  let domain_mgr (t : <domain_mgr : _ Domain_manager.t; ..>) = t#domain_mgr
  let clock (t : <clock : _ Time.clock; ..>) = t#clock
  let mono_clock (t : <mono_clock : _ Time.Mono.t; ..>) = t#mono_clock
  let secure_random (t: <secure_random : _ Flow.source; ..>) = t#secure_random
  let fs (t : <fs : _ Path.t; ..>) = t#fs
  let cwd (t : <cwd : _ Path.t; ..>) = t#cwd
  let debug (t : <debug : 'a; ..>) = t#debug
  let backend_id (t: <backend_id : string; ..>) = t#backend_id
end

exception Io = Exn.Io
