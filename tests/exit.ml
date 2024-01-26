open Eio.Std

(* I was wondering what happens if you're the only fiber in one thread and await a promise from another thread. 
   My idea was that the eun queue would run empty and when the callback is called to after the promise is finished, some error happens. 
  The domain manager has an internal active_ops counter and only exits if the run queue is empty and the counter is 0. I suspect that 
    the counter is not 0.
  
  TODO add some print statements to see what happens. *)

let main ~domain_mgr ~clock =
  Switch.run @@ fun sw ->
  let longp = Fiber.fork_promise ~sw (fun () ->
    for i = 0 to 5 do
      Eio.Time.sleep clock 1.0;
      traceln "Not yet";
      Fiber.yield ()
    done) in
  Eio.Domain_manager.run domain_mgr (fun () ->
  Promise.await longp;
  traceln "Finished waiting")

Eio_posix.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let clock = Eio.Stdenv.clock env in
  main domain_mgr clock


(* I was wondering wether Domain_manager.run is blocking. Turns out it is. The Fiber.both (or just the implicit switch if you don't use Fiber.both) 
   waits until the other domain has been joined. *)
let main ~domain_mgr ~clock =
  Fiber.both (fun () ->
  Eio.Domain_manager.run domain_mgr (fun () ->
    for i = 0 to 5 do
      Eio.Time.sleep clock 1.0;
      traceln "Not yet";
      Fiber.yield ()
    done))
    (fun () -> 
    for i = 0 to 5 do
      Eio.Time.sleep clock 0.3;
      traceln "Fiber";
      Fiber.yield ()
    done);
  traceln "Exit main"

Eio_posix.run (fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let clock = Eio.Stdenv.clock env in
  main domain_mgr clock); traceln "After main"
