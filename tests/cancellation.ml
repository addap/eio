open Eio.Std

let main () =
  Switch.run @@ fun sw ->
    Fiber.all [
      (fun () ->
        for _ = 0 to 5 do
          traceln "fiber 1"; Fiber.yield ()
        done);
      (fun () ->
        for _ = 0 to 5 do
          traceln "fiber 2"; Fiber.yield ()
        done);
      (fun () ->
        Fiber.yield (); 
        Switch.fail sw (Failure "too long");
        traceln "others cancelled")]

let _ = Eio_main.run @@ fun _ -> main ()
