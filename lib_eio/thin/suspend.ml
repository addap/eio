type 'a enqueue = ('a, exn) result -> unit
type _ Effect.t += Suspend : (Cancel.t -> 'a enqueue -> unit) -> 'a Effect.t

let enter_unchecked fn = Effect.perform (Suspend fn)

let enter fn =
  enter_unchecked @@ fun fiber enqueue ->
  match Cancel.get_error fiber with
  | None -> fn fiber enqueue
  | Some ex -> enqueue (Error ex)