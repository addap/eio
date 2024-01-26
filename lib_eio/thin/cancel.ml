exception Cancelled = Exn.Cancelled
exception Cancel_hook_failed = Exn.Cancel_hook_failed

type state =
  | On
  | Cancelling of exn * Printexc.raw_backtrace
  | Finished

type t = {
  tid : Ctf.id;
  mutable state : state;
  protected : bool;
  domain : Domain.id;         (* Prevent access from other domains *)
  mutable cancel_fn : exn -> unit;
}

type _ Effect.t += Get_context : t Effect.t

let is_on t =
  match t.state with
  | On -> true
  | Cancelling _ | Finished -> false

let check t =
  match t.state with
  | On -> ()
  | Cancelling (ex, _) -> raise (Cancelled ex)
  | Finished -> invalid_arg "Cancellation context finished!"

let get_error t =
  match t.state with
  | On -> None
  | Cancelling (ex, _) -> Some (Cancelled ex)
  | Finished -> Some (Invalid_argument "Cancellation context finished!")

let is_finished t =
  match t.state with
  | Finished -> true
  | On | Cancelling _ -> false

let tid t = t.tid

let set_cancel_fn t fn =
  t.cancel_fn <- fn

let clear_cancel_fn t =
  t.cancel_fn <- ignore

let make ~protected =
  let tid = Ctf.mint_id () in
  Ctf.note_created tid Ctf.Task;
  let t = { tid; state = Finished; protected; domain = Domain.self (); cancel_fn = ignore } in
  t.state <- On;
  t

let make_root () =
  let t = make ~protected:false in
  t.state <- On;
  t

(* Mark the cancellation tree rooted at [t] as Cancelling (stopping at protected sub-contexts),
   and return a list of all fibers in the newly-cancelling contexts. Since modifying the cancellation
   tree can only be done from our domain, this is effectively an atomic operation. Once it returns,
   new (non-protected) fibers cannot be added to any of the cancelling contexts. *)
let cancel_internal t ex =
  match t.state with
  | Finished -> invalid_arg "Cancellation context finished!"
  | Cancelling _ -> ()
  | On ->
    let bt = Printexc.get_raw_backtrace () in
    t.state <- Cancelling (ex, bt);
    ()

let check_our_domain t =
  if Domain.self () <> t.domain then invalid_arg "Cancellation context accessed from wrong domain!"

(* a.d. TODO this seems to be one of the most important functions for the cancelling behavior.
   It also uses the cancel_fn in a fiber_context. *)
let cancel t ex =
  check_our_domain t;
  cancel_internal t ex;
  let cex = Cancelled ex in
  let fn = t.cancel_fn in
  t.cancel_fn <- ignore;
  match fn cex with
  | () -> ()
  | exception ex2 -> raise (Cancel_hook_failed [ex2])