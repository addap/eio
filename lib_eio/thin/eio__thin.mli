(** a.d. This is a thinned out version of the eio core module.
    * Switch
        We removed the whole cancellation context/switch hierarchy. 
        Instead, we create a new cancellation context for each fiber.
        The cancellation context & fiber context are fused into just a cancellation context.
    * Fiber
        We just support the basic fork_promise operation so that fibers can interact with each other.
        fork_promise returns the cancellation context along with the promise. 
        So if you spawn a fiber you can either await or cancel it.
        Helper functions like both/all should still be possible.
        first/any would probably need an equivalent first/any function on promises.
    * Promise/Broadcast/Cells
        These modules were not changed. We assume the cells module is verified anyways.
    * Suspend
        The suspend module is already minimal so we did not change it.

    The signatures below are mostly a selection copied from the actual core module. So the comments did not change.
    In lib_eio/eio.ml we added some examples to show what you can still do with the restricted fibers.
    There is also a mock backend to run thin fibers in lib_eio/mock/thin_backend.ml.

    Problems:
    1. Protected cancellation contexts are never created right now but iirc for some cleanup tasks they were needed.
        Since we don't have the cancellation context hierarchy I am not sure how to implement protected sections of code.
  *)

module Promise : sig
  type 'a t
  (** An ['a t] is a promise for a value of type ['a]. *)

  type 'a u
  (** An ['a u] is a resolver for a promise of type ['a]. *)

  val create : ?label:string -> unit -> 'a t * 'a u
  (** [create ()] is a fresh promise/resolver pair.
      The promise is initially unresolved. *)

  val create_resolved : 'a -> 'a t
  (** [create_resolved x] is a promise that is already resolved with result [x]. *)

  val await : 'a t -> 'a
  (** [await t] blocks until [t] is resolved.
      If [t] is already resolved then this returns immediately. *)

  val resolve : 'a u -> 'a -> unit
  (** [resolve u v] resolves [u]'s promise with the value [v].
      Any threads waiting for the result will be added to the run queue.
      @raise Invalid_argument if [u] is already resolved. *)

  val peek : 'a t -> 'a option
  (** [peek t] is [Some v] if the promise has been resolved to [v], or [None] otherwise.
      If the result is [None] then it may change in future, otherwise it won't.
      If another domain has access to the resolver then the state may have already
      changed by the time this call returns. *)

  val is_resolved : 'a t -> bool
  (** [is_resolved t] is [Option.is_some (peek t)]. *)

  (** {1 Result promises} *)

  type 'a or_exn = ('a, exn) result t

  val resolve_ok : ('a, 'b) result u -> 'a -> unit
  (** [resolve_ok u x] is [resolve u (Ok x)]. *)

  val resolve_error : ('a, 'b) result u -> 'b -> unit
  (** [resolve_error u x] is [resolve u (Error x)]. *)

  val await_exn : 'a or_exn -> 'a
  (** [await_exn t] is like [await t], but if the result is [Error ex] then it raises [ex]. *)
end

module Cancel : sig
  type t
  (** A cancellation context. *)

  exception Cancelled of exn
  (** [Cancelled ex] indicates that the context was cancelled with exception [ex].
      It is usually not necessary to report a [Cancelled] exception to the user,
      as the original problem will be handled elsewhere.

      The nested exception is only intended for debug-level logging and should generally be ignored. *)

  exception Cancel_hook_failed of exn list
  (** Raised by {!cancel} if any of the cancellation hooks themselves fail. *)

  val check : t -> unit
  (** [check t] checks that [t] hasn't been cancelled.
      @raise Cancelled If the context has been cancelled. *)

  val get_error : t -> exn option
  (** [get_error t] is like [check t] except that it returns the exception instead of raising it.

      If [t] is finished, this returns (rather than raising) the [Invalid_argument] exception too. *)

  val cancel : t -> exn -> unit
  (** [cancel t ex] marks [t] and its child contexts as cancelled, recursively,
      and calls all registered fibers' cancellation functions, passing [Cancelled ex] as the argument.

      All cancellation functions are run, even if some of them raise exceptions.

      If [t] is already cancelled then this does nothing.

      Note that the caller of this function is still responsible for handling the error somehow
      (e.g. reporting it to the user); it does not become the responsibility of the cancelled thread(s).

      @raise Cancel_hook_failed if one or more hooks fail. *)

  val make_root : unit -> t
  (** Make a new root context for a new domain. *)

  val tid : t -> Ctf.id

  val set_cancel_fn : t -> (exn -> unit) -> unit
  (** [set_cancel_fn t fn] sets [fn] as the fiber's cancel function.

      If [t]'s cancellation context is cancelled, the function is called.
      It should attempt to make the current operation finish quickly, either with
      a successful result or by raising the given exception.

      Just before being called, the fiber's cancel function is replaced with [ignore]
      so that [fn] cannot be called twice.

      On success, the cancel function is cleared automatically when {!Suspend.enter} returns,
      but for single-domain operations you may like to call {!clear_cancel_fn}
      manually to remove it earlier.

      [fn] will be called from [t]'s domain (from the fiber that called [cancel]).

      [fn] must not switch fibers. If it did, this could happen:

      + Another suspended fiber in the same cancellation context resumes before
        its cancel function is called.
      + It enters a protected block and starts a new operation.
      + [fn] returns.
      + We cancel the protected operation. *)

  val clear_cancel_fn : t -> unit
  (** [clear_cancel_fn t] is [set_cancel_fn t ignore].

      This must only be called from the fiber's own domain.

      For single-domain operations, it can be useful to call this manually as soon as
      the operation succeeds (i.e. when the fiber is added to the run-queue)
      to prevent the cancel function from being called.

      For operations where another domain may resume the fiber, your cancel function
      will need to cope with being called after the operation has succeeded. In that
      case you should not call [clear_cancel_fn]. The backend will do it automatically
      just before resuming your fiber. *)
end

module Fiber : sig
  val fork_promise : (unit -> 'a) -> Cancel.t * 'a Promise.or_exn

  val fork : (unit -> 'a) -> Cancel.t

  val check : unit -> unit

  val yield : unit -> unit
end

(** @canonical Eio.Exn *)
module Exn : sig
  type with_bt = exn * Printexc.raw_backtrace

  type err = ..
  (** Describes the particular error that occurred.

      They are typically nested (e.g. [Fs (Permission_denied (Unix_error ...))])
      so that you can match e.g. all IO errors, all file-system errors, all
      permission denied errors, etc.

      If you extend this, use {!register_pp} to add a printer for the new error. *)

  type context
  (** Extra information attached to an IO error.
      This provides contextual information about what caused the error. *)

  exception Io of err * context
  (** A general purpose IO exception.

      This is used for most errors interacting with the outside world,
      and is similar to {!Unix.Unix_error}, but more general.
      An unknown [Io] error should typically be reported to the user, but does
      not generally indicate a bug in the program. *)

  type err += Multiple_io of (err * context * Printexc.raw_backtrace) list
  (** Error code used when multiple IO errors occur.

      This is useful if you want to catch and report all IO errors. *)

  val create : err -> exn
  (** [create err] is an {!Io} exception with an empty context. *)

  val add_context : exn -> ('a, Format.formatter, unit, exn) format4 -> 'a
  (** [add_context ex msg] returns a new exception with [msg] added to [ex]'s context,
      if [ex] is an {!Io} exception.

      If [ex] is not an [Io] exception, this function just returns the original exception. *)

  val reraise_with_context : exn -> Printexc.raw_backtrace -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [reraise_with_context ex bt msg] raises [ex] extended with additional information [msg].

      [ex] should be an {!Io} exception (if not, is re-raised unmodified).

      Example:
      {[
         try connect addr
         with Eio.Io _ as ex ->
           let bt = Printexc.get_raw_backtrace () in
           reraise_with_context ex bt "connecting to %S" addr
      ]}

      You must get the backtrace before calling any other function
      in the exception handler to prevent corruption of the backtrace. *)

  val register_pp : (Format.formatter -> err -> bool) -> unit
  (** [register_pp pp] adds [pp] as a pretty-printer of errors.

      [pp f err] should format [err] using [f], if possible.
      It should return [true] on success, or [false] if it didn't
      recognise [err]. *)

  val pp : exn Fmt.t
  (** [pp] is a formatter for exceptions.

      This is similar to {!Fmt.exn}, but can do a better job on {!Io} exceptions
      because it can format them directly without having to convert to a string first. *)

  (** Extensible backend-specific exceptions. *)
  module Backend : sig
    type t = ..

    val show : bool ref
    (** Controls the behaviour of {!pp}. *)

    val register_pp : (Format.formatter -> t -> bool) -> unit
    (** [register_pp pp] adds [pp] as a pretty-printer of backend errors.

        [pp f err] should format [err] using [f], if possible.
        It should return [true] on success, or [false] if it didn't
        recognise [err]. *)

    val pp : t Fmt.t
    (** [pp] behaves like {!pp} except that if display of backend errors has been turned off
        (with {!show}) then it just prints a place-holder.

        This is useful for formatting the backend-specific part of exceptions,
        which should be hidden in expect-style testing that needs to work on multiple backends. *)
  end

  type err += X of Backend.t
  (** A top-level code for backend errors that don't yet have a cross-platform classification in Eio.

      You should avoid matching on these (in portable code). Instead, request a proper Eio code for them. *)

  exception Multiple of with_bt list
  (** Raised if multiple fibers fail, to report all the exceptions.

      This usually indicates a bug in the program.

      Note: If multiple {b IO} errors occur, then you will get [Io (Multiple_io _, _)] instead of this. *)

  val combine : with_bt -> with_bt -> with_bt
  (** [combine x y] returns a single exception and backtrace to use to represent two errors.

      The resulting exception is typically just [Multiple [y; x]],
      but various heuristics are used to simplify the result:
      - Combining with a {!Cancel.Cancelled} exception does nothing, as these don't need to be reported.
        The result is only [Cancelled] if there is no other exception available.
      - If both errors are [Io] errors, then the result is [Io (Multiple_io _)]. *)
end


module Private : sig
  module Ctf = Ctf

  module Cells = Cells
  module Broadcast = Broadcast

  module Effects : sig
    type 'a enqueue = ('a, exn) result -> unit
    (** A function provided by the scheduler to reschedule a previously-suspended thread. *)

    type _ Effect.t +=
      | Suspend : (Cancel.t -> 'a enqueue -> unit) -> 'a Effect.t
      (** [Suspend fn] is performed when a fiber must be suspended
          (e.g. because it called {!Promise.await} on an unresolved promise).
          The effect handler runs [fn fiber enqueue] in the scheduler context,
          passing it the suspended fiber's context and a function to resume it.
          [fn] should arrange for [enqueue] to be called once the thread is ready to run again. *)

      | Fork : Cancel.t * (unit -> unit) -> unit Effect.t
      (** [perform (Fork new_context f)] creates a new fiber and runs [f] in it, with context [new_context].
          [f] must not raise an exception. See {!Fiber.fork}. *)

      | Get_context : Cancel.t Effect.t
      (** [perform Get_context] immediately returns the current fiber's context (without switching fibers). *)
  end

  (** Suspend a fiber and enter the scheduler. *)
  module Suspend : sig
    val enter : (Cancel.t -> 'a Effects.enqueue -> unit) -> 'a
    (** [enter fn] suspends the calling fiber and calls [fn ctx enqueue] in the scheduler's context.
        This should arrange for [enqueue] to be called when the fiber should be resumed.
        [enqueue] is thread-safe and so can be called from another domain or systhread.

        [ctx] should be used to set a cancellation function. Otherwise, the operation is non-interruptable.
        If the caller's cancellation context is already cancelled, [enter] immediately aborts. *)

    val enter_unchecked : (Cancel.t -> 'a Effects.enqueue -> unit) -> 'a
    (** [enter_unchecked] is like [enter] except that it does not perform the initial check
        that the fiber isn't cancelled (this is useful if you want to do the check yourself, e.g.
        because you need to unlock a mutex if cancelled). *)
  end

  module Debug : sig
    val traceln :
      ?__POS__:string * int * int * int ->
      ('a, Format.formatter, unit, unit) format4 -> 'a
    (** Writes trace logging using the current fiber's configured traceln function. *)

    val traceln_mutex : Stdlib.Mutex.t
    (** The mutex used to prevent two domains writing to stderr at once.

        This might be useful if you want to write to it directly yourself,
        e.g. for a log reporter. *)

    val default_traceln :
      ?__POS__:string * int * int * int ->
      ('a, Format.formatter, unit, unit) format4 -> 'a
      (** [default_traceln] is a suitable default implementation for {!Eio.Std.traceln}.

          It writes output to stderr, prefixing each line with a "+".
          If [__POS__] is given, it also displays the file and line number from that.
          It uses {!traceln_mutex} so that only one domain's output is written at a time. *)

    type traceln = {
      traceln : 'a. ?__POS__:string * int * int * int -> ('a, Format.formatter, unit, unit) format4 -> 'a;
    } [@@unboxed]
  end
end
