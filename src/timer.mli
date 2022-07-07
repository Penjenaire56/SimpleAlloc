(** The interface of timers *)
module type S = sig
  val start : unit -> unit
  (** Start (or restart) recording {e user time} when this function is
      called. *)

  val pause : unit -> unit
  (** Pause the time, i.e. accumulates time elapsed since last (re-)start *)

  val get : unit -> float
  (** Returns the time in seconds accumulated in the timer. *)
end

(** Functor to create a new timer. If [profiling] is false in the parameter
    module, the timer will be inactive. *)
module Make (Config : sig
  val profiling : bool
end) : S

val print_time : Format.formatter -> float -> unit
