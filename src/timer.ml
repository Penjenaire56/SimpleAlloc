module type S = sig
  val start : unit -> unit
  val pause : unit -> unit
  val get : unit -> float
end

module Make (Config : sig
  val profiling : bool
end) =
struct
  open Unix

  let u = ref 0.0
  let cpt = ref 0.0

  let start =
    if not Config.profiling then fun () -> ()
    else fun () -> u := (times ()).tms_utime

  let pause =
    if not Config.profiling then fun () -> ()
    else fun () -> cpt := !cpt +. ((times ()).tms_utime -. !u)

  let get () = !cpt
end

let print_time ppf sec =
  let minu = floor (sec /. 60.) in
  let sec = sec -. (minu *. 60.) in
  Format.fprintf ppf "%dm%2.3fs" (int_of_float minu) sec
