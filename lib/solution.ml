open Cmdliner

module type Solution = sig
  type t

  val parse_input : string -> (t, Cmd.Exit.code) result
  val compute_simple : t -> (unit, Cmd.Exit.code) result
  val compute_advanced : t -> (unit, Cmd.Exit.code) result
end
