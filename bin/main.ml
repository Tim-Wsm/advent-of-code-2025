open Cmdliner
open AdventOfCode2025.CustomErrors
open AdventOfCode2025.Solution
open AdventOfCode2025.Day01
open AdventOfCode2025.Day02
open AdventOfCode2025.Day03
open AdventOfCode2025.Day04
open AdventOfCode2025.Day05
open AdventOfCode2025.Day06
open AdventOfCode2025.Day07
open AdventOfCode2025.Day08
open AdventOfCode2025.Day09
open AdventOfCode2025.Day10
open AdventOfCode2025.Day11
open AdventOfCode2025.Day12

(*----------------------------------------------------------------------------*)
(* selecting and running the solution                                         *)
(*----------------------------------------------------------------------------*)

let run_solution day input_path =
  (*Custom syntax for working with results *)
  let ( let* ) x f = Result.bind x f in

  (* first find the matching implementation for the day *)
  let* (module Impl : Solution) =
    match day with
    | "day01" -> Ok (module Day01 : Solution)
    | "day02" -> Ok (module Day02 : Solution)
    | "day03" -> Ok (module Day03 : Solution)
    | "day04" -> Ok (module Day04 : Solution)
    | "day05" -> Ok (module Day05 : Solution)
    | "day06" -> Ok (module Day06 : Solution)
    | "day07" -> Ok (module Day07 : Solution)
    | "day08" -> Ok (module Day08 : Solution)
    | "day09" -> Ok (module Day09 : Solution)
    | "day10" -> Ok (module Day10 : Solution)
    | "day11" -> Ok (module Day11 : Solution)
    | "day12" -> Ok (module Day12 : Solution)
    | _ ->
        Printf.eprintf "\"%s\" is not a valid day" day;
        Error CustomErrors.day_does_not_exist
  in
  (* then open the input file and parse the content *)
  let* input =
    if not (Sys.file_exists input_path) then
      Error CustomErrors.day_does_not_exist
    else
      Impl.parse_input
        (In_channel.with_open_text input_path In_channel.input_all)
  in
  (* and finally run both the simple and complex solution. *)
  let* _ = Impl.compute_simple input in
  Impl.compute_advanced input

(*----------------------------------------------------------------------------*)
(* argument parsing                                                           *)
(*----------------------------------------------------------------------------*)
open Cmdliner.Term.Syntax

let parameter_day =
  let doc = "Select the day. Valid inputs are \"day01\" to \"day12\". " in
  Arg.(value & pos 0 string "" & info [] ~doc ~docv:"DAY")

let parameter_input_path =
  let doc = "Path to the challenge input." in
  Arg.(value & pos 1 string "" & info [] ~doc ~docv:"PATH")

let advent_of_code_cmd =
  let doc = "Run my solutions for the advent of code 2025." in
  Cmd.make
    (Cmd.info "AdventOfCode2025" ~version:"0.0.1" ~doc ~exits:CustomErrors.exits)
  @@
  let+ parameter_day = parameter_day
  and+ parameter_input_path = parameter_input_path in
  match run_solution parameter_day parameter_input_path with
  | Ok () -> Cmd.Exit.ok
  | Error code -> code

(*----------------------------------------------------------------------------*)
(* main function                                                              *)
(*----------------------------------------------------------------------------*)
let main () = Cmd.eval' advent_of_code_cmd
let () = if !Sys.interactive then () else exit (main ())
