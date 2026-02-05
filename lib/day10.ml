(* --- Day 10: Factory ---

Just across the hall, you find a large factory. Fortunately, the Elves here
have plenty of time to decorate. Unfortunately, it's because the factory
machines are all offline, and none of the Elves can figure out the
initialization procedure.

The Elves do have the manual for the machines, but the section detailing the
initialization procedure was eaten by a Shiba Inu. All that remains of the
manual are some indicator light diagrams, button wiring schematics, and joltage
requirements for each machine.

For example:

[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

The manual describes one machine per line. Each line contains a single
indicator light diagram in [square brackets], one or more button wiring
schematics in (parentheses), and joltage requirements in {curly braces}.

To start a machine, its indicator lights must match those shown in the diagram,
where . means off and # means on. The machine has the number of indicator
lights shown, but its indicator lights are all initially off.

So, an indicator light diagram like [.##.] means that the machine has four
indicator lights which are initially off and that the goal is to simultaneously
configure the first light to be off, the second light to be on, the third to be
on, and the fourth to be off.

You can toggle the state of indicator lights by pushing any of the listed
buttons. Each button lists which indicator lights it toggles, where 0 means the
first light, 1 means the second light, and so on. When you push a button, each
listed indicator light either turns on (if it was off) or turns off (if it was
on). You have to push each button an integer number of times; there's no such
thing as "0.5 presses" (nor can you push a button a negative number of times).

So, a button wiring schematic like (0,3,4) means that each time you push that
button, the first, fourth, and fifth indicator lights would all toggle between
on and off. If the indicator lights were [#.....], pushing the button would
change them to be [...##.] instead.

Because none of the machines are running, the joltage requirements are
irrelevant and can be safely ignored.

You can push each button as many times as you like. However, to save on time,
you will need to determine the fewest total presses required to correctly
configure all indicator lights for all machines in your list.

There are a few ways to correctly configure the first machine:

[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}

    You could press the first three buttons once each, a total of 3 button presses.
    You could press (1,3) once, (2,3) once, and (0,1) twice, a total of 4 button presses.
    You could press all of the buttons except (1,3) once each, a total of 5 button presses.

However, the fewest button presses required is 2. One way to do this is by
pressing the last two buttons ((0,2) and (0,1)) once each.

The second machine can be configured with as few as 3 button presses:

[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}

One way to achieve this is by pressing the last three buttons ((0,4), (0,1,2),
and (1,2,3,4)) once each.

The third machine has a total of six indicator lights that need to be
configured correctly:

[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

The fewest presses required to correctly configure it is 2; one way to do this
is by pressing buttons (0,3,4) and (0,1,2,4,5) once each.

So, the fewest button presses required to correctly configure the indicator
lights on all of the machines is 2 + 3 + 2 = 7.

Analyze each machine's indicator light diagram and button wiring schematics.
What is the fewest button presses required to correctly configure the indicator
lights on all of the machines?


--- Part Two ---

All of the machines are starting to come online! Now, it's time to worry about
the joltage requirements.

Each machine needs to be configured to exactly the specified joltage levels to
function properly. Below the buttons on each machine is a big lever that you
can use to switch the buttons from configuring the indicator lights to
increasing the joltage levels. (Ignore the indicator light diagrams.)

The machines each have a set of numeric counters tracking its joltage levels,
one counter per joltage requirement. The counters are all initially set to
zero.

So, joltage requirements like {3,5,4,7} mean that the machine has four counters
which are initially 0 and that the goal is to simultaneously configure the
first counter to be 3, the second counter to be 5, the third to be 4, and the
fourth to be 7.

The button wiring schematics are still relevant: in this new joltage
configuration mode, each button now indicates which counters it affects, where
0 means the first counter, 1 means the second counter, and so on. When you push
a button, each listed counter is increased by 1.

So, a button wiring schematic like (1,3) means that each time you push that
button, the second and fourth counters would each increase by 1. If the current
joltage levels were {0,1,2,3}, pushing the button would change them to be
{0,2,2,4}.

You can push each button as many times as you like. However, your finger is
getting sore from all the button pushing, and so you will need to determine the
fewest total presses required to correctly configure each machine's joltage
level counters to match the specified joltage requirements.

Consider again the example from before:

[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

Configuring the first machine's counters requires a minimum of 10 button
presses. One way to do this is by pressing (3) once, (1,3) three times, (2,3)
three times, (0,2) once, and (0,1) twice.

Configuring the second machine's counters requires a minimum of 12 button
presses. One way to do this is by pressing (0,2,3,4) twice, (2,3) five times,
and (0,1,2) five times.

Configuring the third machine's counters requires a minimum of 11 button
presses. One way to do this is by pressing (0,1,2,3,4) five times, (0,1,2,4,5)
five times, and (1,2) once.

So, the fewest button presses required to correctly configure the joltage level
counters on all of the machines is 10 + 12 + 11 = 33.

Analyze each machine's joltage requirements and button wiring schematics. What
is the fewest button presses required to correctly configure the joltage level
counters on all of the machines?

 *)

open Angstrom
open Solution
open CustomErrors

module Day10 : Solution = struct
  type machine_t = {
    lights : bool list;
    buttons : int list list;
    joltages : int list;
  }

  type t = machine_t list

  module LightsStateSet = Set.Make (struct
    type t = bool list

    let compare = Stdlib.compare
  end)

  module JoltageStateSet = Set.Make (struct
    type t = int list

    let compare = Stdlib.compare
  end)

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parse_integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse_lights =
    char '['
    *> many1 (char '.' >>| (fun _ -> false) <|> (char '#' >>| fun _ -> true))
    <* char ']'

  let parse_button = char '(' *> sep_by1 (char ',') parse_integer <* char ')'
  let parse_joltages = char '{' *> sep_by1 (char ',') parse_integer <* char '}'

  let parse_machine =
    let* lights = parse_lights in
    let* _ = char ' ' in
    let* buttons = sep_by1 (char ' ') parse_button in
    let* _ = char ' ' in
    let* joltages = parse_joltages in
    return { lights; buttons; joltages }

  let parse_machines = many1 (parse_machine <* end_of_line)

  let parse_input str =
    match parse_string ~consume:Prefix parse_machines str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Helper functions                                                         *)
  (*--------------------------------------------------------------------------*)

  let _print_machine machine =
    Printf.printf "[";
    List.iter
      (fun light -> if light then Printf.printf "#" else Printf.printf ".")
      machine.lights;
    Printf.printf "] ";

    List.iter
      (fun button ->
        Printf.printf "(%i" (List.hd button);
        List.iter (fun pos -> Printf.printf ",%i" pos) (List.tl button);
        Printf.printf ") ")
      machine.buttons;

    Printf.printf "{%i" (List.hd machine.joltages);
    List.iter
      (fun joltage -> Printf.printf ",%i" joltage)
      (List.tl machine.joltages);
    Printf.printf "}\n"

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  let apply_button_light button (lights, num_steps) =
    List.mapi
      (fun light_pos light ->
        if List.exists (fun button_pos -> light_pos = button_pos) button then
          not light
        else light)
      lights
    |> fun new_state -> (new_state, num_steps + 1)

  let distance_light machine lights =
    List.map2
      (fun light_solution light -> if light_solution = light then 0 else 1)
      machine.lights lights
    |> List.fold_left (fun x y -> x + y) 0

  (* I chose the scaling of "light distance" to steps to steps by trail and
     error until the small example worked and was reasonably fast. *)
  let distance_states_light machine (lights_1, steps_1) (lights_2, steps_2) =
    compare
      (distance_light machine lights_1 + (steps_1 * 5))
      (distance_light machine lights_2 + (steps_2 * 5))

  let rec find_quickest_run_lights_df machine current_states visited_states =
    (* sort the set of current states by their distance to the solution *)
    let sorted_states =
      List.sort (distance_states_light machine) current_states
    in
    (* apply each possible button to the state closest to the solution *)
    let next_states =
      List.map
        (fun button -> apply_button_light button (List.hd sorted_states))
        machine.buttons
      |> List.filter (fun (lights, _) ->
          not (LightsStateSet.mem lights visited_states))
    in
    (* if one of the resulting states is the solution we are otherwise continue
       with this new set of states. *)
    match
      List.find_opt (fun (lights, _) -> lights = machine.lights) next_states
    with
    | Some (_, num_steps) -> num_steps
    | None ->
        let new_visited_states =
          next_states
          |> List.map (fun (lights, _) -> lights)
          |> LightsStateSet.of_list
          |> LightsStateSet.union visited_states
        in
        find_quickest_run_lights_df machine
          (List.append next_states (List.tl sorted_states))
          new_visited_states

  let compute_simple machines =
    let result =
      machines
      |> List.map (fun machine ->
          find_quickest_run_lights_df machine
            [ (List.init (List.length machine.lights) (fun _ -> false), 0) ]
            LightsStateSet.empty)
      |> List.fold_left (fun x y -> x + y) 0
    in
    Printf.printf "Task 1: %i\n" result;
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let apply_button_joltage button joltages =
    List.mapi
      (fun joltage_pos joltage ->
        if List.exists (fun button_pos -> joltage_pos = button_pos) button then
          joltage + 1
        else joltage)
      joltages

  let valid_joltage machine joltages =
    List.map2
      (fun joltage_solution joltage -> joltage <= joltage_solution)
      machine.joltages joltages
    |> List.fold_left (fun x y -> x && y) true

  let distance_joltage machine joltages =
    List.map2
      (fun joltage_solution joltage -> joltage_solution - joltage)
      machine.joltages joltages
    |> List.fold_left (fun x y -> x + y) 0

  (* I chose the scaling of "joltage distance" to steps to steps by trail and
     error until the small example worked and was reasonably fast. *)
  let distance_states_joltage machine (joltages_1, steps_1, _)
      (joltages_2, steps_2, _) =
    compare
      (distance_joltage machine joltages_1 + steps_1)
      (distance_joltage machine joltages_2 + steps_2)

  (*
  let rec find_quickest_run_joltages_df machine current_states visited_states =
    (* sort the set of current states by their distance to the solution *)
    let sorted_states =
      List.sort (distance_states_joltage machine) current_states
    in
    (* apply each possible button to the state closest to the solution *)
    let next_states =
      List.map
        (fun button -> apply_button_joltage button (List.hd sorted_states))
        machine.buttons
      |> List.filter (fun (joltages, _) ->
          (not (JoltageStateSet.mem joltages visited_states))
          && valid_joltage machine joltages)
    in
    (* if one of the resulting states is the solution we are otherwise continue
       with this new set of states. *)
    match
      List.find_opt
        (fun (joltages, _) -> joltages = machine.joltages)
        next_states
    with
    | Some (_, num_steps) -> num_steps
    | None ->
        let new_visited_states =
          next_states
          |> List.map (fun (joltages, _) -> joltages)
          |> JoltageStateSet.of_list
          |> JoltageStateSet.union visited_states
        in
        find_quickest_run_joltages_df machine
          (List.append next_states (List.tl sorted_states))
          new_visited_states
          *)

  let rec find_quickest_run_joltages_df machine current_states visited_states =
    (* sort the set of current states by their distance to the solution *)
    let sorted_states =
      List.sort (distance_states_joltage machine) current_states
    in
    (* apply each possible button to the state closest to the solution *)
    let hd_state, hd_steps, hd_buttons = List.hd sorted_states in
    (*
    Printf.printf "steps: %i buttons:%i distance: %i\n" hd_steps
      (List.length hd_buttons)
      (distance_joltage machine hd_state);
      *)
    let next_joltages, next_buttons =
      List.map
        (fun button -> (apply_button_joltage button hd_state, button))
        hd_buttons
      |> List.filter (fun (joltages, _) -> valid_joltage machine joltages)
      |> List.split
    in
    let next_states =
      next_joltages
      |> List.filter (fun joltages ->
          not (JoltageStateSet.mem joltages visited_states))
      |> List.map (fun joltages -> (joltages, hd_steps + 1, next_buttons))
      |> List.filter (fun (_, _, buttons) -> not (List.is_empty buttons))
    in
    (* if one of the resulting states is the solution we are otherwise continue
       with this new set of states. *)
    match
      List.find_opt
        (fun (joltages, _, _) -> joltages = machine.joltages)
        next_states
    with
    | Some (_, num_steps, _) -> num_steps
    | None ->
        let new_visited_states =
          next_states
          |> List.map (fun (joltages, _, _) -> joltages)
          |> JoltageStateSet.of_list
          |> JoltageStateSet.union visited_states
        in
        find_quickest_run_joltages_df machine
          (List.append next_states (List.tl sorted_states))
          new_visited_states

  let compute_advanced machines =
    let result =
      machines |> List.take 2
      |> List.map (fun machine ->
          let init_joltages =
            List.init (List.length machine.joltages) (fun _ -> 0)
          in
          find_quickest_run_joltages_df machine
            [ (init_joltages, 0, machine.buttons) ]
            (JoltageStateSet.singleton init_joltages))
      |> List.fold_left (fun x y -> x + y) 0
    in
    Printf.printf "Task 2: %i\n" result;
    Ok ()
end
