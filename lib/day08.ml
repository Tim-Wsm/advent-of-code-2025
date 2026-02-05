(*  --- Day 8: Playground ---

Equipped with a new understanding of teleporter maintenance, you confidently
step onto the repaired teleporter pad.

You rematerialize on an unfamiliar teleporter pad and find yourself in a vast
underground space which contains a giant playground!

Across the playground, a group of Elves are working on setting up an ambitious
Christmas decoration project. Through careful rigging, they have suspended a
large number of small electrical junction boxes.

Their plan is to connect the junction boxes with long strings of lights. Most
of the junction boxes don't provide electricity; however, when two junction
boxes are connected by a string of lights, electricity can pass between those
two junction boxes.

The Elves are trying to figure out which junction boxes to connect so that
electricity can reach every junction box. They even have a list of all of the
junction boxes' positions in 3D space (your puzzle input).

For example:

162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689

This list describes the position of 20 junction boxes, one per line. Each
position is given as X,Y,Z coordinates. So, the first junction box in the list
is at X=162, Y=817, Z=812.

To save on string lights, the Elves would like to focus on connecting pairs of
junction boxes that are as close together as possible according to
straight-line distance. In this example, the two junction boxes which are
closest together are 162,817,812 and 425,690,689.

By connecting these two junction boxes together, because electricity can flow
between them, they become part of the same circuit. After connecting them,
there is a single circuit which contains two junction boxes, and the remaining
18 junction boxes remain in their own individual circuits.

Now, the two junction boxes which are closest together but aren't already
directly connected are 162,817,812 and 431,825,988. After connecting them,
since 162,817,812 is already connected to another junction box, there is now a
single circuit which contains three junction boxes and an additional 17
circuits which contain one junction box each.

The next two junction boxes to connect are 906,360,560 and 805,96,715. After
connecting them, there is a circuit containing 3 junction boxes, a circuit
containing 2 junction boxes, and 15 circuits which contain one junction box
each.

The next two junction boxes are 431,825,988 and 425,690,689. Because these two
junction boxes were already in the same circuit, nothing happens!

This process continues for a while, and the Elves are concerned that they don't
have enough extension cables for all these circuits. They would like to know
how big the circuits will be.

After making the ten shortest connections, there are 11 circuits: one circuit
which contains 5 junction boxes, one circuit which contains 4 junction boxes,
two circuits which contain 2 junction boxes each, and seven circuits which each
contain a single junction box. Multiplying together the sizes of the three
largest circuits (5, 4, and one of the circuits of size 2) produces 40.

Your list contains many junction boxes; connect together the 1000 pairs of
junction boxes which are closest together. Afterward, what do you get if you
multiply together the sizes of the three largest circuits?

--- Part Two ---

The Elves were right; they definitely don't have enough extension cables.
You'll need to keep connecting junction boxes together until they're all in one
large circuit.

Continuing the above example, the first connection which causes all of the
junction boxes to form a single circuit is between the junction boxes at
216,146,977 and 117,168,530. The Elves need to know how far those junction
boxes are from the wall so they can pick the right extension cable; multiplying
the X coordinates of those two junction boxes (216 and 117) produces 25272.

Continue connecting the closest unconnected pairs of junction boxes together
until they're all in the same circuit. What do you get if you multiply together
the X coordinates of the last two junction boxes you need to connect?

*)

open Angstrom
open Solution
open CustomErrors

module Day08 : Solution = struct
  type position_t = { x : int; y : int; z : int }
  type t = position_t list

  module Circuit = Set.Make (struct
    type t = position_t

    let compare = Stdlib.compare
  end)

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parse_integer_as =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse_position =
    let* x = parse_integer_as in
    let* _ = char ',' in
    let* y = parse_integer_as in
    let* _ = char ',' in
    let* z = parse_integer_as in
    return { x; y; z }

  let parse_positions = many1 (parse_position <* end_of_line)

  let parse_input str =
    match parse_string ~consume:Prefix parse_positions str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Helper functions                                                         *)
  (*--------------------------------------------------------------------------*)

  let distance_of pos_1 pos_2 =
    Float.(
      sqrt
        (pow (of_int (pos_1.x - pos_2.x)) 2.0
        +. pow (of_int (pos_1.y - pos_2.y)) 2.0
        +. pow (of_int (pos_1.z - pos_2.z)) 2.0))

  let rec compute_distances positions distances =
    match positions with
    | [] | [ _ ] -> distances
    | pos_1 :: rest ->
        let new_distances =
          List.map (fun pos_2 -> (distance_of pos_1 pos_2, pos_1, pos_2)) rest
        in
        compute_distances rest (List.append distances new_distances)

  let connect_circuits circuits pos_1 pos_2 =
    let new_circuit =
      Circuit.union
        (List.find (fun circuit -> Circuit.mem pos_1 circuit) circuits)
        (List.find (fun circuit -> Circuit.mem pos_2 circuit) circuits)
    in
    let filtered_circuits =
      List.filter
        (fun circuit ->
          (not (Circuit.mem pos_1 circuit)) && not (Circuit.mem pos_2 circuit))
        circuits
    in
    new_circuit :: filtered_circuits

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  let compute_simple positions =
    let result =
      compute_distances positions []
      |> List.sort (fun (distance_1, _, _) (distance_2, _, _) ->
          compare distance_1 distance_2)
      |> List.take (if List.length positions = 20 then 10 else 1000)
      |> List.fold_left
           (fun circuits (_, pos_1, pos_2) ->
             connect_circuits circuits pos_1 pos_2)
           (List.map (fun pos -> Circuit.singleton pos) positions)
      |> List.sort (fun circuit_1 circuit_2 ->
          compare (Circuit.cardinal circuit_1) (Circuit.cardinal circuit_2))
      |> List.rev |> List.take 3
      |> List.fold_left (fun acc circuit -> acc * Circuit.cardinal circuit) 1
    in

    Printf.printf "Task 1: %i\n" result;
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let rec find_fixpoint circuits sorted_distances =
    match sorted_distances with
    | [] -> assert false
    | (_, pos_1, pos_2) :: rem_distances ->
        let merged_circuits = connect_circuits circuits pos_1 pos_2 in
        if List.length merged_circuits = 1 then pos_1.x * pos_2.x
        else find_fixpoint merged_circuits rem_distances

  let compute_advanced positions =
    let circuits = List.map (fun pos -> Circuit.singleton pos) positions in
    let sorted_distances =
      compute_distances positions []
      |> List.sort (fun (distance_1, _, _) (distance_2, _, _) ->
          compare distance_1 distance_2)
    in

    let result = find_fixpoint circuits sorted_distances in

    Printf.printf "Task 2: %i\n" result;
    Ok ()
end
