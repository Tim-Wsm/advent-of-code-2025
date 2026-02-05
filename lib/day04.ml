(* --- Day 4: Printing Department ---

You ride the escalator down to the printing department. They're clearly getting
ready for Christmas; they have lots of large rolls of paper everywhere, and
there's even a massive printer in the corner (to handle the really big print
jobs).

Decorating here will be easy: they can make their own decorations. What you
really need is a way to get further into the North Pole base while the
elevators are offline.

"Actually, maybe we can help with that," one of the Elves replies when you ask
for help. "We're pretty sure there's a cafeteria on the other side of the back
wall. If we could break through the wall, you'd be able to keep moving. It's
too bad all of our forklifts are so busy moving those big rolls of paper
around."

If you can optimize the work the forklifts are doing, maybe they would have
time to spare to break through the wall.

The rolls of paper (@) are arranged on a large grid; the Elves even have a
helpful diagram (your puzzle input) indicating where everything is located.

For example:

..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.

The forklifts can only access a roll of paper if there are fewer than four
rolls of paper in the eight adjacent positions. If you can figure out which
rolls of paper the forklifts can access, they'll spend less time looking and
more time breaking down the wall to the cafeteria.

In this example, there are 13 rolls of paper that can be accessed by a forklift
(marked with x):

..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x.

Consider your complete diagram of the paper roll locations. How many rolls of
paper can be accessed by a forklift?

--- Part Two ---

Now, the Elves just need help accessing as much of the paper as they can.

Once a roll of paper can be accessed by a forklift, it can be removed. Once a
roll of paper is removed, the forklifts might be able to access more rolls of
paper, which they might also be able to remove. How many total rolls of paper
could the Elves remove if they keep repeating this process?

Starting with the same example as above, here is one way you could remove as
many rolls of paper as possible, using highlighted @ to indicate that a roll of
paper is about to be removed, and using x to indicate that a roll of paper was
just removed:

Initial state:
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.

Remove 13 rolls of paper:
..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x.

Remove 12 rolls of paper:
.......x..
.@@.x.x.@x
x@@@@...@@
x.@@@@..x.
.@.@@@@.x.
.x@@@@@@.x
.x.@.@.@@@
..@@@.@@@@
.x@@@@@@@.
....@@@...

Remove 7 rolls of paper:
..........
.x@.....x.
.@@@@...xx
..@@@@....
.x.@@@@...
..@@@@@@..
...@.@.@@x
..@@@.@@@@
..x@@@@@@.
....@@@...

Remove 5 rolls of paper:
..........
..x.......
.x@@@.....
..@@@@....
...@@@@...
..x@@@@@..
...@.@.@@.
..x@@.@@@x
...@@@@@@.
....@@@...

Remove 2 rolls of paper:
..........
..........
..x@@.....
..@@@@....
...@@@@...
...@@@@@..
...@.@.@@.
...@@.@@@.
...@@@@@x.
....@@@...

Remove 1 roll of paper:
..........
..........
...@@.....
..x@@@....
...@@@@...
...@@@@@..
...@.@.@@.
...@@.@@@.
...@@@@@..
....@@@...

Remove 1 roll of paper:
..........
..........
...x@.....
...@@@....
...@@@@...
...@@@@@..
...@.@.@@.
...@@.@@@.
...@@@@@..
....@@@...

Remove 1 roll of paper:
..........
..........
....x.....
...@@@....
...@@@@...
...@@@@@..
...@.@.@@.
...@@.@@@.
...@@@@@..
....@@@...

Remove 1 roll of paper:
..........
..........
..........
...x@@....
...@@@@...
...@@@@@..
...@.@.@@.
...@@.@@@.
...@@@@@..
....@@@...

Stop once no more rolls of paper are accessible by a forklift. In this example,
a total of 43 rolls of paper can be removed.

Start with your original diagram. How many rolls of paper in total can be
removed by the Elves and their forklifts?

*)

open Solution
open CustomErrors
open Angstrom

module Day04 : Solution = struct
  type tile_t = Empty | PaperRoll
  type t = tile_t list list

  (*--------------------------------------------------------------------------*)
  (* Printing                                                                *)
  (*--------------------------------------------------------------------------*)
  let print_tile fmt = function
    | Empty -> Printf.fprintf fmt "."
    | PaperRoll -> Printf.fprintf fmt "@"

  let _print_diagram fmt =
    List.iter (fun row ->
        List.iter (print_tile fmt) row;
        Printf.fprintf fmt "\n")

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)
  let parse_diagram =
    sep_by1 (char '\n')
      (many1
         (choice
            [
              (char '@' >>= fun _ -> return PaperRoll);
              (char '.' >>= fun _ -> return Empty);
            ]))

  let parse_input str =
    match parse_string ~consume:Prefix parse_diagram str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Helpers                                                                  *)
  (*--------------------------------------------------------------------------*)
  let diagram_nth_opt diagram x y =
    if x < 0 || y < 0 then None
    else Option.bind (List.nth_opt diagram y) (fun row -> List.nth_opt row x)

  let accessible diagram x y =
    let neighours =
      [
        diagram_nth_opt diagram (x + 1) y;
        diagram_nth_opt diagram (x - 1) y;
        diagram_nth_opt diagram x (y + 1);
        diagram_nth_opt diagram x (y - 1);
        diagram_nth_opt diagram (x + 1) (y + 1);
        diagram_nth_opt diagram (x + 1) (y - 1);
        diagram_nth_opt diagram (x - 1) (y + 1);
        diagram_nth_opt diagram (x - 1) (y - 1);
      ]
      |> List.filter (function Some PaperRoll -> true | _ -> false)
    in
    match List.nth (List.nth diagram y) x with
    | Empty -> false
    | PaperRoll -> List.length neighours < 4

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  let sum_num_accessible diagram =
    List.mapi
      (fun y row ->
        List.mapi
          (fun x tile ->
            match tile with
            | Empty -> 0
            | PaperRoll -> if accessible diagram x y then 1 else 0)
          row)
      diagram
    |> List.fold_left
         (fun acc row -> acc + List.fold_left (fun x y -> x + y) 0 row)
         0

  let compute_simple diagram =
    Printf.printf "Task 1: %i\n" (sum_num_accessible diagram);
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let remove_accessible diagram =
    List.mapi
      (fun y row ->
        List.mapi
          (fun x tile ->
            match tile with
            | Empty -> Empty
            | PaperRoll -> if accessible diagram x y then Empty else PaperRoll)
          row)
      diagram

  let rec fixpoint_accessible diagram =
    let new_diagram = remove_accessible diagram in
    if diagram = new_diagram then 0
    else sum_num_accessible diagram + fixpoint_accessible new_diagram

  let compute_advanced diagram =
    Printf.printf "Task 2: %i \n" (fixpoint_accessible diagram);
    Ok ()
end
