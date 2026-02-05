(*--- Day 5: Cafeteria ---

As the forklifts break through the wall, the Elves are delighted to discover
that there was a cafeteria on the other side after all.

You can hear a commotion coming from the kitchen. "At this rate, we won't have
any time left to put the wreaths up in the dining hall!" Resolute in your
quest, you investigate.

"If only we hadn't switched to the new inventory management system right before
Christmas!" another Elf exclaims. You ask what's going on.

The Elves in the kitchen explain the situation: because of their complicated
new inventory management system, they can't figure out which of their
ingredients are fresh and which are spoiled. When you ask how it works, they
give you a copy of their database (your puzzle input).

The database operates on ingredient IDs. It consists of a list of fresh
ingredient ID ranges, a blank line, and a list of available ingredient IDs. For
example:

    3-5
    10-14
    16-20
    12-18

    1
    5
    8
    11
    17
    32

The fresh ID ranges are inclusive: the range 3-5 means that ingredient IDs 3,
4, and 5 are all fresh. The ranges can also overlap; an ingredient ID is fresh
if it is in any range.

The Elves are trying to determine which of the available ingredient IDs are
fresh. In this example, this is done as follows:

    - Ingredient ID 1 is spoiled because it does not fall into any range.
    - Ingredient ID 5 is fresh because it falls into range 3-5.
    - Ingredient ID 8 is spoiled.
    - Ingredient ID 11 is fresh because it falls into range 10-14.
    - Ingredient ID 17 is fresh because it falls into range 16-20 as well as range 12-18.
    - Ingredient ID 32 is spoiled.

So, in this example, 3 of the available ingredient IDs are fresh.

Process the database file from the new inventory management system. How many of
the available ingredient IDs are fresh?

--- Part Two ---

The Elves start bringing their spoiled inventory to the trash chute at the back
of the kitchen.

So that they can stop bugging you when they get new inventory, the Elves would
like to know all of the IDs that the fresh ingredient ID ranges consider to be
fresh. An ingredient ID is still considered fresh if it is in any range.

Now, the second section of the database (the available ingredient IDs) is
irrelevant. Here are the fresh ingredient ID ranges from the above example:

3-5
10-14
16-20
12-18

The ingredient IDs that these ranges consider to be fresh are 3, 4, 5, 10, 11,
12, 13, 14, 15, 16, 17, 18, 19, and 20. So, in this example, the fresh
ingredient ID ranges consider a total of 14 ingredient IDs to be fresh.

Process the database file again. How many ingredient IDs are considered to be
fresh according to the fresh ingredient ID ranges?
*)

open Angstrom
open Solution
open CustomErrors

module Day05 : Solution = struct
  type range_t = { left : int; right : int }
  type t = range_t list * int list

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parse_integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse_range =
    let* left = parse_integer in
    let* _ = char '-' in
    let* right = parse_integer in
    let* _ = char '\n' in
    return { left; right }

  let parse_input =
    let* ranges = many1 parse_range in
    let* _ = char '\n' in
    let* ids = sep_by1 (char '\n') parse_integer in
    return (ranges, ids)

  let parse_input str =
    match parse_string ~consume:Prefix parse_input str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)
  let compute_simple (ranges, ids) =
    let fresh_ids =
      List.filter
        (fun id ->
          List.exists
            (fun range -> range.left <= id && id <= range.right)
            ranges)
        ids
    in
    Printf.printf "Task 1: %i\n" (List.length fresh_ids);
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let rec insert ranges range =
    (* Find the first range that overlaps. *)
    let overlaps =
      List.filter
        (fun inner_range ->
          (inner_range.left <= range.left && range.left <= inner_range.right)
          || inner_range.left <= range.right
             && range.right <= inner_range.right)
        ranges
      |> List.sort (fun range_x range_y -> compare range_x.left range_y.left)
    in
    (* If no other range overlaps, insert this range into the set of ranges. *)
    if List.length overlaps = 0 then range :: ranges
    else
      let overlap = List.hd overlaps in
      (* Otherwise, split this range into three parts:
         1. The start of the range that does not overlap.
         -> Insert this range into the set of ranges.
         2. The middle of the range that overlaps with a range already in the set.
         -> Throw this range away.
         3. The rest.
         -> Try to insert this part recursively.
      *)
      let extended_ranges =
        if overlap.left <= range.left then ranges
        else { left = range.left; right = overlap.left - 1 } :: ranges
      in
      if range.right <= overlap.right then extended_ranges
      else
        insert extended_ranges { left = overlap.right + 1; right = range.right }

  let compute_advanced (ranges, _) =
    let sum_fresh_ids =
      ranges
      (* First sort the ranges by their starting point, *)
      |> List.sort (fun range_x range_y -> compare range_x.left range_y.left)
      (* then create a new set of ranges that has no overlaps, *)
      |> List.fold_left (fun acc range -> insert acc range) []
      (* and then count the number of fresh ids by summing up the ranges. *)
      |> List.map (fun range -> range.right - range.left + 1)
      |> List.fold_left (fun x y -> x + y) 0
    in
    Printf.printf "Task 2: %i\n" sum_fresh_ids;
    Ok ()
end
