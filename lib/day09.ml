(* --- Day 9: Movie Theater ---

You slide down the firepole in the corner of the playground and land in the
North Pole base movie theater!

The movie theater has a big tile floor with an interesting pattern. Elves here
are redecorating the theater by switching out some of the square tiles in the
big grid they form. Some of the tiles are red; the Elves would like to find the
largest rectangle that uses red tiles for two of its opposite corners. They
even have a list of where the red tiles are located in the grid (your puzzle
input).

For example:

7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3

Showing red tiles as # and other tiles as ., the above arrangement of red tiles
would look like this:

..............
.......#...#..
..............
..#....#......
..............
..#......#....
..............
.........#.#..
..............

You can choose any two red tiles as the opposite corners of your rectangle;
your goal is to find the largest rectangle possible.

For example, you could make a rectangle (shown as O) with an area of 24 between
2,5 and 9,7:

..............
.......#...#..
..............
..#....#......
..............
..OOOOOOOO....
..OOOOOOOO....
..OOOOOOOO.#..
..............

Or, you could make a rectangle with area 35 between 7,1 and 11,7:

..............
.......OOOOO..
.......OOOOO..
..#....OOOOO..
.......OOOOO..
..#....OOOOO..
.......OOOOO..
.......OOOOO..
..............

You could even make a thin rectangle with an area of only 6 between 7,3 and
2,3:

..............
.......#...#..
..............
..OOOOOO......
..............
..#......#....
..............
.........#.#..
..............

Ultimately, the largest rectangle you can make in this example has area 50. One
way to do this is between 2,5 and 11,1:

..............
..OOOOOOOOOO..
..OOOOOOOOOO..
..OOOOOOOOOO..
..OOOOOOOOOO..
..OOOOOOOOOO..
..............
.........#.#..
..............

Using two red tiles as opposite corners, what is the largest area of any
rectangle you can make?

--- Part Two ---

The Elves just remembered: they can only switch out tiles that are red or
green. So, your rectangle can only include red or green tiles.

In your list, every red tile is connected to the red tile before and after it
by a straight line of green tiles. The list wraps, so the first red tile is
also connected to the last red tile. Tiles that are adjacent in your list will
always be on either the same row or the same column.

Using the same example as before, the tiles marked X would be green:

..............
.......#XXX#..
.......X...X..
..#XXXX#...X..
..X........X..
..#XXXXXX#.X..
.........X.X..
.........#X#..
..............

In addition, all of the tiles inside this loop of red and green tiles are also
green. So, in this example, these are the green tiles:

..............
.......#XXX#..
.......XXXXX..
..#XXXX#XXXX..
..XXXXXXXXXX..
..#XXXXXX#XX..
.........XXX..
.........#X#..
..............

The remaining tiles are never red nor green.

The rectangle you choose still must have red tiles in opposite corners, but any
other tiles it includes must now be red or green. This significantly limits
your options.

For example, you could make a rectangle out of red and green tiles with an area
of 15 between 7,3 and 11,1:

..............
.......OOOOO..
.......OOOOO..
..#XXXXOOOOO..
..XXXXXXXXXX..
..#XXXXXX#XX..
.........XXX..
.........#X#..
..............

Or, you could make a thin rectangle with an area of 3 between 9,7 and 9,5:

..............
.......#XXX#..
.......XXXXX..
..#XXXX#XXXX..
..XXXXXXXXXX..
..#XXXXXXOXX..
.........OXX..
.........OX#..
..............

The largest rectangle you can make in this example using only red and green
tiles has area 24. One way to do this is between 9,5 and 2,3:

..............
.......#XXX#..
.......XXXXX..
..OOOOOOOOXX..
..OOOOOOOOXX..
..OOOOOOOOXX..
.........XXX..
.........#X#..
..............

Using two red tiles as opposite corners, what is the largest area of any
rectangle you can make using only red and green tiles?


*)

open Angstrom
open Solution
open CustomErrors

module Day09 : Solution = struct
  type position_t = { x : int; y : int }
  type t = position_t list

  module Polygon = Set.Make (struct
    type t = position_t * position_t

    let compare = Stdlib.compare
  end)

  module PositionSet = Set.Make (struct
    type t = position_t

    let compare = Stdlib.compare
  end)

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parse_integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse_position =
    let* x = parse_integer in
    let* _ = char ',' in
    let* y = parse_integer in
    return { x; y }

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

  let rec get_permutations positions permutations =
    match positions with
    | [] | [ _ ] -> permutations
    | pos_1 :: rest ->
        get_permutations rest
          (List.append permutations
             (List.map (fun pos_2 -> (pos_1, pos_2)) rest))

  let square_size pos_1 pos_2 =
    let length =
      if pos_1.x < pos_2.x then pos_2.x - pos_1.x else pos_1.x - pos_2.x
    in
    let height =
      if pos_1.y < pos_2.y then pos_2.y - pos_1.y else pos_1.y - pos_2.y
    in
    (length + 1) * (height + 1)

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  let compute_simple positions =
    let result =
      get_permutations positions []
      |> List.map (fun (pos_1, pos_2) -> square_size pos_1 pos_2)
      |> List.sort compare |> List.rev |> List.hd
    in

    Printf.printf "Task 1: %i\n" result;
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let square_to_polygon pos_1 pos_2 =
    let top_left =
      {
        x = (if pos_1.x < pos_2.x then pos_1.x else pos_2.x);
        y = (if pos_1.y < pos_2.y then pos_1.y else pos_2.y);
      }
    in
    let top_right =
      {
        x = (if pos_1.x < pos_2.x then pos_2.x else pos_1.x);
        y = (if pos_1.y < pos_2.y then pos_1.y else pos_2.y);
      }
    in
    let bottom_right =
      {
        x = (if pos_1.x < pos_2.x then pos_2.x else pos_1.x);
        y = (if pos_1.y < pos_2.y then pos_2.y else pos_1.y);
      }
    in
    let bottom_left =
      {
        x = (if pos_1.x < pos_2.x then pos_1.x else pos_2.x);
        y = (if pos_1.y < pos_2.y then pos_2.y else pos_1.y);
      }
    in
    Polygon.of_list
      [
        (top_left, top_right);
        (top_right, bottom_right);
        (bottom_left, bottom_right);
        (top_left, bottom_left);
      ]

  let vertex_contains_point (pos_a, pos_b) pos =
    pos.x = pos_a.x && pos_a.x = pos_b.x && pos_a.y <= pos.y && pos.y <= pos_b.y
    || pos.y = pos_a.y && pos_a.y = pos_b.y && pos_a.x <= pos.x
       && pos.x <= pos_b.x

  (* https://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/ *)
  let ccw pos_a pos_b pos_c =
    (pos_c.y - pos_a.y) * (pos_b.x - pos_a.x)
    > (pos_b.y - pos_a.y) * (pos_c.x - pos_a.x)

  let vertex_intersects (pos_a, pos_b) (pos_c, pos_d) =
    ccw pos_a pos_c pos_d <> ccw pos_b pos_c pos_d
    && ccw pos_a pos_b pos_c <> ccw pos_a pos_b pos_d

  let polygon_contains_point polygon pos =
    (* A polygon contains a point if the point is on one of the vertices *)
    let on_vertex =
      Polygon.exists (fun vertex -> vertex_contains_point vertex pos) polygon
    in
    (* ... or if a raycast from the point to the edge of the map intersects an
       odd number of verteces of the polygon. *)
    let inside_polygon =
      polygon
      |> Polygon.filter (fun vertex ->
          vertex_intersects vertex ({ pos with x = 0 }, pos))
      |> fun intersecting -> Polygon.cardinal intersecting mod 2 <> 0
    in
    on_vertex || inside_polygon

  let polygon_covers outer inner =
    (* A polygon covers a different polygon if all points of the inner
         polygon are contained in the outer polygon *)
    Polygon.to_list inner
    |> List.concat_map (fun (pos_a, pos_b) -> [ pos_a; pos_b ])
    |> PositionSet.of_list
    |> PositionSet.for_all (fun inner_point ->
        polygon_contains_point outer inner_point)
    &&
    (* ... and none of the vertices of the inner polygon intersect the vertices
       of the outer polygon (ending on the vertex is fine tho). *)
    Polygon.for_all
      (fun (inner_a, inner_b) ->
        Polygon.for_all
          (fun (outer_a, outer_b) ->
            (not (vertex_intersects (inner_a, inner_b) (outer_a, outer_b)))
            || inner_a.x = inner_b.x
               && (inner_a.x = outer_a.x || inner_a.x = outer_b.x)
            || inner_a.y = inner_b.y
               && (inner_a.y = outer_a.y || inner_a.y = outer_b.y)
            || outer_a.x = outer_b.x
               && (outer_a.x = inner_a.x || outer_a.x = inner_b.x)
            || outer_a.y = outer_b.y
               && (outer_a.y = inner_a.y || outer_a.y = inner_b.y))
          outer)
      inner

  let compute_advanced positions =
    let green_area =
      List.map2
        (fun pos_1 pos_2 ->
          if pos_1.x = pos_2.x && pos_1.y <= pos_2.y then (pos_1, pos_2)
          else if pos_1.x = pos_2.x && pos_2.y <= pos_1.y then (pos_2, pos_1)
          else if pos_1.y = pos_2.y && pos_1.x <= pos_2.x then (pos_1, pos_2)
          else if pos_1.y = pos_2.y && pos_2.x <= pos_1.x then (pos_2, pos_1)
          else assert false)
        positions
        (List.append (List.tl positions) [ List.hd positions ])
      |> Polygon.of_list
    in

    let result =
      get_permutations positions []
      |> List.sort (fun (pos_a, pos_b) (pos_c, pos_d) ->
          compare (square_size pos_a pos_b) (square_size pos_c pos_d))
      |> List.rev
      |> List.find (fun (pos_a, pos_b) ->
          polygon_covers green_area (square_to_polygon pos_a pos_b))
      |> fun (pos_a, pos_b) -> square_size pos_a pos_b
    in

    Printf.printf "Task 2: %i\n" result;
    Ok ()
end
