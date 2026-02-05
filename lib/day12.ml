(*  --- Day 12: Christmas Tree Farm ---

You're almost out of time, but there can't be much left to decorate. Although
there are no stairs, elevators, escalators, tunnels, chutes, teleporters,
firepoles, or conduits here that would take you deeper into the North Pole
base, there is a ventilation duct. You jump in.

After bumping around for a few minutes, you emerge into a large, well-lit
cavern full of Christmas trees!

There are a few Elves here frantically decorating before the deadline. They
think they'll be able to finish most of the work, but the one thing they're
worried about is the presents for all the young Elves that live here at the
North Pole. It's an ancient tradition to put the presents under the trees, but
the Elves are worried they won't fit.

The presents come in a few standard but very weird shapes. The shapes and the
regions into which they need to fit are all measured in standard units. To be
aesthetically pleasing, the presents need to be placed into the regions in a
way that follows a standardized two-dimensional unit grid; you also can't stack
presents.

As always, the Elves have a summary of the situation (your puzzle input) for
you. First, it contains a list of the presents' shapes. Second, it contains the
size of the region under each tree and a list of the number of presents of each
shape that need to fit into that region. For example:

0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2

The first section lists the standard present shapes. For convenience, each
shape starts with its index and a colon; then, the shape is displayed visually,
where # is part of the shape and . is not.

The second section lists the regions under the trees. Each line starts with the
width and length of the region; 12x5 means the region is 12 units wide and 5
units long. The rest of the line describes the presents that need to fit into
that region by listing the quantity of each shape of present; 1 0 1 0 3 2 means
you need to fit one present with shape index 0, no presents with shape index 1,
one present with shape index 2, no presents with shape index 3, three presents
with shape index 4, and two presents with shape index 5.

Presents can be rotated and flipped as necessary to make them fit in the
available space, but they have to always be placed perfectly on the grid.
Shapes can't overlap (that is, the # part from two different presents can't go
in the same place on the grid), but they can fit together (that is, the . part
in a present's shape's diagram does not block another present from occupying
that space on the grid).

The Elves need to know how many of the regions can fit the presents listed. In
the above example, there are six unique present shapes and three regions that
need checking.

The first region is 4x4:

....
....
....
....

In it, you need to determine whether you could fit two presents that have shape
index 4:

###
#..
###

After some experimentation, it turns out that you can fit both presents in this
region. Here is one way to do it, using A to represent one present and B to
represent the other:

AAA.
ABAB
ABAB
.BBB

The second region, 12x5: 1 0 1 0 2 2, is 12 units wide and 5 units long. In
that region, you need to try to fit one present with shape index 0, one present
with shape index 2, two presents with shape index 4, and two presents with
shape index 5.

It turns out that these presents can all fit in this region. Here is one way to
do it, again using different capital letters to represent all the required
presents:

....AAAFFE.E
.BBBAAFFFEEE
DDDBAAFFCECE
DBBB....CCC.
DDD.....C.C.

The third region, 12x5: 1 0 1 0 3 2, is the same size as the previous region;
the only difference is that this region needs to fit one additional present
with shape index 4. Unfortunately, no matter how hard you try, there is no way
to fit all of the presents into this region.

So, in this example, 2 regions can fit all of their listed presents.

Consider the regions beneath each tree and the presents the Elves would like to
fit into each of them. How many of the regions can fit all of the presents
listed?
*)

open Angstrom
open Solution
open CustomErrors

module Day12 : Solution = struct
  type tile_t = Shape | Empty
  type shape_t = { id : int; layout : tile_t list list; dimensions : int }
  type region_t = { width : int; height : int; shapes : int list }
  type t = shape_t list * region_t list

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parses_integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse_tile =
    char '#' >>| (fun _ -> Shape) <|> (char '.' >>| fun _ -> Empty)

  let parse_shape =
    let* id = parses_integer <* char ':' <* end_of_line in
    let* layout = many1 (many1 parse_tile <* end_of_line) in
    let dimensions = List.length layout in
    assert (List.for_all (fun row -> dimensions = List.length row) layout);
    return { id; layout; dimensions }

  let parse_region =
    let* width = parses_integer in
    let* _ = char 'x' in
    let* height = parses_integer in
    let* _ = char ':' in
    let* _ = char ' ' in
    let* shapes = sep_by1 (char ' ') parses_integer in
    return { width; height; shapes }

  let parse_situation =
    let* shapes = sep_by1 end_of_line parse_shape in
    let* _ = end_of_line in
    let* regions = sep_by1 end_of_line parse_region in
    return (shapes, regions)

  let parse_input str =
    match parse_string ~consume:Prefix parse_situation str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Helper functions                                                         *)
  (*--------------------------------------------------------------------------*)

  let print_layout layout =
    List.iter
      (fun row ->
        List.iter
          (function Shape -> Printf.printf "#" | Empty -> Printf.printf ".")
          row;
        Printf.printf "\n")
      layout

  let print_shape shape =
    Printf.printf "%i:\n" shape.id;
    print_layout shape.layout;
    Printf.printf "\n"

  let print_region region =
    Printf.printf "%ix%i:" region.width region.height;
    List.iter (fun num_shapes -> Printf.printf " %i" num_shapes) region.shapes;
    Printf.printf "\n"

  let empty_layout width height =
    List.init height (fun _ -> List.init width (fun _ -> Empty))

  let rotate_shape shape =
    let layout =
      List.fold_left
        (fun acc row -> List.map2 (fun column value -> value :: column) acc row)
        (List.init (List.length shape.layout) (fun _ -> []))
        shape.layout
    in
    { shape with layout }

  let rotate_shape_by shape num =
    match num with
    | 0 -> shape
    | 1 -> rotate_shape shape
    | 2 -> rotate_shape (rotate_shape shape)
    | 3 -> rotate_shape (rotate_shape (rotate_shape shape))
    | _ -> assert false

  (* the x y coordinates corespond to the top left corner of the shape *)
  let place_shape shape target_x target_y width height =
    assert (target_x + shape.dimensions <= width);
    assert (target_y + shape.dimensions <= height);
    List.init height (fun y ->
        List.init width (fun x ->
            if
              target_x <= x
              && x < target_x + shape.dimensions
              && target_y <= y
              && y < target_y + shape.dimensions
            then List.nth (List.nth shape.layout (y - target_y)) (x - target_x)
            else Empty))

  let possible_placements shape width height =
    (* all x values between 0 and width - shape dimensions *)
    (* all y values between 0 and height - shape dimensions *)
    (* all four rotations of the shape *)
    List.init
      (width - shape.dimensions + 1)
      (fun x ->
        List.init
          (height - shape.dimensions + 1)
          (fun y -> List.init 4 (fun num_rotations -> (x, y, num_rotations))))
    |> List.flatten |> List.flatten

  let layouts_overlap layout_1 layout_2 =
    List.map2
      (fun row_1 row_2 ->
        List.map2 (fun tile_1 tile_2 -> (tile_1, tile_2)) row_1 row_2)
      layout_1 layout_2
    |> List.exists (List.exists (function Shape, Shape -> true | _ -> false))

  let merge_layouts layout_1 layout_2 =
    List.map2
      (fun row_1 row_2 ->
        List.map2
          (fun tile_1 tile_2 ->
            match (tile_1, tile_2) with
            | Shape, _ | _, Shape -> Shape
            | Empty, Empty -> Empty)
          row_1 row_2)
      layout_1 layout_2

  let tile_is_usable x y width height layout shapes =
    (* first obtain all possible placements around this position *)
    shapes
    |> List.map (fun (_, shape) ->
        List.init 4 (fun num_rotation ->
            List.init 3 (fun x_shift ->
                List.init 3 (fun y_shift ->
                    (shape, x + x_shift, y + y_shift, num_rotation)))))
    |> List.flatten |> List.flatten |> List.flatten
    |> List.filter (fun (shape, x, y, _) ->
        x + shape.dimensions <= width && y + shape.dimensions <= height)
      (* then check if there is one placement that can fill this tile *)
    |> List.exists (fun (shape, x, y, num_rotations) ->
        let placed_shape =
          place_shape (rotate_shape_by shape num_rotations) x y width height
        in
        (not (layouts_overlap placed_shape layout))
        && List.nth (List.nth (merge_layouts placed_shape layout) y) x = Shape)

  let num_usable_tiles width height layout shapes =
    List.init width (fun y -> List.init height (fun x -> (x, y)))
    |> List.flatten
    |> List.filter (fun (x, y) -> tile_is_usable x y width height layout shapes)
    |> List.length

  let rec all_shapes_fit shapes width height layout =
    match shapes with
    | [] -> true
    | (0, _) :: rest -> all_shapes_fit rest width height layout
    | (n, shape) :: rest ->
        let placements_sorted_by_usable_tiles =
          possible_placements shape width height
          |> List.filter_map (fun (x, y, num_rotations) ->
              let placed_shape =
                place_shape
                  (rotate_shape_by shape num_rotations)
                  x y width height
              in
              if layouts_overlap layout placed_shape then None
              else
                let merged_layout = merge_layouts layout placed_shape in
                if n = 0 then
                  Some
                    ( (x, y, num_rotations),
                      num_usable_tiles width height merged_layout rest )
                else
                  Some
                    ( (x, y, num_rotations),
                      num_usable_tiles width height merged_layout shapes ))
          |> List.sort (fun (_, x) (_, y) -> compare y x)
        in

        if List.is_empty placements_sorted_by_usable_tiles then false
        else
          let _, optimal_usable_tiles =
            List.hd placements_sorted_by_usable_tiles
          in

          placements_sorted_by_usable_tiles
          |> List.filter (fun (_, usable_tiles) ->
              usable_tiles = optimal_usable_tiles)
          |> List.map (fun (placement, _) -> placement)
          |> List.exists (fun (x, y, num_rotations) ->
              let placed_shape =
                place_shape
                  (rotate_shape_by shape num_rotations)
                  x y width height
              in
              let merged_layout = merge_layouts placed_shape layout in
              all_shapes_fit ((n - 1, shape) :: rest) width height merged_layout)

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  let compute_simple (shapes, regions) =
    List.iter (fun shape -> print_shape shape) shapes;
    List.iter (fun region -> print_region region) regions;
    Printf.printf "\n";

    let result =
      regions
      |> List.filter (fun region ->
          let shapes =
            region.shapes
            |> List.mapi (fun shape_id shape_num ->
                (shape_num, List.nth shapes shape_id))
          in
          all_shapes_fit shapes region.width region.height
            (empty_layout region.width region.height))
      |> List.length
    in

    Printf.printf "Task 1: %i\n" result;
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let compute_advanced _ =
    Printf.printf "Task 2: \n";
    Ok ()
end
