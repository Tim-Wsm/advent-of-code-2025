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
  module PositionSet = Set.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

  module IntMap = Map.Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

  type shape_t = { layout : PositionSet.t; width : int; height : int }

  module ShapeSet = Set.Make (struct
    type t = shape_t

    let compare = Stdlib.compare
  end)

  module ShapeMap = Map.Make (struct
    type t = shape_t

    let compare = Stdlib.compare
  end)

  (*
  type shape_placement_t = {
    x : int;
    y : int;
    layout : PositionSet.t;
    score : int;
    advanced_score : int ShapeMap.t;
  }
  *)

  type region_t = { width : int; height : int; shapes : int list }
  type t = (int * shape_t) list * region_t list

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parses_integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse_tile = char '#' >>| (fun _ -> true) <|> (char '.' >>| fun _ -> false)

  let parse_shape =
    let* id = parses_integer <* char ':' <* end_of_line in
    let* grid = many1 (many1 parse_tile <* end_of_line) in
    let layout =
      grid
      |> List.mapi (fun y row ->
          List.mapi
            (fun x is_shape -> if is_shape then Some (x, y) else None)
            row)
      |> List.flatten
      |> List.filter_map (fun x -> x)
      |> PositionSet.of_list
    in
    return (id, { layout; width = List.length grid; height = List.length grid })

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
  (* Helper functions (printing)                                              *)
  (*--------------------------------------------------------------------------*)

  let _print_layout layout =
    let width =
      PositionSet.to_list layout
      |> List.map (fun (x, _) -> x)
      |> List.sort (fun a b -> compare b a)
      |> List.hd
    in
    let height =
      PositionSet.to_list layout
      |> List.map (fun (_, y) -> y)
      |> List.sort (fun a b -> compare b a)
      |> List.hd
    in

    List.iter
      (fun y ->
        List.iter
          (fun x ->
            if PositionSet.mem (x, y) layout then Printf.printf "#"
            else Printf.printf ".")
          (List.init (height + 1) (fun x -> x));
        Printf.printf "\n")
      (List.init (width + 1) (fun y -> y))

  let _print_shape (shape : shape_t) =
    Printf.printf "(dim:%ix%i, size:%i)\n" shape.width shape.height
      (PositionSet.cardinal shape.layout);
    List.iter
      (fun y ->
        List.iter
          (fun x ->
            if PositionSet.mem (x, y) shape.layout then Printf.printf "#"
            else Printf.printf ".")
          (List.init shape.width (fun x -> x));
        Printf.printf "\n")
      (List.init shape.height (fun y -> y));
    Printf.printf "\n"

  let _print_region region =
    Printf.printf "%ix%i:" region.width region.height;
    List.iter (fun num_shapes -> Printf.printf " %i" num_shapes) region.shapes;
    Printf.printf "\n"

  (*--------------------------------------------------------------------------*)
  (* Helper functions (layouts and shapes)                                    *)
  (*--------------------------------------------------------------------------*)

  let rotate_layout_by layout width height num =
    match num with
    | 0 -> layout
    | 1 ->
        (* rotate by 90° *)
        layout |> PositionSet.map (fun (x, y) -> (y, width - x - 1))
    | 2 ->
        (* rotate by 180° *)
        layout
        |> PositionSet.map (fun (x, y) -> (width - x - 1, height - y - 1))
    | 3 ->
        (* rotate by 270° *)
        layout
        |> PositionSet.map (fun (x, y) ->
            (height - y - 1, width - (width - x - 1) - 1))
    | _ -> assert false

  (*--------------------------------------------------------------------------*)
  (* Helper functions                                                         *)
  (*--------------------------------------------------------------------------*)

  let possible_placements (shape : shape_t) width height =
    (* all x values between 0 and width - shape dimensions *)
    let xs = Seq.init (width - shape.width + 1) (fun x -> x) in
    (* all y values between 0 and height - shape dimensions *)
    let ys = Seq.init (height - shape.height + 1) (fun y -> y) in
    (* all four rotations of the shape *)
    let num_rotations = Seq.init 4 (fun n -> n) in
    (* generate all possible placements of the shape *)
    Seq.product xs ys |> Seq.product num_rotations
    |> Seq.filter (fun (_, (target_x, target_y)) ->
        assert (shape.width = shape.height);
        target_x + shape.width <= width && target_y + shape.height <= height)
    |> Seq.map (fun (num_rotations, (target_x, target_y)) ->
        let layout =
          rotate_layout_by shape.layout shape.width shape.height num_rotations
          |> PositionSet.map (fun (x, y) -> (x + target_x, y + target_y))
        in
        { layout; width; height })

  let possible_overlapping_layouts shape =
    Seq.product
      (Seq.init 3 (fun x_shift -> -x_shift))
      (Seq.init 3 (fun y_shift -> -y_shift))
    |> Seq.product (Seq.init 4 (fun n -> n))
    |> Seq.map (fun (num_rotations, (target_x, target_y)) ->
        rotate_layout_by shape.layout shape.width shape.height num_rotations
        |> PositionSet.map (fun (shape_x, shape_y) ->
            (shape_x + target_x, shape_y + target_y)))
    |> Seq.filter (fun rotated_layout ->
        PositionSet.exists
          (fun (shape_x, shape_y) -> 0 = shape_x && 0 = shape_y)
          rotated_layout)

  let tile_is_usable layout x y width height shape shapes_possible_layouts =
    ShapeMap.find shape shapes_possible_layouts
    |> Seq.map (fun possible_layout ->
        possible_layout
        |> PositionSet.map (fun (shape_x_shift, shape_y_shift) ->
            (x + shape_x_shift, y + shape_y_shift)))
    |> Seq.filter (fun possible_layout ->
        PositionSet.for_all
          (fun (shape_x, shape_y) ->
            0 <= shape_x && shape_x < width && 0 <= shape_y && shape_y < height)
          possible_layout)
    |> Seq.exists (fun possible_layout ->
        PositionSet.for_all
          (fun (shape_x, shape_y) ->
            not (PositionSet.mem (shape_x, shape_y) layout))
          possible_layout)

  let count_if width height f =
    let rec iter x y acc =
      if height <= y then acc
      else if width <= x then iter 0 (y + 1) acc
      else if f x y then iter (x + 1) y (acc + 1)
      else iter (x + 1) y acc
    in
    iter 0 0 0

  let num_usable_tiles layout width height shape shapes_possible_layouts =
    count_if width height (fun x y ->
        (not (PositionSet.mem (x, y) layout))
        && tile_is_usable layout x y width height shape shapes_possible_layouts)

  let num_usable_tiles_combined layout width height shapes
      shapes_possible_layouts =
    count_if width height (fun x y ->
        (not (PositionSet.mem (x, y) layout))
        && ShapeSet.exists
             (fun shape ->
               tile_is_usable layout x y width height shape
                 shapes_possible_layouts)
             shapes)

  let calculate_score layout width height remaining_shapes
      shapes_possible_layouts =
    ShapeSet.to_list remaining_shapes
    |> List.map (fun shape ->
        ( shape,
          num_usable_tiles layout width height shape shapes_possible_layouts ))
    |> ShapeMap.of_list

  let score_strictly_worse score_1 score_2 =
    ShapeMap.exists
      (fun shape_1 num_usable_tiles ->
        num_usable_tiles < ShapeMap.find shape_1 score_2)
      score_1
    && ShapeMap.for_all
         (fun shape_1 num_usable_tiles ->
           num_usable_tiles <= ShapeMap.find shape_1 score_2)
         score_1

  (* This version works even if the shapes could be more tightly packed (such as the simpel input) *)
  let rec _all_shapes_fit_propper (shapes : shape_t list) width height layout
      shapes_possible_layouts =
    match shapes with
    | [] -> true
    | shape :: rest ->
        let num_remaining_tiles =
          rest
          |> List.map (fun shape -> PositionSet.cardinal shape.layout)
          |> List.fold_left ( + ) 0
        in

        let remaining_shapes = ShapeSet.of_list rest in

        let placement_is_eq_or_mirror placement_1 placement_2 =
          PositionSet.equal placement_1.layout placement_2.layout
          || PositionSet.equal placement_1.layout
               (rotate_layout_by placement_2.layout placement_2.width
                  placement_2.height 2)
        in

        (* generate all possible placements of the shape *)
        possible_placements shape width height
        |>
        (* remove all placements that overlap in the current layout *)
        Seq.filter (fun placement ->
            PositionSet.for_all
              (fun (x, y) -> not (PositionSet.mem (x, y) layout))
              placement.layout)
        |>
        (* apply every possible placement *)
        Seq.map (fun placement ->
            {
              placement with
              layout = PositionSet.union placement.layout layout;
            })
        |> Seq.memoize
        |>
        (* remove all placements that result in a mirrored solution *)
        fun placements ->
        Seq.filter
          (fun p1 ->
            Some p1
            = Seq.find (fun p2 -> placement_is_eq_or_mirror p1 p2) placements)
          placements
        |> Seq.memoize
        |>
        (* remove all placements that result in a layout that does not fit
           all remaining shapes *)
        Seq.filter (fun placed_shape ->
            num_remaining_tiles
            <= num_usable_tiles_combined placed_shape.layout placed_shape.width
                 placed_shape.height remaining_shapes shapes_possible_layouts)
        |> Seq.memoize
        (* calculate the score for every possible placement *)
        |> Seq.map (fun placed_shape ->
            ( placed_shape,
              calculate_score placed_shape.layout placed_shape.width
                placed_shape.height remaining_shapes shapes_possible_layouts ))
        |> Seq.memoize
        |>
        (* remove all placements for which have a strictly worse score *)
        fun placed_shapes ->
        Seq.filter
          (fun (_, s1) ->
            Seq.for_all
              (fun (_, s2) -> not (score_strictly_worse s1 s2))
              placed_shapes)
          placed_shapes
        |>
        (* remove all placements that will not fit the remaining shapes *)
        Seq.filter (fun (_, score) ->
            ShapeSet.for_all
              (fun remaining_shape ->
                let num_shapes =
                  rest
                  |> List.filter (fun s -> s = remaining_shape)
                  |> List.length
                in
                PositionSet.cardinal remaining_shape.layout * num_shapes
                <= ShapeMap.find remaining_shape score)
              remaining_shapes)
        |> Seq.map (fun (p, _) -> p)
        |> Seq.exists (fun placed_shape ->
            _all_shapes_fit_propper rest width height placed_shape.layout
              shapes_possible_layouts)

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  (* This solution works for the given "complex" input. 
    
     Although this only works because the "complex" input does not consider
     thight packing such as the small example from the description.
   *)
  let all_shapes_fit (shapes : shape_t list) width height layout
      shapes_possible_layouts =
    let num_remaining_tiles =
      shapes
      |> List.map (fun shape -> PositionSet.cardinal shape.layout)
      |> List.fold_left ( + ) 0
    in

    num_remaining_tiles
    <= num_usable_tiles_combined layout width height (ShapeSet.of_list shapes)
         shapes_possible_layouts

  let compute_simple (shapes, regions) =
    let shapes_possible_layouts =
      shapes
      |> List.map (fun (_, shape) -> shape)
      |> ShapeSet.of_list |> ShapeSet.to_list
      |> List.map (fun shape ->
          (shape, Seq.memoize (possible_overlapping_layouts shape)))
      |> ShapeMap.of_list
    in

    let result =
      regions
      |> List.filter (fun region ->
          let shapes =
            region.shapes
            |> List.mapi (fun shape_id shape_num ->
                List.init shape_num (fun _ -> List.nth shapes shape_id))
            |> List.flatten
            |> List.map (fun (_, shape) -> shape)
          in

          all_shapes_fit shapes region.width region.height PositionSet.empty
            shapes_possible_layouts)
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
