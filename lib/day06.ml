(* --- Day 6: Trash Compactor ---

After helping the Elves in the kitchen, you were taking a break and helping
them re-enact a movie scene when you over-enthusiastically jumped into the
garbage chute!

A brief fall later, you find yourself in a garbage smasher. Unfortunately, the
door's been magnetically sealed.

As you try to find a way out, you are approached by a family of cephalopods!
They're pretty sure they can get the door open, but it will take some time.
While you wait, they're curious if you can help the youngest cephalopod with
her math homework.

Cephalopod math doesn't look that different from normal math. The math
worksheet (your puzzle input) consists of a list of problems; each problem has
a group of numbers that need to be either added (+) or multiplied ( * )
together.

However, the problems are arranged a little strangely; they seem to be
presented next to each other in a very long horizontal list. For example:

    123 328  51 64 
     45 64  387 23 
      6 98  215 314
    *   +   *   +  

Each problem's numbers are arranged vertically; at the bottom of the problem is
the symbol for the operation that needs to be performed. Problems are separated
by a full column of only spaces. The left/right alignment of numbers within
each problem can be ignored.

So, this worksheet contains four problems:

    123 * 45 * 6 = 33210
    328 + 64 + 98 = 490
    51 * 387 * 215 = 4243455
    64 + 23 + 314 = 401

To check their work, cephalopod students are given the grand total of adding
together all of the answers to the individual problems. In this worksheet, the
grand total is 33210 + 490 + 4243455 + 401 = 4277556.

Of course, the actual worksheet is much wider. You'll need to make sure to
unroll it completely so that you can read the problems clearly.

Solve the problems on the math worksheet. What is the grand total found by
adding together all of the answers to the individual problems?

--- Part Two ---

The big cephalopods come back to check on how things are going. When they see
that your grand total doesn't match the one expected by the worksheet, they
realize they forgot to explain how to read cephalopod math.

Cephalopod math is written right-to-left in columns. Each number is given in
its own column, with the most significant digit at the top and the least
significant digit at the bottom. (Problems are still separated with a column
consisting only of spaces, and the symbol at the bottom of the problem is still
the operator to use.)

Here's the example worksheet again:

123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  

Reading the problems right-to-left one column at a time, the problems are now
quite different:

    - The rightmost problem is 4 + 431 + 623 = 1058
    - The second problem from the right is 175 * 581 * 32 = 3253600
    - The third problem from the right is 8 + 248 + 369 = 625
    - Finally, the leftmost problem is 356 * 24 * 1 = 8544

Now, the grand total is 1058 + 3253600 + 625 + 8544 = 3263827.

Solve the problems on the math worksheet again. What is the grand total found
by adding together all of the answers to the individual problems?

*)

open Angstrom
open Solution
open CustomErrors

module Day06 : Solution = struct
  type tile_t = Blank | Num of int
  type operation_t = Mul | Add

  let to_op = function Mul -> fun x y -> x * y | Add -> fun x y -> x + y

  type t = tile_t list list * operation_t list

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parse_num =
    satisfy (function '0' .. '9' -> true | _ -> false) >>| fun i ->
    int_of_char i - int_of_char '0'

  let parse_rows =
    many1
      (many1 (parse_num >>| (fun i -> Num i) <|> (char ' ' >>| fun _ -> Blank))
      <* end_of_line)

  let parse_operations =
    many1
      (skip_while (function ' ' -> true | _ -> false)
       *> (char '*' >>| (fun _ -> Mul) <|> (char '+' >>| fun _ -> Add))
      <* skip_while (function ' ' -> true | _ -> false))
    <* end_of_line

  let parse_worksheet =
    let* values = parse_rows in
    let* operations = parse_operations in
    return (values, operations)

  let parse_input str =
    match parse_string ~consume:Prefix parse_worksheet str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  let transpose rows =
    List.hd rows |> List.map (fun value -> [ value ]) |> fun init_rows ->
    List.fold_left
      (fun acc row -> List.map2 (fun column value -> value :: column) acc row)
      init_rows (List.tl rows)

  let compute_simple (raw_rows, operations) =
    let sum_of_results =
      (* First, parse the raw rows into rows of values, *)
      raw_rows
      |> List.map
           (List.fold_left
              (fun (row, value_opt) tile ->
                match (tile, value_opt) with
                | Blank, None -> (row, None)
                | Blank, Some value -> (value :: row, None)
                | Num i, None -> (row, Some i)
                | Num i, Some j -> (row, Some ((j * 10) + i)))
              ([], None))
      |> List.map (fun (row, value_opt) ->
          match value_opt with
          | None -> List.rev row
          | Some value -> List.rev (value :: row))
      (* then extract the columns from the rows of values, *)
      |> transpose
      (* and finally compute the results for each column and sum up the results *)
      |> List.map2
           (fun operation column ->
             List.fold_left (to_op operation) (List.hd column) (List.tl column))
           operations
      |> List.fold_left (fun x y -> x + y) 0
    in

    Printf.printf "Task 1: %i\n" sum_of_results;
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let compute_advanced (raw_rows, operations) =
    let sum_of_results =
      (* First, transpose the rows into columns, *)
      transpose raw_rows
      |> List.map (fun row -> List.rev row)
      (* then extract the vertical values from the rows *)
      |> List.map (List.filter_map (function Blank -> None | Num i -> Some i))
      |> List.fold_left
           (fun columns row ->
             match (columns, row) with
             | columns, [] -> [] :: columns
             | column :: columns, row -> (row :: column) :: columns
             | _ -> assert false)
           [ [] ]
      |> List.rev
      |> List.map
           (List.map (List.fold_left (fun acc digit -> (acc * 10) + digit) 0))
      (* and finally compute the results for each column and sum up the results *)
      |> List.map2
           (fun operation column ->
             List.fold_left (to_op operation) (List.hd column) (List.tl column))
           operations
      |> List.fold_left (fun x y -> x + y) 0
    in

    Printf.printf "Task 2: %i\n" sum_of_results;
    Ok ()
end
