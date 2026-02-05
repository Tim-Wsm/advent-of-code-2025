(* --- Day 3: Lobby ---

You descend a short staircase, enter the surprisingly vast lobby, and are
quickly cleared by the security checkpoint. When you get to the main elevators,
however, you discover that each one has a red light above it: they're all
offline.

"Sorry about that," an Elf apologizes as she tinkers with a nearby control
panel. "Some kind of electrical surge seems to have fried them. I'll try to get
them online soon."

You explain your need to get further underground. "Well, you could at least
take the escalator down to the printing department, not that you'd get much
further than that without the elevators working. That is, you could if the
escalator weren't also offline."

"But, don't worry! It's not fried; it just needs power. Maybe you can get it
running while I keep working on the elevators."

There are batteries nearby that can supply emergency power to the escalator for
just such an occasion. The batteries are each labeled with their joltage
rating, a value from 1 to 9. You make a note of their joltage ratings (your
puzzle input). For example:

987654321111111
811111111111119
234234234234278
818181911112111

The batteries are arranged into banks; each line of digits in your input
corresponds to a single bank of batteries. Within each bank, you need to turn
on exactly two batteries; the joltage that the bank produces is equal to the
number formed by the digits on the batteries you've turned on. For example, if
you have a bank like 12345 and you turn on batteries 2 and 4, the bank would
produce 24 jolts. (You cannot rearrange batteries.)

You'll need to find the largest possible joltage each bank can produce. In the
above example:

    - In 987654321111111, you can make the largest joltage possible, 98, by
      turning on the first two batteries.
    - In 811111111111119, you can make the largest joltage possible by turning
      on the batteries labeled 8 and 9, producing 89 jolts.
    - In 234234234234278, you can make 78 by turning on the last two batteries
      (marked 7 and 8).
    - In 818181911112111, the largest joltage you can produce is 92.

The total output joltage is the sum of the maximum joltage from each bank, so
in this example, the total output joltage is 98 + 89 + 78 + 92 = 357.

There are many batteries in front of you. Find the maximum joltage possible
from each bank; what is the total output joltage?

--- Part Two ---

The escalator doesn't move. The Elf explains that it probably needs more
joltage to overcome the static friction of the system and hits the big red
"joltage limit safety override" button. You lose count of the number of times
she needs to confirm "yes, I'm sure" and decorate the lobby a bit while you
wait.

Now, you need to make the largest joltage by turning on exactly twelve
batteries within each bank.

The joltage output for the bank is still the number formed by the digits of the
batteries you've turned on; the only difference is that now there will be 12
digits in each bank's joltage output instead of two.

Consider again the example from before:

987654321111111
811111111111119
234234234234278
818181911112111

Now, the joltages are much larger:

    - In 987654321111111, the largest joltage can be found by turning on
      everything except some 1s at the end to produce 987654321111.
    - In the digit sequence 811111111111119, the largest joltage can be found
      by turning on everything except some 1s, producing 811111111119.
    - In 234234234234278, the largest joltage can be found by turning on
      everything except a 2 battery, a 3 battery, and another 2 battery near the
      start to produce 434234234278.
    - In 818181911112111, the joltage 888911112111 is produced by turning on
      everything except some 1s near the front.

The total output joltage is now much larger: 987654321111 + 811111111119 +
434234234278 + 888911112111 = 3121910778619.

What is the new total output joltage?

*)

open Angstrom
open Solution
open CustomErrors

module Day03 : Solution = struct
  type t = int list list

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)
  let parse_battery =
    take_while1 (function '0' .. '9' -> true | _ -> false)
    >>| String.to_seq >>| List.of_seq >>| List.map int_of_char
    >>| List.map (fun x -> x - 48)

  let parse_list = sep_by1 (char '\n') parse_battery

  let parse_input str =
    match parse_string ~consume:Prefix parse_list str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  let rec find_biggest_voltage leading_digit biggest_voltage cells =
    match cells with
    | [] -> 0
    | [ digit ] ->
        if biggest_voltage < (leading_digit * 10) + digit then
          (leading_digit * 10) + digit
        else biggest_voltage
    | digit :: tail ->
        if leading_digit < digit then find_biggest_voltage digit 0 tail
        else if biggest_voltage < (leading_digit * 10) + digit then
          find_biggest_voltage leading_digit ((leading_digit * 10) + digit) tail
        else find_biggest_voltage leading_digit biggest_voltage tail

  let compute_simple batteries =
    let sum =
      batteries
      |> List.map (fun battery -> find_biggest_voltage 0 0 battery)
      |> List.fold_left (fun x y -> x + y) 0
    in
    Printf.printf "Task 1: %i\n" sum;
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  let rec find_biggest_voltage_advanced length cells =
    if length = 0 then 0
    else
      let leading_digit_index, leading_digit =
        cells
        (* first add the position to each cell*)
        |> List.mapi (fun i cell -> (i, cell))
        (* then filter all cells that can not be the leading digit*)
        |> List.filter (fun (i, _) -> i <= List.length cells - length)
        (* then sort the cells first by value then by position *)
        |> List.sort (fun (i_x, cell_x) (i_y, cell_y) ->
            if compare cell_x cell_y <> 0 then compare cell_x cell_y
            else compare i_y i_x)
        (* then take the biggest cell as the leading digit *)
        |> List.rev
        |> List.hd
      in
      let remaining_cells = List.drop (leading_digit_index + 1) cells in
      (leading_digit * Float.to_int (Float.pow 10.0 (Float.of_int (length - 1))))
      + find_biggest_voltage_advanced (length - 1) remaining_cells

  let compute_advanced batteries =
    let sum =
      batteries
      |> List.map (fun battery -> find_biggest_voltage_advanced 12 battery)
      |> List.fold_left (fun x y -> x + y) 0
    in
    Printf.printf "Task 2: %i\n" sum;
    Ok ()
end
