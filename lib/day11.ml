(* --- Day 11: Reactor ---

You hear some loud beeping coming from a hatch in the floor of the factory, so
you decide to check it out. Inside, you find several large electrical conduits
and a ladder.

Climbing down the ladder, you discover the source of the beeping: a large,
toroidal reactor which powers the factory above. Some Elves here are hurriedly
running between the reactor and a nearby server rack, apparently trying to fix
something.

One of the Elves notices you and rushes over. "It's a good thing you're here!
We just installed a new server rack, but we aren't having any luck getting the
reactor to communicate with it!" You glance around the room and see a tangle of
cables and devices running from the server rack to the reactor. She rushes off,
returning a moment later with a list of the devices and their outputs (your
puzzle input).

For example:

aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out

Each line gives the name of a device followed by a list of the devices to which
its outputs are attached. So, bbb: ddd eee means that device bbb has two
outputs, one leading to device ddd and the other leading to device eee.

The Elves are pretty sure that the issue isn't due to any specific device, but
rather that the issue is triggered by data following some specific path through
the devices. Data only ever flows from a device through its outputs; it can't
flow backwards.

After dividing up the work, the Elves would like you to focus on the devices
starting with the one next to you (an Elf hastily attaches a label which just
says you) and ending with the main output to the reactor (which is the device
with the label out).

To help the Elves figure out which path is causing the issue, they need you to
find every path from you to out.

In this example, these are all of the paths from you to out:

    - Data could take the connection from you to bbb, then from bbb to ddd,
      then from ddd to ggg, then from ggg to out.
    - Data could take the connection to bbb, then to eee, then to out.
    - Data could go to ccc, then ddd, then ggg, then out.
    - Data could go to ccc, then eee, then out.
    - Data could go to ccc, then fff, then out.

In total, there are 5 different paths leading from you to out.

How many different paths lead from you to out?

--- Part Two ---

Thanks in part to your analysis, the Elves have figured out a little bit about
the issue. They now know that the problematic data path passes through both dac
(a digital-to-analog converter) and fft (a device which performs a fast Fourier
transform).

They're still not sure which specific path is the problem, and so they now need
you to find every path from svr (the server rack) to out. However, the paths
you find must all also visit both dac and fft (in any order).

For example:

svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out

This new list of devices contains many paths from svr to out:

svr,aaa,fft,ccc,ddd,hub,fff,ggg,out
svr,aaa,fft,ccc,ddd,hub,fff,hhh,out
svr,aaa,fft,ccc,eee,dac,fff,ggg,out
svr,aaa,fft,ccc,eee,dac,fff,hhh,out
svr,bbb,tty,ccc,ddd,hub,fff,ggg,out
svr,bbb,tty,ccc,ddd,hub,fff,hhh,out
svr,bbb,tty,ccc,eee,dac,fff,ggg,out
svr,bbb,tty,ccc,eee,dac,fff,hhh,out

However, only 2 paths from svr to out visit both dac and fft.

Find all of the paths that lead from svr to out. How many of those paths visit
both dac and fft?
*)

open Angstrom
open Solution
open CustomErrors

module Day11 : Solution = struct
  module StringMap = Map.Make (String)
  module StringSet = Set.Make (String)

  type t = StringSet.t StringMap.t

  (*--------------------------------------------------------------------------*)
  (* Parser                                                                   *)
  (*--------------------------------------------------------------------------*)

  let parse_connection =
    let* source = take_while1 (function ':' -> false | _ -> true) in
    let* _ = char ':' in
    let* _ = char ' ' in
    let* destinations =
      sep_by1 (char ' ')
        (take_while1 (function ' ' | '\n' -> false | _ -> true))
      >>| StringSet.of_list
    in
    let* _ = end_of_line in
    return (StringMap.singleton source destinations)

  let parse_network =
    many1 parse_connection
    >>| List.fold_left
          (StringMap.union (fun _ _ _ -> assert false))
          StringMap.empty

  let parse_input str =
    match parse_string ~consume:Prefix parse_network str with
    | Ok v -> Ok v
    | Error msg ->
        Printf.eprintf "%s" msg;
        Error CustomErrors.parsing_error

  (*--------------------------------------------------------------------------*)
  (* Solution for the first task                                              *)
  (*--------------------------------------------------------------------------*)

  (* A simple breadth first path counting algorithm. *)
  let count_paths network start target ignored_nodes =
    let rec iter states num_paths =
      let final_states, remaining_states =
        states |> List.partition (fun (state, _) -> state = target)
      in

      let next_states =
        remaining_states
        |> List.filter (fun (state, visited_states) ->
            state <> target
            && (not (StringSet.mem state visited_states))
            && StringMap.mem state network)
        |> List.concat_map (fun (state, visited_states) ->
            let next_visited_states = StringSet.add state visited_states in
            StringMap.find state network
            |> StringSet.to_list
            |> List.filter_map (fun dest ->
                if not (StringSet.mem dest next_visited_states) then
                  Some (dest, next_visited_states)
                else None))
      in

      if List.is_empty next_states then num_paths + List.length final_states
      else iter next_states (num_paths + List.length final_states)
    in

    iter [ (start, ignored_nodes) ] 0

  let compute_simple network =
    Printf.printf "Task 1: %i\n"
      (count_paths network "you" "out" StringSet.empty);
    flush_all ();
    Ok ()

  (*--------------------------------------------------------------------------*)
  (* Solution for the second task                                             *)
  (*--------------------------------------------------------------------------*)

  (* A simple breadth first reachability algorithm. *)
  let rec reachable network states target visited_states =
    match states with
    | [] -> false
    | state :: _ when state = target -> true
    | state :: rest ->
        let next_states =
          match StringMap.find_opt state network with
          | None -> rest
          | Some destinations ->
              StringSet.to_list destinations
              |> List.append rest
              |> List.filter (fun dest ->
                  not (StringSet.mem dest visited_states))
        in
        reachable network next_states target
          (StringSet.add state visited_states)

  let network_filter network target ignore_set =
    let reachable_nodes =
      StringMap.to_list network
      |> List.filter (fun (source, _) ->
          reachable network [ source ] target ignore_set)
      |> List.fold_left
           (fun acc (source, _) -> StringSet.add source acc)
           StringSet.empty
    in
    StringMap.filter
      (fun state _ -> StringSet.mem state reachable_nodes)
      network
    |> StringMap.map (fun destinations ->
        StringSet.inter destinations reachable_nodes)

  let network_intersect =
    StringMap.merge (fun _ left_opt right_opt ->
        match (left_opt, right_opt) with
        | None, None -> None
        | Some destinations, None | None, Some destinations -> Some destinations
        | Some destinations_left, Some destinations_right ->
            destinations_left
            |> StringSet.filter (fun dest_left ->
                StringSet.exists
                  (fun dest_right -> dest_left = dest_right)
                  destinations_right)
            |> fun d -> if StringSet.is_empty d then None else Some d)

  let compute_advanced network =
    let paths_srv_to_fft =
      count_paths
        (network_intersect
           (network_filter network "fft" (StringSet.of_list [ "dac"; "out" ]))
           (network_filter network "dac" (StringSet.of_list [ "out" ])))
        "svr" "fft"
        (StringSet.of_list [ "dac"; "out" ])
    in

    let paths_srv_to_dac =
      count_paths
        (network_intersect
           (network_filter network "dac" (StringSet.of_list [ "fft"; "out" ]))
           (network_filter network "fft" (StringSet.of_list [ "out" ])))
        "svr" "dac"
        (StringSet.of_list [ "fft"; "out" ])
    in

    let paths_fft_to_dac =
      if paths_srv_to_fft = 0 then 0
      else
        count_paths
          (network_filter network "dac" (StringSet.of_list [ "fft"; "out" ]))
          "fft" "dac"
          (StringSet.of_list [ "svr"; "out" ])
    in

    let paths_dac_to_fft =
      if paths_srv_to_dac = 0 then 0
      else
        count_paths
          (network_filter network "fft" (StringSet.of_list [ "dac"; "out" ]))
          "dac" "fft"
          (StringSet.of_list [ "svr"; "out" ])
    in

    let paths_dac_to_out =
      if paths_fft_to_dac = 0 then 0
      else count_paths network "dac" "out" (StringSet.of_list [ "svr"; "fft" ])
    in

    let paths_fft_to_out =
      if paths_dac_to_fft = 0 then 0
      else count_paths network "fft" "out" (StringSet.of_list [ "svr"; "dac" ])
    in

    Printf.printf "Task 2: %i\n"
      ((paths_srv_to_fft * paths_fft_to_dac * paths_dac_to_out)
      + (paths_srv_to_dac * paths_dac_to_fft * paths_fft_to_out));

    Ok ()
end
