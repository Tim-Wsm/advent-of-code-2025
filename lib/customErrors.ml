open Cmdliner

module CustomErrors = struct
  let day_not_implemented = -1
  let day_does_not_exist = -2
  let parsing_error = -3

  let exits =
    [
      Cmd.Exit.info day_not_implemented
        ~doc:"No solution is implemented for this day.";
      Cmd.Exit.info day_does_not_exist ~doc:"This day does not exist";
      Cmd.Exit.info parsing_error
        ~doc:"There was an error while parsing the input.";
    ]
    @ Cmd.Exit.defaults
end
