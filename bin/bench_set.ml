(*
open Basis.Rudiments
open Basis
*)

let main () =
  try
    Printexc.record_backtrace true;

    0
  with
    e -> begin
      Printf.eprintf "==> Error: %s\n%s\n"
        (Printexc.to_string e) (Printexc.get_backtrace ());
      exit 1
    end
in
exit (main ())
