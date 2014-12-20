open Gpio

let probe c =
  Format.printf "Probing pins controlled by %s@." c.label;
  for i = c.base to c.base + c.ngpio - 1 do
    (try
      export c i;
      let p = get_pin i in
      Format.printf "%d: %a@." i pp_pin p;
    with exn ->
      Format.printf "%d: %s@." i (Printexc.to_string exn));
    try unexport c i with _ -> ()
  done; Format.printf "@."

let main () =
  let ctrls = Gen.fold (fun acc c -> c::acc) [] @@ controllers () in
  List.iter probe ctrls

let () = main ()
