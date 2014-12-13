open Lwt
open Gpio

let probe c =
  Format.printf "Probing pins controlled by %s@." c.label;
  for%lwt i = c.base to c.base + c.ngpio - 1 do
    (try%lwt
      export c i >>
      let%lwt p = get_pin i in
      return @@ Format.printf "%d: %a@." i pp_pin p;
    with exn ->
      return @@ Format.printf "%d: %s@." i (Printexc.to_string exn))
      [%finally try%lwt unexport c i with _ -> return_unit]
  done >|= fun () -> Format.printf "@."

let main () =
  let%lwt ctrls = controllers () in
  Lwt_list.iter_s probe ctrls

let () = Lwt_main.run @@ main ()
