open Lwt
open OUnit2

open Gpio

let pin = 252

let test_get ctx =
  let inner () =
    let%lwt ctrl = one_controller () in
    assert_equal ~printer:string_of_int ~msg:"base" 180 @@ ctrl.base;
    assert_equal ~msg:"label" "gpio_ich" @@ ctrl.label;
    assert_equal ~printer:string_of_int ~msg:"ngpio" 76 @@ ctrl.ngpio;
    for%lwt i = ctrl.base to ctrl.base + ctrl.ngpio - 1 do
      try%lwt unexport ctrl i with _ -> return_unit
    done
    in Lwt_main.run @@ inner ()

let export_unexport ctx =
  let inner () =
    let%lwt ctrl = one_controller () in
    export ctrl pin >>= fun () ->
    assert_equal ~msg:"export_fs" true
      (Sys.file_exists @@ "/sys/class/gpio/gpio" ^ string_of_int pin);
    unexport ctrl pin >|= fun () ->
    assert_equal ~msg:"unexport_fs" false
      (Sys.file_exists @@ "/sys/class/gpio/gpio" ^  string_of_int pin)
    in Lwt_main.run @@ inner ()

let test_get_direction ctx =
  let inner () =
    let%lwt ctrl = one_controller () in
    export ctrl pin >>
    get_direction pin >>= fun d ->
    assert_equal ~msg:"get_direction" `In d;
    unexport ctrl pin
    in Lwt_main.run @@ inner ()

let test_set_direction ctx =
  let inner () =
    let%lwt ctrl = one_controller () in
    export ctrl pin >>
    (try%lwt
      get_direction pin >>= fun d ->
      assert_equal ~msg:"get_direction" `In d;
      set_direction pin `Out >>= fun () ->
      get_direction pin >>= fun d ->
      assert_equal ~msg:"set_direction1" `Out d;
      set_direction pin `In >>= fun () ->
      get_direction pin >|= fun d ->
      assert_equal ~msg:"set_direction2" `In d
     with exn -> fail exn)
      [%finally try%lwt
                  set_direction pin `In >>
                  unexport ctrl pin
        with _ -> return_unit]
    in Lwt_main.run @@ inner ()

let test_get_value ctx =
  let inner () =
    let%lwt ctrl = one_controller () in
    export ctrl pin >>= fun () ->
    get_value pin >>= fun v ->
    assert_equal `High v;
    unexport ctrl pin
    in Lwt_main.run @@ inner ()

let suite =
  "Gpio">:::
  [
    "get" >:: test_get;
    "export_unexport" >:: export_unexport;
    "get_direction" >:: test_get_direction;
    "get_value" >:: test_get_value;
    "set_direction" >:: test_set_direction;
  ]

let () =
  run_test_tt_main suite
