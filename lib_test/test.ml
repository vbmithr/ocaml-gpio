open OUnit2

open Gpio

let pin = 252

let test_get ctx =
  let ctrl = CCOpt.get_exn @@ controllers () () in
  assert_equal ~printer:string_of_int ~msg:"base" 180 @@ ctrl.base;
  assert_equal ~msg:"label" "gpio_ich" @@ ctrl.label;
  assert_equal ~printer:string_of_int ~msg:"ngpio" 76 @@ ctrl.ngpio;
  for i = ctrl.base to ctrl.base + ctrl.ngpio - 1 do
    try unexport ctrl i with _ -> ()
  done

let export_unexport ctx =
  let ctrl = CCOpt.get_exn @@ controllers () () in
  export ctrl pin;
  assert_equal ~msg:"export_fs" true
    (Sys.file_exists @@ "/sys/class/gpio/gpio" ^ string_of_int pin);
  unexport ctrl pin;
  assert_equal ~msg:"unexport_fs" false
    (Sys.file_exists @@ "/sys/class/gpio/gpio" ^  string_of_int pin)

let test_get_direction ctx =
  let ctrl = CCOpt.get_exn @@ controllers () () in
  export ctrl pin;
  let d = get_direction pin in
  assert_equal ~msg:"get_direction" `In d;
  unexport ctrl pin

let test_get_value ctx =
  let ctrl = CCOpt.get_exn @@ controllers () () in
  export ctrl pin;
  let v = get_value pin in
  assert_equal `High v;
  unexport ctrl pin

let test_set_direction ctx =
  let ctrl = CCOpt.get_exn @@ controllers () () in
  export ctrl pin;
  let d = get_direction pin in
  assert_equal ~msg:"get_direction" `In d;
  set_direction pin `Out;
  let d = get_direction pin in
  assert_equal ~msg:"set_direction1" `Out d;
  set_direction pin `In;
  let d = get_direction pin in
  assert_equal ~msg:"set_direction2" `In d

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
