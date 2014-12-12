open OUnit2
open Gpio

module TestController = struct
  let get ctx =
    assert_equal
      [Controller.{base=0; label="1c20800.pinctrl"; ngpio=288}]
      (Controller.get ())

  let export_unexport ctx =
    let ctrl = Controller.get_first () in
    assert_equal ~msg:"export" `Ok (Controller.export ctrl 34);
    assert_equal ~msg:"export_fs" true (Sys.file_exists "/sys/class/gpio/gpio34");
    assert_equal ~msg:"unexport" `Ok (Controller.unexport ctrl 34);
    assert_equal ~msg:"unexport_fs" false (Sys.file_exists "/sys/class/gpio/gpio34")
end

module TestGPIO = struct
  let get_direction ctx =
    let ctrl = Controller.get_first () in
    assert_equal ~msg:"export" `Ok @@ Controller.export ctrl 34;
    assert_equal ~msg:"export_fs" true (Sys.file_exists "/sys/class/gpio/gpio34");
    assert_equal ~msg:"get_direction" `In @@ GPIO.get_direction 34;
    assert_equal ~msg:"unexport" `Ok @@ Controller.unexport ctrl 34;
    assert_equal ~msg:"unexport_fs" false (Sys.file_exists "/sys/class/gpio/gpio34")

  let get_value ctx =
    let ctrl = Controller.get_first () in
    assert_equal `Ok @@ Controller.export ctrl 34;
    assert_equal `High @@ GPIO.get_value 34;
    assert_equal `Ok @@ Controller.unexport ctrl 34
end

let suite =
  "Gpio">:::
  [
    "Controller.get" >:: TestController.get;
    "Controller.export_unexport" >:: TestController.export_unexport;
    "GPIO.get_direction" >:: TestGPIO.get_direction;
(*      "GPIO.get_value" >:: TestGPIO.get_value; *)
  ]

let () =
  run_test_tt_main suite
