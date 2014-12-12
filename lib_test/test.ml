open Lwt

open OUnit2
open Gpio

module TestController = struct
  let get ctx =
    let open Controller in
    let ctrl = Lwt_main.run @@ get_first () in
    assert_equal 0 @@ base ctrl;
    assert_equal "1c20800.pinctrl" @@ label ctrl;
    assert_equal 288 @@ ngpio ctrl

  let export_unexport ctx =
    let inner () =
      let%lwt ctrl =  Controller.get_first () in
      Controller.export ctrl 34 >>= fun () ->
      assert_equal ~msg:"export_fs" true (Sys.file_exists "/sys/class/gpio/gpio34");
      Controller.unexport ctrl 34 >|= fun () ->
      assert_equal ~msg:"unexport_fs" false (Sys.file_exists "/sys/class/gpio/gpio34")
    in Lwt_main.run @@ inner ()
end

module TestGPIO = struct
  let get_direction ctx =
    let inner () =
      let%lwt ctrl = Controller.get_first () in
      Controller.export ctrl 34 >>= fun () ->
      GPIO.get_direction 34 >>= fun v ->
      assert_equal ~msg:"get_direction" `In v;
      Controller.unexport ctrl 34
    in Lwt_main.run @@ inner ()

  let get_value ctx =
    let inner () =
      let%lwt ctrl = Controller.get_first () in
      Controller.export ctrl 34 >>= fun () ->
      GPIO.get_value 34 >>= fun v ->
      assert_equal `High v;
      Controller.unexport ctrl 34
    in Lwt_main.run @@ inner ()
end

let suite =
  "Gpio">:::
  [
    "Controller.get" >:: TestController.get;
    (* "Controller.export_unexport" >:: TestController.export_unexport; *)
    (* "GPIO.get_direction" >:: TestGPIO.get_direction; *)
(*      "GPIO.get_value" >:: TestGPIO.get_value; *)
  ]

let () =
  run_test_tt_main suite
