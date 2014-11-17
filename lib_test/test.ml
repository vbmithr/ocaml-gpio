open OUnit2
open Gpio

module TestGPIO = struct
end

module TestController = struct
  let get ctx =
    assert_equal true (List.length @@ Controller.get () >= 0)
end


let suite =
  "Gpio">:::
  [
    "Controller.get">:: TestController.get;
  ]

let () =
  run_test_tt_main suite
