(*
 * Copyright (c) 2014-2015 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Printf

module P = struct
  let (/) = Filename.concat
  let gpio = "/sys/class/gpio"
  let export = gpio / "export"
  let unexport = gpio / "unexport"
  let gpio_n n = gpio / "gpio" ^ string_of_int n
  let gpio_n_active_low n = gpio_n n / "active_low"
  let gpio_n_direction n = gpio_n n / "direction"
  let gpio_n_value n = gpio_n n / "value"
  let gpio_n_edge n = gpio_n n / "edge"
end

type direction =
  [`NA
  | `In [@printer fun fmt -> Format.fprintf fmt "in"]
  | `Out [@printer fun fmt -> Format.fprintf fmt "out"]
  ] [@@deriving show]

type value =
  [`Low [@printer fun fmt -> Format.fprintf fmt "0"]
  | `High [@printer fun fmt -> Format.fprintf fmt "1"]
  ] [@@deriving show]

type edge =
  [`NA
  | `None [@printer fun fmt -> Format.fprintf fmt "none"]
  | `Rising [@printer fun fmt -> Format.fprintf fmt "rising"]
  | `Falling [@printer fun fmt -> Format.fprintf fmt "falling"]
  | `Both [@printer fun fmt -> Format.fprintf fmt "both"]
  ] [@@deriving show]

type pin = {
  direction: direction;
  value: value;
  edge: edge;
  active_low: bool;
} [@@deriving show]

let pins = Bitv.create 4096 false

let direction_of_string = function
  | "in" -> `In
  | "out" -> `Out
  | _ -> invalid_arg "direction_of_string"

let value_of_string = function
  | "0" -> `Low
  | "1" -> `High
  | _ -> invalid_arg "value_of_string"

let edge_of_string = function
  | "none" -> `None
  | "rising" -> `Rising
  | "falling" -> `Falling
  | "both" -> `Both
  | _ -> invalid_arg "edge_of_string"

let bool_of_string = function
  | "0" -> false
  | "1" -> true
  | _ -> invalid_arg "bool_of_string"

let string_of_bool = function
  | false -> "0"
  | true -> "1"

let unexported id = Failure (sprintf "GPIO %d not exported or nonexistent" id)

let get_direction id =
  if Bitv.get pins id
  then
    try CCOpt.get_exn @@ CCIO.with_in (P.gpio_n_direction id) CCIO.read_line |>
        direction_of_string
    with _ -> `NA
  else raise (unexported id)

let get_value id =
  if Bitv.get pins id
  then CCOpt.get_exn @@ CCIO.with_in (P.gpio_n_value id) CCIO.read_line |> value_of_string
  else raise (unexported id)

let get_edge id =
  if Bitv.get pins id
  then
    try CCOpt.get_exn @@ CCIO.with_in (P.gpio_n_edge id) CCIO.read_line |>
        edge_of_string
    with _ -> `NA
  else raise (unexported id)

let get_active_low id =
  if Bitv.get pins id
  then CCOpt.get_exn @@ CCIO.with_in (P.gpio_n_active_low id) CCIO.read_line |> bool_of_string
  else raise (unexported id)

let get_pin id =
  {
    direction = (try get_direction id with _ -> `NA);
    value = get_value id;
    edge = (try get_edge id with _ -> `NA);
    active_low = get_active_low id;
  }

let set id id_f t_to_string v =
  if Bitv.get pins id
  then
    CCIO.with_out ~flags:[Open_wronly]
      (id_f id) (fun oc -> output_string oc @@ t_to_string v)
  else raise (unexported id)

let set_direction id d = set id P.gpio_n_direction show_direction d
let set_value id v = set id P.gpio_n_value show_value v
let set_edge id e = set id P.gpio_n_edge show_edge e
let set_active_low id b = set id P.gpio_n_active_low string_of_bool b

type controller = {
  base: int;
  label: string;
  ngpio: int;
}

let base t = t.base
let label t = t.label
let ngpio t = t.ngpio

let belongs t i = i >= t.base && i < t.base + t.ngpio

let of_dir dn =
  let (/) = Filename.concat in
  let base_fn = dn / "base" in
  let label_fn = dn / "label" in
  let ngpio_fn = dn / "ngpio" in
  try
    let base = CCOpt.get_exn CCIO.(with_in base_fn read_line) in
    let label = CCOpt.get_exn  CCIO.(with_in label_fn read_line) in
    let ngpio = CCOpt.get_exn CCIO.(with_in ngpio_fn read_line) in
    Some {
      base = int_of_string base;
      label;
      ngpio = int_of_string ngpio;
    }
  with _ -> None

let controllers () = Gen.filter_map of_dir
    (CCIO.File.read_dir P.gpio |>
     Gen.map (fun n -> Filename.concat P.gpio n))

let export t id =
  if not @@ belongs t id
  then failwith
      (sprintf "Impossible to export %d: should be in range [%d-%d)"
         id t.base (t.base + t.ngpio));
  CCIO.with_out ~flags:[Open_wronly]
    P.export (fun oc -> output_string oc @@ string_of_int id);
  Bitv.set pins id true

let unexport t id =
  if not @@ belongs t id
  then failwith
      (sprintf "Impossible to export %d: should be in range [%d-%d)"
         id t.base (t.base + t.ngpio));
  CCIO.with_out ~flags:[Open_wronly]
    P.unexport (fun oc -> output_string oc @@ string_of_int id);
  Bitv.set pins id false
