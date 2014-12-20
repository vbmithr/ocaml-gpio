type controller = private {
  base: int;
  label: string;
  ngpio: int;
}
(** Type of a GPIO controller handle. *)

(** Accesors *)

val base : controller -> int
val label : controller -> string
val ngpio : controller -> int

(** Obtain a GPIO controller handle and export/unexport pins. *)

val controllers : unit -> controller Gen.t
val export : controller -> int -> unit
val unexport : controller -> int -> unit

(** I/O on pins. *)

type direction = [`NA | `In | `Out] [@@deriving show]
type value = [`Low | `High] [@@deriving show]
type edge = [`NA | `None | `Rising | `Falling | `Both] [@@deriving show]

type pin = private {
  direction: direction;
  value: value;
  edge: edge;
  active_low: bool;
} [@@deriving show]

val get_pin : int -> pin
val get_direction : int -> direction
val get_value : int -> value
val get_edge : int -> edge
val get_active_low : int -> bool

val set_direction : int -> [`In | `Out] -> unit
val set_value : int -> value -> unit
val set_edge : int -> [`None | `Rising | `Falling | `Both] -> unit
val set_active_low : int -> bool -> unit

