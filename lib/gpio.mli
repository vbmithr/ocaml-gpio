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

(** Obtain a GPIO controller handle and export/unexport pins *)

val controllers : unit -> controller list Lwt.t
val one_controller : unit -> controller Lwt.t
val export : controller -> int -> unit Lwt.t
val unexport : controller -> int -> unit Lwt.t

(** Functions on pins *)

type direction = [`NA | `In | `Out] [@@deriving show]
type value = [`Low | `High] [@@deriving show]
type edge = [`NA | `None | `Rising | `Falling | `Both] [@@deriving show]

type pin = private {
  direction: direction;
  value: value;
  edge: edge;
  active_low: bool;
} [@@deriving show]

val get_pin : int -> pin Lwt.t
val get_direction : int -> direction Lwt.t
val get_value : int -> value Lwt.t
val get_edge : int -> edge Lwt.t
val get_active_low : int -> bool Lwt.t

val set_direction : int -> [`In | `Out] -> unit Lwt.t
val set_value : int -> value -> unit Lwt.t
val set_edge : int -> [`None | `Rising | `Falling | `Both] -> unit Lwt.t
val set_active_low : int -> bool -> unit Lwt.t

