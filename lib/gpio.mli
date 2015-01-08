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

