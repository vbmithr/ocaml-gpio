module GPIO : sig
  val get_direction : int -> [`Nonexistent | `In | `Out] option
  val get_value : int -> [`Low | `High] option
  val get_edge : int -> [ `Nonexistent | `None | `Rising | `Falling | `Both] option
  val get_active_low : int -> bool option

  val set_direction : int -> [`In | `Out] -> [`Ok | `Failure of string]
  val set_value : int -> [`Low | `High] -> [`Ok | `Failure of string]
  val set_edge : int -> [ `Both | `Falling | `None | `Rising ] -> [`Ok | `Failure of string]
  val set_active_low : int -> bool -> [`Ok | `Failure of string]
end

module Controller : sig
  type t = private {
    base: int;
    label: string;
    ngpio: int;
  }

  val get : unit -> t list
  val export : t -> int -> [`Ok | `Failure of string]
  val unexport : t -> int -> [`Ok | `Failure of string]
end
