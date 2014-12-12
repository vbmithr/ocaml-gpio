module GPIO : sig
  val get_direction : int -> [`NA | `In | `Out ] Lwt.t
  val get_value : int -> [`Low | `High ] Lwt.t
  val get_edge : int -> [`NA | `None | `Rising | `Falling | `Both ] Lwt.t
  val get_active_low : int -> bool Lwt.t

  val set_direction : int -> [`In | `Out] -> unit Lwt.t
  val set_value : int -> [`Low | `High] -> unit Lwt.t
  val set_edge : int -> [ `Both | `Falling | `None | `Rising ] -> unit Lwt.t
  val set_active_low : int -> bool -> unit Lwt.t
end

module Controller : sig
  type t

  val base : t -> int
  val label : t -> string
  val ngpio : t -> int

  val get : unit -> t list Lwt.t
  val get_first : unit -> t Lwt.t
  val export : t -> int -> unit Lwt.t
  val unexport : t -> int -> unit Lwt.t
end
