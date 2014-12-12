open Lwt
open Printf

module Opt = struct
  let return v = Some v

  let run_exn = function
    | Some v -> v
    | None -> invalid_arg "run_exn"

  let default d = function
    | Some v -> v
    | None -> d

  let (>>=) v f = match v with
    | Some v -> f v
    | None -> None

  let map f = function
    | Some v -> Some (f v)
    | None -> None
end

module Lwt_list = struct
  include Lwt_list
  let filter_map f l =
    Lwt_list.fold_left_s
      (fun a e -> match%lwt f e with None -> return a | Some res -> return @@ res::a)
      [] l >|= List.rev
end

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

module FSUtils = struct
  let (/) = Filename.concat
  let ls dn =
    let rec get_all acc dh = Lwt.try_bind
        (fun () -> Lwt_unix.readdir dh)
        (function
          | "."  -> get_all acc dh
          | ".." -> get_all acc dh
          | s    -> get_all (dn / s :: acc) dh)
        (fun _ -> Lwt_unix.closedir dh >|= fun () -> List.rev acc) in
    match Sys.is_directory dn with
    | true -> Lwt_unix.opendir dn >>= get_all []
    | false -> return []
    | exception _ -> return []

  let read_first_line fn =
    Lwt_io.(with_file fn ~mode:Input (fun ic -> read_line_opt ic))

  let write_string fn s =
    Lwt_io.(with_file ~mode:Output fn (fun oc -> write oc s))

  let with_fd_ro_safe fn (f : Lwt_unix.file_descr -> 'a Lwt.t) : 'a Lwt.t =
    let open Lwt_unix in
    let%lwt fd = openfile fn [O_RDONLY] 0o644 in
    try%lwt
      let%lwt res = f fd in close fd >|= fun () -> res
    with exn -> close fd >> fail exn

  let with_fd_wo_safe fn f =
    let open Lwt_unix in
    let%lwt fd = openfile fn [O_WRONLY] 0o644 in
    try%lwt
      let%lwt res = f fd in close fd >|= fun () -> res
    with exn -> close fd >> fail exn

  let with_fd_rw_safe fn f =
    let open Lwt_unix in
    let%lwt fd = openfile fn [O_RDWR] 0o644 in
    try%lwt
      let%lwt res = f fd in close fd >|= fun () -> res
    with exn -> close fd >> fail exn
end

module GPIO = struct
  type direction = [`NA | `In | `Out]
  type value = [`Low | `High]
  type edge = [`NA | `None | `Rising | `Falling | `Both]

  type t = {
    direction: direction;
    value: value;
    edge: edge;
    active_low: bool;
  }

  let t = Array.make 4096 None

  let direction_of_string = function
    | "in" -> `In
    | "out" -> `Out
    | _ -> invalid_arg "direction_of_string"

  let string_of_direction = function
    | `In -> "in" | `Out -> "out"

  let value_of_string = function
    | "0" -> `Low
    | "1" -> `High
    | _ -> invalid_arg "value_of_string"

  let string_of_value = function
    | `Low -> "0" | `High -> "1"

  let edge_of_string = function
    | "none" -> `None
    | "rising" -> `Rising
    | "falling" -> `Falling
    | "both" -> `Both
    | _ -> invalid_arg "edge_of_string"

  let string_of_edge = function
    | `None -> "none"
    | `Rising -> "rising"
    | `Falling -> "falling"
    | `Both -> "both"

  let bool_of_string = function
    | "0" -> false
    | "1" -> true
    | _ -> invalid_arg "bool_of_string"

  let string_of_bool = function
    | false -> "0"
    | true -> "1"

  let fail_unexported id =
    fail @@ Failure (sprintf "GPIO %d not exported or nonexistent" id)

  let install id =
    let open FSUtils in
    match Sys.is_directory @@ P.gpio_n id with
    | true -> begin
        let%lwt direction = Lwt.try_bind
            (fun () -> read_first_line @@ P.gpio_n_direction id)
            (function Some v -> return @@ direction_of_string v | None -> return `NA)
            (fun _ -> return `NA)
        and value = read_first_line @@ P.gpio_n_value id >>= function
          | Some v -> return @@ value_of_string v
          | None -> fail @@ Failure (P.gpio_n_value id  ^ " does not exist")
        and edge = Lwt.try_bind
            (fun () -> read_first_line @@ P.gpio_n_edge id)
            (function Some v -> return @@ edge_of_string v | None -> return `NA)
            (fun _ -> return `NA)
        and active_low = read_first_line @@ P.gpio_n_active_low id >>= function
          | Some v -> return @@ bool_of_string v
          | None -> fail (Failure (P.gpio_n_active_low id ^ " does not exist")) in
        t.(id) <- Some { direction; value; edge; active_low; }; return_unit end
    | false -> (t.(id) <- None; fail_unexported id)
    | exception _ -> (t.(id) <- None; fail_unexported id)

  let uninstall id = t.(id) <- None; return_unit

  let get_direction id = match t.(id) with
    | None -> fail_unexported id
    | Some t -> return t.direction

  let get_value id = match t.(id) with
    | None -> fail_unexported id
    | Some t -> return t.value

  let get_edge id = match t.(id) with
    | None -> fail_unexported id
    | Some t -> return t.edge

 let get_active_low id = match t.(id) with
    | None -> fail_unexported id
    | Some t -> return t.active_low

  let set_direction id d = match t.(id) with
    | None -> fail_unexported id
    | Some { direction = `NA; _ } ->
      fail @@ Failure (sprintf "GPIO %d does not support changing direction" id)
    | Some _ ->
      FSUtils.write_string (P.gpio_n_direction id) (string_of_direction d) <&>
      FSUtils.with_fd_ro_safe (P.gpio_n_direction id) Lwt_unix.wait_pollpri

  let set_value id v = match t.(id) with
    | None ->
      fail @@ Failure (sprintf "GPIO %d is not exported" id)
    | Some { direction = `In; _ } ->
      fail @@ Failure (sprintf "GPIO %d is an input pin" id)
    | Some _ ->
      FSUtils.write_string (P.gpio_n_value id) (string_of_value v) <&>
      FSUtils.with_fd_ro_safe (P.gpio_n_value id) Lwt_unix.wait_pollpri

  let set_edge id e = match t.(id) with
    | None ->
      fail @@ Failure (sprintf "GPIO %d is not exported" id)
    | Some { edge = `NA; _ } ->
      fail @@ Failure (sprintf "GPIO %d does not support changing edge" id)
    | Some _ ->
      FSUtils.write_string (P.gpio_n_edge id) (string_of_edge e) <&>
      FSUtils.with_fd_ro_safe (P.gpio_n_edge id) Lwt_unix.wait_pollpri

  let set_active_low id b = match t.(id) with
    | None ->
      fail @@ Failure (sprintf "GPIO %d is not exported" id)
    | Some _ ->
      FSUtils.write_string (P.gpio_n_active_low id) (string_of_bool b) <&>
      FSUtils.with_fd_ro_safe (P.gpio_n_active_low id) Lwt_unix.wait_pollpri
end

module Controller = struct
  type t = {
    base: int;
    label: string;
    ngpio: int;
  }

  let base t = t.base
  let label t = t.label
  let ngpio t = t.ngpio

  let belongs t i = i >= t.base && i < t.base + t.ngpio

  let of_dir dn =
    let open FSUtils in
    let base_fn = dn / "base" in
    let label_fn = dn / "label" in
    let ngpio_fn = dn / "ngpio" in
    try%lwt
      let%lwt base = read_first_line base_fn >|= Opt.run_exn in
      let%lwt label = read_first_line label_fn >|= Opt.run_exn in
      let%lwt ngpio = read_first_line ngpio_fn >|= Opt.run_exn in
      return @@ Some {
        base = int_of_string base;
        label;
        ngpio = int_of_string ngpio;
      }
    with _ -> return None

  let get () =
    match Sys.is_directory P.gpio with
    | true -> FSUtils.ls P.gpio >>= Lwt_list.filter_map of_dir
    | false -> return []
    | exception _ -> return []

  let get_first () = get () >|= List.hd

  let export t id =
    if not @@ belongs t id
    then
      fail @@ Failure
        (sprintf "Impossible to export %d: should be in range [%d-%d)"
           id t.base (t.base + t.ngpio))
    else
      let id_str = string_of_int id in
      FSUtils.write_string P.export id_str >> GPIO.install id

  let unexport t id =
    if not @@ belongs t id
    then
      fail @@ Failure
        (sprintf "Impossible to unexport %d: should be in range [%d-%d)"
           id t.base (t.base + t.ngpio))
    else
      let id_str = string_of_int id in
      FSUtils.write_string P.unexport id_str >> GPIO.uninstall id
end
