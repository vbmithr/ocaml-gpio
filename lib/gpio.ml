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

module FSUtils = struct
  let (/) = Filename.concat

  let ls dn =
    let rec get_all acc dh =
      let%lwt acc = Lwt.try_bind
          (fun () -> Lwt_unix.readdir dh)
          (function
          | "." -> return acc
          | ".." -> return acc
          | s -> return @@ dn / s :: acc)
          (fun _ -> Lwt_unix.closedir dh >|= fun () -> List.rev acc)
      in
      get_all acc dh in
    match Sys.is_directory dn with
    | true -> let%lwt dh = Lwt_unix.opendir dn in get_all [] dh
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
    with exn -> close fd >>= fun () -> fail exn

let with_fd_wo_safe fn f =
  let open Lwt_unix in
  let%lwt fd = openfile fn [O_WRONLY] 0o644 in
  try%lwt
    let%lwt res = f fd in close fd >|= fun () -> res
  with exn -> close fd >>= fun () -> fail exn

let with_fd_rw_safe fn f =
  let open Lwt_unix in
  let%lwt fd = openfile fn [O_RDWR] 0o644 in
  try%lwt
    let%lwt res = f fd in close fd >|= fun () -> res
  with exn -> close fd >>= fun () -> fail exn
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

  let gpio_dn id =  "/sys/class/gpio/gpio" ^ string_of_int id
  let direction_fn id =
    "/sys/class/gpio/gpio" ^ string_of_int id ^ "/direction"
  let value_fn id =
    "/sys/class/gpio/gpio" ^ string_of_int id ^ "/value"
  let edge_fn id =
    "/sys/class/gpio/gpio" ^ string_of_int id ^ "/edge"
  let active_low_fn id =
    "/sys/class/gpio/gpio" ^ string_of_int id ^ "/active_low"

  let fail_unexported id =
    fail @@ Failure (sprintf "GPIO %d not exported or nonexistent" id)

  let install id =
    let open FSUtils in
    let dn = gpio_dn id in
    if Sys.file_exists dn then
      let direction_fn = dn / "direction" in
      let value_fn = dn / "value" in
      let edge_fn = dn / "edge" in
      let active_low_fn = dn / "active_low" in
      let%lwt direction = Lwt.try_bind
          (fun () -> read_first_line direction_fn)
          (function Some v -> return @@ direction_of_string v | None -> return `NA)
          (fun _ -> return `NA)
      and value = read_first_line value_fn >>= function
         | Some v -> return @@ value_of_string v
         | None -> fail (Failure (value_fn  ^ " does not exist"))
      and edge = Lwt.try_bind
          (fun () -> read_first_line edge_fn)
          (function Some v -> return @@ edge_of_string v | None -> return `NA)
          (fun _ -> return `NA)
      and active_low = read_first_line active_low_fn >>= function
        | Some v -> return @@ bool_of_string v
        | None -> fail (Failure (active_low_fn ^ " does not exist")) in
      t.(id) <- Some { direction; value; edge; active_low; }; return_unit
    else (t.(id) <- None; fail_unexported id)

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
      FSUtils.write_string (direction_fn id) (string_of_direction d) <&>
      FSUtils.with_fd_ro_safe (direction_fn id) Lwt_unix.wait_pollpri

  let set_value id v = match t.(id) with
    | None ->
      fail @@ Failure (sprintf "GPIO %d is not exported" id)
    | Some { direction = `In; _ } ->
      fail @@ Failure (sprintf "GPIO %d is an input pin" id)
    | Some _ ->
      FSUtils.write_string (value_fn id) (string_of_value v) <&>
      FSUtils.with_fd_ro_safe (value_fn id) Lwt_unix.wait_pollpri

  let set_edge id e = match t.(id) with
    | None ->
      fail @@ Failure (sprintf "GPIO %d is not exported" id)
    | Some { edge = `NA; _ } ->
      fail @@ Failure (sprintf "GPIO %d does not support changing edge" id)
    | Some _ ->
      FSUtils.write_string (edge_fn id) (string_of_edge e) <&>
      FSUtils.with_fd_ro_safe (edge_fn id) Lwt_unix.wait_pollpri

  let set_active_low id b = match t.(id) with
    | None ->
      fail @@ Failure (sprintf "GPIO %d is not exported" id)
    | Some _ ->
      FSUtils.write_string (active_low_fn id) (string_of_bool b) <&>
      FSUtils.with_fd_ro_safe (active_low_fn id) Lwt_unix.wait_pollpri
end

module Controller = struct
  type t = {
    base: int;
    label: string;
    ngpio: int;
    inotify: Lwt_inotify.t;
    mutable iwatches: Inotify.watch list;
  }

  let base t = t.base
  let label t = t.label
  let ngpio t = t.ngpio

  let belongs t i = i >= t.base && i < t.ngpio

  let of_dir dn =
    let open FSUtils in
    let base_fn = dn / "base" in
    let label_fn = dn / "label" in
    let ngpio_fn = dn / "ngpio" in
    let%lwt base = read_first_line base_fn in
    let%lwt label = read_first_line label_fn in
    let%lwt ngpio_fn = read_first_line ngpio_fn in
    match base, label, ngpio_fn with
    | Some base, Some label, Some ngpio ->
      let%lwt inotify = Lwt_inotify.create () in
      let%lwt iwatch = Lwt_inotify.add_watch inotify "/sys/class/gpio" [Inotify.S_Create] in
      return @@ Some {
        base = int_of_string base;
        label;
        ngpio = int_of_string ngpio;
        inotify;
        iwatches = [iwatch]
      }
    | _ -> return None

  let get () =
    match Sys.is_directory "/sys/class/gpio" with
    | true -> FSUtils.ls "/sys/class/gpio" >>= Lwt_list.filter_map of_dir
    | false -> return []
    | exception _ -> return []

  let get_first () = get () >|= List.hd

  let rec wait_exported inotify id =
    let id_str = "gpio" ^ string_of_int id in
    match%lwt Lwt_inotify.read inotify with
    | _, evtkinds, _, Some str when
        str = id_str &&
        List.mem Inotify.Create evtkinds -> return_unit
    | _ -> wait_exported inotify id

  let rec wait_unexported inotify id =
    let id_str = "gpio" ^ string_of_int id in
    match%lwt Lwt_inotify.read inotify with
    | _, evtkinds, _, Some str when
        str = id_str &&
        List.mem Inotify.Delete evtkinds -> return_unit
    | _ -> wait_unexported inotify id

  let export t id =
    if not @@ belongs t id
    then
      fail @@ Failure
      (sprintf "Impossible to export %d: should be in range [%d-%d)"  id t.base t.ngpio)
    else
      let id_str = string_of_int id in
      FSUtils.write_string "/sys/class/gpio/export" id_str >>= fun () ->
      wait_exported t.inotify id >>= fun () -> GPIO.install id

  let unexport t id =
    let id_str = string_of_int id in
    FSUtils.write_string "/sys/class/gpio/unexport" id_str >>= fun () ->
    wait_unexported t.inotify id >>= fun () -> GPIO.uninstall id
end
