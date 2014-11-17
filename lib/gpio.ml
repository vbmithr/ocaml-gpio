open Printf

let fail s = `Failure s

module Opt = struct
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

module List = struct
  include List
  let filter_map f l =
    List.rev @@
    List.fold_left
      (fun a e -> match f e with None -> a | Some res -> res::a)
      [] l
end

module FSUtils = struct
  let (/) = Filename.concat

  let ls dn =
    let rec get_all acc dh =
      try
        get_all ((Unix.readdir dh) :: acc) dh
      with _ ->
        Unix.closedir dh;
        List.rev acc
    in
    match Sys.is_directory dn with
    | true ->
      let dh = Unix.opendir dn in
      get_all [] dh
    | false -> []
    | exception _ -> []

  let with_ic_safe fn f =
    let ic = open_in fn in
    try
      let res = f ic in
      close_in ic; res
    with exn -> close_in ic; raise exn

  let read_file fn =
    match Sys.file_exists fn with
    | false -> None
    | true ->
      with_ic_safe fn
        (fun ic ->
           let ic_len = in_channel_length ic in
           let buf = Bytes.create ic_len in
           let nb_read = input ic buf 0 ic_len in
           if nb_read <> ic_len then None
           else Some buf
        )

  let with_fd_ro_safe fn f =
    let open Unix in
    let fd = openfile fn [O_RDONLY] 0o644 in
    try
      let res = f fd in close fd; res
    with exn -> close fd; raise exn

  let with_fd_wo_safe fn f =
    let open Unix in
    let fd = openfile fn [O_WRONLY] 0o644 in
    try
      let res = f fd in close fd; res
    with exn -> close fd; raise exn

  let with_fd_rw_safe fn f =
    let open Unix in
    let fd = openfile fn [O_RDWR] 0o644 in
    try
      let res = f fd in close fd; res
    with exn -> close fd; raise exn

  let atomic_write fn s =
    try
      let nb_written = with_fd_wo_safe fn
          (fun fd -> Unix.write fd s 0 @@ String.length s) in
      nb_written = String.length s
    with _ -> false
end

module GPIO = struct
  type t = {
    direction: [`Nonexistent | `In | `Out];
    value: [`Low | `High];
    edge: [`Nonexistent | `None | `Rising | `Falling | `Both];
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

  let install id =
    let open FSUtils in
    let dn = gpio_dn id in
    if Sys.file_exists dn then
      let direction_fn = dn / "direction" in
      let value_fn = dn / "value" in
      let edge_fn = dn / "edge" in
      let active_low_fn = dn / "active_low" in
      t.(id) <- Some {
          direction = (read_file direction_fn |>
                       function None -> `Nonexistent | Some v -> direction_of_string v);
          value = Opt.(map value_of_string @@ read_file value_fn |> run_exn);
          edge = (read_file edge_fn |>
                  function None -> `Nonexistent | Some v -> edge_of_string v);
          active_low = Opt.(map bool_of_string @@ read_file active_low_fn |> run_exn);
      }; `Ok
    else
      begin
        t.(id) <- None;
        sprintf "GPIO %d not exported or nonexistent" id |> fail
      end

  let uninstall id = t.(id) <- None

  let get_direction id = Opt.map (fun t -> t.direction) t.(id)
  let get_value id = Opt.map (fun t -> t.value) t.(id)
  let get_edge id = Opt.map (fun t -> t.edge) t.(id)
  let get_active_low id = Opt.map (fun t -> t.active_low) t.(id)

  let set_direction id d =
    match t.(id) with
    | None -> sprintf "GPIO %d is not exported" id |> fail
    | Some { direction = `Nonexistent; _ } ->
      sprintf "GPIO %d does not support changing direction" id |> fail
    | Some _ ->
      let written =
        FSUtils.atomic_write (direction_fn id) (string_of_direction d) in
      let reread = if written
        then Opt.map direction_of_string @@ FSUtils.read_file (direction_fn id)
        else None in
      match reread with
      | Some d' when d' = d -> `Ok
      | _ -> sprintf "Failed changing direction of GPIO %d" id |> fail

  let set_value id v =
    match t.(id) with
    | None ->
      sprintf "GPIO %d is not exported" id |> fail
    | Some { direction = `In; _ } ->
      sprintf "GPIO %d is an input pin" id |> fail
    | Some _ ->
      let written =
        FSUtils.atomic_write (value_fn id) (string_of_value v) in
      let reread = if written
        then Opt.map value_of_string @@ FSUtils.read_file (value_fn id)
        else None in
      match reread with
      | Some v' when v = v' -> `Ok
      | _ -> sprintf "Failed changing direction of GPIO %d" id |> fail

  let set_edge id e =
    match t.(id) with
    | None ->
      sprintf "GPIO %d is not exported" id |> fail
    | Some { edge = `Nonexistent; _ } ->
      sprintf "GPIO %d does not support changing edge" id |> fail
    | Some _ ->
      let written =
        FSUtils.atomic_write (edge_fn id) (string_of_edge e) in
      let reread = if written
        then Opt.map edge_of_string @@ FSUtils.read_file (edge_fn id)
        else None in
      match reread with
      | Some e' when e' = e -> `Ok
      | _ -> sprintf "Failed changing direction of GPIO %d" id |> fail

  let set_active_low id b =
    match t.(id) with
    | None ->
      sprintf "GPIO %d is not exported" id |> fail
    | Some _ ->
      let written =
        FSUtils.atomic_write (active_low_fn id) (string_of_bool b) in
      let reread = if written
        then Opt.map bool_of_string @@ FSUtils.read_file (active_low_fn id)
        else None in
      match reread with
      | Some _ -> `Ok
      | None -> sprintf "Failed changing direction of GPIO %d" id |> fail
end

module Controller = struct
  type t = {
    base: int;
    label: string;
    ngpio: int;
  }

  let belongs t i = i >= t.base && i < t.ngpio

  let of_dir dn =
    let open FSUtils in
    let base_fn = dn / "base" in
    let label_fn = dn / "label" in
    let ngpio_fn = dn / "ngpio" in
    match
      read_file base_fn,
      read_file label_fn,
      read_file ngpio_fn
    with
    | Some base, Some label, Some ngpio ->
      Some {
        base = int_of_string base;
        label;
        ngpio = int_of_string ngpio;
      }
    | _ -> None

  let get () =
    match Sys.is_directory "/sys/class/gpio" with
    | true ->
      List.filter_map of_dir @@ FSUtils.ls "/sys/class/gpio"
    | false -> []
    | exception _ -> []

  let export t id =
    if not @@ belongs t id
    then
      sprintf "Impossible to export %d: should be in range [%d-%d["
        id t.base t.ngpio |> fail
    else
      let id_str = string_of_int id in
      let id_str_len = String.length id_str in
      try
        FSUtils.with_fd_wo_safe "/sys/class/gpio/export"
          (fun fd ->
             if Unix.write fd id_str 0 id_str_len <> id_str_len
             then sprintf "Failed to export GPIO %d: write_error" id |> fail
             else GPIO.install id
          )
      with exn ->
        sprintf "Failed to export %d: %s" id (Printexc.to_string exn) |>
        fail

  let unexport t id =
    let id_str = string_of_int id in
    let id_str_len = String.length id_str in
    try
      FSUtils.with_fd_wo_safe "/sys/class/gpio/unexport"
        (fun fd ->
           let nb_written = Unix.write fd id_str 0 id_str_len in
           if nb_written <> id_str_len
           then
             sprintf "Failed to unexport %d (Linux side): write_error" id |>
             fail
           else
             (GPIO.uninstall id; `Ok)
        )
    with exn ->
      sprintf "Failed to unexport %d (Linux side): %s"
        id (Printexc.to_string exn) |> fail
end
