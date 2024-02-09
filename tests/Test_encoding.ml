type case = { ns : int list; id : string }

let parse line =
  if String.length line = 0 then None
  else if String.get line 0 = '#' then None
  else
    match String.split_on_char ';' line with
    | [ ""; id ] -> Some { ns = []; id }
    | [ ns; id ] ->
      let ns = List.map int_of_string (String.split_on_char ',' ns) in
      Some { ns; id }
    | [ ns; id; _err ] ->
      let ns = List.map int_of_string (String.split_on_char ',' ns) in
      Some { ns; id }
    | _ -> invalid_arg line

let print ?exn { ns; id } =
  let io = String.concat ";" [ String.concat "," (List.map string_of_int ns); id ] in
  match exn with
  | None -> print_endline io
  | Some exn -> print_endline (String.concat ";" [ io; Printexc.to_string exn ])

let test sqids case =
  try
    match case with
    | { ns; id = "" } ->
      let id' = Sqids.encode sqids ns in
      print { ns; id = id' }
    | { ns = []; id } ->
      let ns' = Sqids.decode sqids id in
      print { ns = ns'; id }
    | { ns; id } ->
      let ns' = Sqids.decode sqids id in
      let id' = Sqids.encode sqids ns in
      print { ns = ns'; id = id' }
  with exn -> print ~exn case

let rec repl sqids =
  match In_channel.input_line stdin with
  | Some line ->
    let () =
      match parse line with
      | None -> print_endline line
      | Some case -> test sqids case
    in
    repl sqids
  | None -> ()

let () =
  let sqids = Sqids.make () in
  repl sqids
