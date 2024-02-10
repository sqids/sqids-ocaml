let () =
  match Sys.argv with
  | [| _; "encode"; ints |] ->
      let s = Sqids.make () in
      let ints = List.map int_of_string (String.split_on_char ',' ints) in
      print_endline (Sqids.encode s ints)
  | [| _; "decode"; str_id |] ->
      let s = Sqids.make () in
      print_endline
        (String.concat "," (List.map string_of_int (Sqids.decode s str_id)))
  | _ -> prerr_endline "usage: sqids-ocaml encode|decode INPUT"
