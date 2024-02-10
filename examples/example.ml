open Printf

let sqids = Sqids.make ~min_length:10 ()

let () =
  let id = Sqids.encode sqids [ 1; 2; 3 ] in
  printf "encode [1; 2; 3] = %S\n" id;
  let numbers = Sqids.decode sqids id in
  printf "decode %S = [%s]\n" id
    (String.concat "; " (List.map string_of_int numbers))
