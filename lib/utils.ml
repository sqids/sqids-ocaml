module String_set = Set.Make (String)
module Char_set = Set.Make (Char)

external ext_string_length : string -> int = "%string_length"
external ext_string_unsafe_get : string -> int -> char = "%string_unsafe_get"
external ext_bytes_length : bytes -> int = "%bytes_length"
external ext_bytes_unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external ext_bytes_of_string : string -> bytes = "%bytes_of_string"

let bytes_fold_left f x a =
  let r = ref x in
  for i = 0 to ext_bytes_length a - 1 do
    r := f !r (ext_bytes_unsafe_get a i)
  done;
  !r

let string_fold_left f a x = bytes_fold_left f a (ext_bytes_of_string x)

let bytes_for_all p s =
  let n = ext_bytes_length s in
  let rec loop i =
    if i = n then true
    else if p (ext_bytes_unsafe_get s i) then loop (succ i)
    else false
  in
  loop 0

let string_for_all f s = bytes_for_all f (ext_bytes_of_string s)

let string_starts_with ~prefix s =
  let len_s = ext_string_length s and len_pre = ext_string_length prefix in
  let rec aux i =
    if i = len_pre then true
    else if ext_string_unsafe_get s i <> ext_string_unsafe_get prefix i then
      false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0

let string_ends_with ~suffix s =
  let len_s = ext_string_length s and len_suf = ext_string_length suffix in
  let diff = len_s - len_suf in
  let rec aux i =
    if i = len_suf then true
    else if ext_string_unsafe_get s (diff + i) <> ext_string_unsafe_get suffix i
    then false
    else aux (i + 1)
  in
  diff >= 0 && aux 0

let bytes_swap_inplace bytes i j =
  let tmp = Stdlib.Bytes.get bytes i in
  Stdlib.Bytes.set bytes i (Stdlib.Bytes.get bytes j);
  Stdlib.Bytes.set bytes j tmp

let bytes_rev_inplace bytes =
  let len = Bytes.length bytes in
  for i = 0 to (len / 2) - 1 do
    let tmp = Bytes.get bytes i in
    Bytes.set bytes i (Bytes.get bytes (len - 1 - i));
    Bytes.set bytes (len - 1 - i) tmp
  done

let bytes_shuffle_inplace bytes =
  let bytes_len = Stdlib.Bytes.length bytes in
  (* ocaml for loop is inclusive, subctract 1 to stop before the last value *)
  for i = 0 to bytes_len - 1 - 1 do
    let j = bytes_len - 1 - i in
    let r =
      ((i * j) + Stdlib.Bytes.get_uint8 bytes i + Stdlib.Bytes.get_uint8 bytes j)
      mod bytes_len
    in
    bytes_swap_inplace bytes i r
  done

(* TODO: maybe also do it inplace? *)
let bytes_rotate bytes offset =
  let len = Bytes.length bytes in
  let rotated_bytes = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set rotated_bytes i (Bytes.get bytes ((i + offset) mod len))
  done;
  rotated_bytes

let char_is_digit c = match c with '0' .. '9' -> true | _ -> false

let string_has_dups str =
  let module Char_set = Set.Make (Char) in
  let set =
    string_fold_left (fun set c -> Char_set.add c set) Char_set.empty str
  in
  Stdlib.String.length str <> Char_set.cardinal set

(* Partially adapted from https://github.com/dbuenzli/astring by dbuenzli licensed under ISC. *)
let string_is_infix ~affix s =
  let len_a = ext_string_length affix in
  let len_s = ext_string_length s in
  if len_a > len_s then false
  else
    let max_idx_a = len_a - 1 in
    let max_idx_s = len_s - len_a in
    let rec loop i k =
      if i > max_idx_s then false
      else if k > max_idx_a then true
      else if k > 0 then
        if ext_string_unsafe_get affix k = ext_string_unsafe_get s (i + k) then
          loop i (k + 1)
        else loop (i + 1) 0
      else if ext_string_unsafe_get affix 0 = ext_string_unsafe_get s i then
        loop i 1
      else loop (i + 1) 0
    in
    loop 0 0

let list_is_empty = function [] -> true | _ -> false
