module String_set = Set.Make (String)
module Char_set = Set.Make (Char)

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
      ((i * j) + Stdlib.Bytes.get_uint8 bytes i + Stdlib.Bytes.get_uint8 bytes j) mod bytes_len
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

let char_is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false

let string_has_dups str =
  let module Char_set = Set.Make (Char) in
  let set = Stdlib.String.fold_left (fun set c -> Char_set.add c set) Char_set.empty str in
  Stdlib.String.length str <> Char_set.cardinal set

(* Partially adapted from https://github.com/dbuenzli/astring by dbuenzli licensed under ISC. *)
external length : string -> int = "%string_length"
external unsafe_get : string -> int -> char = "%string_unsafe_get"

let string_is_infix ~affix s =
  let len_a = length affix in
  let len_s = length s in
  if len_a > len_s then false
  else
    let max_idx_a = len_a - 1 in
    let max_idx_s = len_s - len_a in
    let rec loop i k =
      if i > max_idx_s then false
      else if k > max_idx_a then true
      else if k > 0 then
        if unsafe_get affix k = unsafe_get s (i + k) then loop i (k + 1) else loop (i + 1) 0
      else if unsafe_get affix 0 = unsafe_get s i then loop i 1
      else loop (i + 1) 0
    in
    loop 0 0

let list_is_empty = function [] -> true | _ -> false
