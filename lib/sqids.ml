exception Alphabet_contains_multibyte_characters of string
exception Alphabet_too_short of string
exception Alphabet_contains_repeated_characters of string
exception Minimum_length_outside_limits of string
exception Encode_max_attempts of string

module String_set = Sqids_utils.String_set

type t = { alphabet : Bytes.t; min_length : int; blocklist : String_set.t }

module Defaults = struct
  let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let min_length = 0
  let blocklist = Blocklist.blocklist
end

(* TODO: optimize *)
let to_id num alphabet =
  let alphabet_len = Bytes.length alphabet in
  let rec loop result id_chars =
    let alphabet_idx = result mod alphabet_len in
    (* from the alphabet string get the char in the position with index from the result of the previous operation *)
    let alphabet_char = Bytes.get alphabet alphabet_idx in
    (* prepend the previous char to the id_chars string *)
    let id_chars' = Bytes.cat (Bytes.make 1 alphabet_char) id_chars in
    let result' = result / alphabet_len in
    (* break loop if result' is 0 *)
    if Int.equal result' 0 then id_chars' else loop result' id_chars'
  in
  loop num Bytes.empty

let to_number id alphabet =
  String.fold_left
    (fun acc c ->
      let i = Bytes.index alphabet c in
      (acc * Bytes.length alphabet) + i)
    0 id

let is_blocked_id t ~id =
  let id = String.lowercase_ascii id in
  let id_len = String.length id in
  let exception Blocked_word in
  try
    String_set.iter
      (fun word ->
        let word_len = String.length word in
        if word_len <= id_len then
          if id_len <= 3 || word_len <= 3 then (
            if String.equal id word then raise_notrace Blocked_word
          )
          else if String.for_all Sqids_utils.char_is_digit id then (
            if String.starts_with ~prefix:word id || String.ends_with ~suffix:word id then
              raise_notrace Blocked_word
          )
          else if Sqids_utils.string_is_infix ~affix:word id then raise_notrace Blocked_word
      )
      t.blocklist;
    false
  with Blocked_word -> true

let make ?(alphabet = Defaults.alphabet) ?(min_length = Defaults.min_length)
    ?(blocklist = Defaults.blocklist) () =
  if String.length alphabet < 3 then raise (Alphabet_too_short "Alphabet length must be at least 3");

  if Sqids_utils.string_has_dups alphabet then
    raise (Alphabet_contains_repeated_characters "Alphabet must contain unique characters");

  (* min_length should be equivalent to uint8_t (0-255) *)
  (* https://github.com/sqids/sqids-spec/blob/40f407169fa0f555b93a197ff0a9e974efa9fba6/src/index.ts#L44 *)
  let min_length_limit = 255 in
  if min_length < 0 || min_length > min_length_limit then
    raise
      (Minimum_length_outside_limits
         (Printf.sprintf "Minimum length has to be between 0 and %d" min_length_limit)
      );

  String.to_seq alphabet
  |> Seq.iter (fun char ->
         if Char.code char > 127 then
           raise
             (Alphabet_contains_multibyte_characters "Alphabet cannot contain multibyte characters")
     );

  (* Convert blocklist to String_set *)
  let blocklist_set = List.to_seq blocklist |> String_set.of_seq in

  (* filter using blocklist *)
  let filtered_blocklist =
    let alphabet_lower = String.lowercase_ascii alphabet in
    String_set.filter_map
      (fun word ->
        let word = String.lowercase_ascii word in
        (* Drop word if characters in word are not in the alphabet, and
           words with less than 3 chars should be dropped *)
        if String.length word >= 3 && String.for_all (String.contains alphabet_lower) word then
          Some word
        else None
      )
      blocklist_set
  in

  let alphabet = Bytes.of_string alphabet in
  Sqids_utils.bytes_shuffle_inplace alphabet;

  { alphabet; min_length; blocklist = filtered_blocklist }

let rec encode_numbers t ~numbers ?(increment = 0) () =
  let alphabet = t.alphabet in

  let alphabet_len = Bytes.length alphabet in
  let numbers_len = List.length numbers in

  (* Fail when an increment-number of attempts has been made to re-generated the ID,
     where increment is alphabet length + 1 *)
  if increment > alphabet_len then
    raise (Encode_max_attempts "Reached max attempts to re-generate the ID");

  (* get a semi-random offset from input numbers *)
  let offset =
    List.fold_left
      (fun a (idx, v) ->
        (* Calculate the new value by taking the remainder when dividing by the length of 'alphabet' *)
        let alphabet_idx = v mod alphabet_len in
        (* Add the index 'i' and the accumulated value 'a' *)
        let alphabet_v = Bytes.get_uint8 alphabet alphabet_idx in
        alphabet_v + idx + a
      )
      (* Start with the size of the numbers list' *)
      numbers_len
      (* Enumerate over the elements of 'numbers' along with their indices *)
      (List.mapi (fun i v -> (i, v)) numbers)
  in

  (* if there is a non-zero `increment`, it's an internal attempt to re-generated the ID *)
  let offset = (offset + increment) mod alphabet_len in

  (* re-arrange alphabet so that second-half goes in front of the first-half *)
  let alphabet = Sqids_utils.bytes_rotate alphabet offset in

  (* `prefix` is the first character in the generated ID, used for randomization *)
  let prefix = Bytes.get alphabet 0 in

  (* reverse alphabet (otherwise for [0, x] `offset` and `separator` will be the same char) *)
  Sqids_utils.bytes_rev_inplace alphabet;

  (* final ID will always have the `prefix` character at the beginning *)
  (* create buffer for the resulting id with an arbitrary initial size of 4 bytes *)
  let id_buf = Buffer.create 4 in
  Buffer.add_char id_buf prefix;

  (* encode input array *)
  List.iteri
    (fun idx num ->
      (* the first character of the alphabet is going to be reserved for the `separator` *)
      let alphabet_no_sep = Bytes.sub alphabet 1 (Bytes.length alphabet - 1) in
      Buffer.add_bytes id_buf (to_id num alphabet_no_sep);

      (* if not the last number *)
      if idx < numbers_len - 1 then (
        (* separator` character is used to isolate numbers within the ID *)
        Buffer.add_bytes id_buf (Bytes.sub alphabet 0 1);

        (* shuffle on every iteration *)
        Sqids_utils.bytes_shuffle_inplace alphabet
      )
    )
    numbers;

  (* handle `minLength` requirement, if the ID is too short *)
  if t.min_length > Buffer.length id_buf then (
    (* append a separator *)
    Buffer.add_bytes id_buf (Bytes.sub alphabet 0 1);

    (* keep appending `separator` + however much alphabet is needed
       for decoding: two separators next to each other is what tells us
       the rest are junk characters *)
    while t.min_length - Buffer.length id_buf > 0 do
      Sqids_utils.bytes_shuffle_inplace alphabet;
      let slice_len = min (t.min_length - Buffer.length id_buf) (Bytes.length alphabet) in
      Buffer.add_subbytes id_buf alphabet 0 slice_len
    done
  );

  let id = Buffer.contents id_buf in

  (* if ID has a blocked word anywhere, restart with a +1 increment *)
  match is_blocked_id t ~id with
  | true -> encode_numbers t ~numbers ~increment:(increment + 1) ()
  | false -> id

let encode t (numbers : int list) : string =
  match numbers with
  (* if no numbers passed, return an empty string *)
  | [] -> ""
  (* don't allow out-of-range numbers *)
  | numbers ->
      if List.exists (fun nr -> nr < 0) numbers then
        raise
          (Invalid_argument
             (Printf.sprintf "Encoding supports numbers between 0 and %d"
                max_int));
      encode_numbers t ~numbers ()

let decode t id0 =
  (* check if id is an empty string, if so, return an empty array *)
  (* check if chars in id are in the alphabet, if not, return the empty array *)
  if String.equal id0 "" || not (String.for_all (Bytes.contains t.alphabet) id0)
  then []
  else
    (* re-arrange alphabet back into it's original form *)
    let alphabet0 =
      (* `offset` is the semi-random position that was generated during encoding *)
      let offset =
        (* first character is always the `prefix` *)
        let prefix = String.get id0 0 in
        Bytes.index t.alphabet prefix
      in
      Sqids_utils.bytes_rotate t.alphabet offset
    in
    Sqids_utils.bytes_rev_inplace alphabet0;

    (* now it's safe to remove the prefix character from ID, it's not needed anymore *)
    let id1 = String.sub id0 1 (String.length id0 - 1) in

    let rec loop id alphabet acc =
      (* stop when `id` length is 0 and return numbers *)
      if String.equal id "" then List.rev acc
      else
        let sep = Bytes.get alphabet 0 in
        (* we need the first part to the left of the separator to decode the number *)
        match String.split_on_char sep id with
        (* only run this if `chunks` length is not 0 *)
        | [] -> List.rev acc
        (* if chunk[0] is empty, we are done (the rest are junk characters) *)
        | "" :: _chunks -> List.rev acc
        | chunks_hd :: chunks_tl ->
            let acc' =
              (* decode the number without using the `separator` character *)
              let alphabet_without_sep =
                Bytes.sub alphabet 1 (Bytes.length alphabet - 1)
              in
              let n = to_number chunks_hd alphabet_without_sep in
              n :: acc
            in
            (* if this ID has multiple numbers, shuffle the alphabet because that's what encoding function did *)
            if not (List.is_empty chunks_tl) then
              Sqids_utils.bytes_shuffle_inplace alphabet;
            let id' = String.concat (String.make 1 sep) chunks_tl in
            loop id' alphabet acc'
    in
    loop id1 alphabet0 []
