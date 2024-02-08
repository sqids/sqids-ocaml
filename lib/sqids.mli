type t
(** Represents the Sqids configuration. Can be created with {!val:make}. *)

(** Contains the default configuration. *)
module Defaults : sig
  val alphabet : string
  (** The default alphabet: [abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789]. *)

  val min_length : int
  (** The default minimum identifier length: 0. *)

  val blocklist : string list
  (** The default blocklist. *)
end

val make :
  ?alphabet:string -> ?min_length:int -> ?blocklist:string list -> unit -> t
(** [make ?alphabet ?min_length ?blocklist ()] creates a new Sqids configuration with the 
    provided parameters. *)

val encode : t -> int list -> string
(** [encode sqids ints] encodes a list of numbers [ints] to a string identifier. *)

val decode : t -> string -> int list
(** [decode sqids id] decodes a string identifier [id] to a list of numbers. *)
