type t

module Defaults : sig
  val alphabet : string
  val min_length : int
  val blocklist : string list
end

val make : ?alphabet:string -> ?min_length:int -> ?blocklist:string list -> unit -> t
val encode : t -> int list -> string
val decode : t -> string -> int list
