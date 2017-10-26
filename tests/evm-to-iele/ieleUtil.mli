val rev_string : string -> string
val be_int : Z.t -> string
val be_int_width : Z.t -> int -> string
val rlp_encode_string : string -> string
val string_of_char : char -> string

module UnionFind :
sig
  type t
  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> unit
end
