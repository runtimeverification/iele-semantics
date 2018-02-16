type 'a t

val create : int -> 'a t
val find : 'a t -> 'a
val getOrUpdate : 'a t -> (unit -> 'a) -> 'a
val remove : 'a t -> unit
val finalize : 'a t -> ('a -> unit) -> unit
val put : 'a t -> 'a -> unit
