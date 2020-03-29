module type S = sig
  type t

  val length: t -> int
  val empty: t
  val singleton: int -> t
  val insert: int -> t -> t
  val remove: int -> t -> t
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val disjoint: t -> t -> bool
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val fold: init:'accum -> f:('accum -> int -> 'accum) -> t -> 'accum
  val filter: f:(int -> bool) -> t -> t
  val partition_tf: f:(int -> bool) -> t -> t * t
end
