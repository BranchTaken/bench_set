open Stdlib

module Intset = Set.Make(Int)

type t = Intset.t

let length t = Intset.cardinal t
let is_empty = Intset.is_empty
let empty = Intset.empty
let singleton = Intset.singleton
let choose = Intset.choose
let mem = Intset.mem
let insert x t = Intset.add x t
let remove x t = Intset.remove x t
let equal = Intset.equal
let subset t0 t1 = Intset.subset t1 t0
let disjoint = Intset.disjoint
let union = Intset.union
let inter = Intset.inter
let diff = Intset.diff 
let fold ~init ~f t = Intset.fold (fun x accum -> f accum x) t init
let filter ~f t = Intset.filter f t
let partition_tf ~f t = Intset.partition f t
