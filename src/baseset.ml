open Base

type t = (int, Int.comparator_witness) Set.t

let length = Set.length
let empty = Set.empty (module Int)
let singleton mem = Set.singleton (module Int) mem
let mem x t = Set.mem t x
let insert x t = Set.add t x
let remove x t = Set.remove t x
let equal = Set.equal
let subset t0 t1 = Set.is_subset ~of_:t0 t1
let disjoint t0 t1 = Set.(length (inter t0 t1)) = 0
let union = Set.union
let inter = Set.inter
let diff = Set.diff
let fold ~init ~f t = Set.fold t ~init ~f
let filter ~f t = Set.filter t ~f
let partition_tf ~f t = Set.partition_tf t ~f
