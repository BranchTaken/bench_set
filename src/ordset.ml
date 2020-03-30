open Basis.Rudiments
open Basis

type t = (usize, Usize.cmper_witness) Ordset.t

let length = Ordset.length
let is_empty = Ordset.is_empty
let empty = Ordset.empty (module Usize)
let singleton x = Ordset.singleton (module Usize) x
let choose = Ordset.choose_hlt
let mem = Ordset.mem
let insert = Ordset.insert
let remove = Ordset.remove
let equal = Ordset.equal
let subset = Ordset.subset
let disjoint = Ordset.disjoint
let union = Ordset.union
let inter = Ordset.inter
let diff = Ordset.diff
let fold = Ordset.fold
let filter = Ordset.filter
let partition_tf = Ordset.partition_tf
