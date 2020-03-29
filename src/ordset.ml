open Basis.Rudiments
open Basis

type t = (usize, Usize.cmper_witness) Ordset.t

let length = Ordset.length
let empty = Ordset.empty (module Usize)
let singleton x = Ordset.singleton (module Usize) x
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