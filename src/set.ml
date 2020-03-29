open Basis.Rudiments
open Basis

type t = (usize, Usize.cmper_witness) Set.t

let length = Set.length
let empty = Set.empty (module Usize)
let singleton x = Set.singleton (module Usize) x
let insert = Set.insert
let remove = Set.remove
let equal = Set.equal
let subset = Set.subset
let disjoint = Set.disjoint
let union = Set.union
let inter = Set.inter
let diff = Set.diff
let fold = Set.fold
let filter = Set.filter
let partition_tf = Set.partition_tf
