open Basis.Rudiments
open Basis

type b_rel_a =
  | Independent
  | Equal
  | Subset
  | Disjoint

let b_rel_a_subset = function
  | Subset -> true
  | _ -> false

type order =
  | Incr
  | Decr
  | Rand

module Op = struct
  module T = struct
    type t =
      | Mem
      | Insert
      | Remove
      | Equal
      | Subset
      | Disjoint
      | Union
      | Inter
      | Diff
      | Fold
      | Filter
      | Partition

    let to_usize = function
      | Mem -> 0
      | Insert -> 1
      | Remove -> 2
      | Equal -> 3
      | Subset -> 4
      | Disjoint -> 5
      | Union -> 6
      | Inter -> 7
      | Diff -> 8
      | Fold -> 9
      | Filter -> 10
      | Partition -> 11

    let of_string = function
      | "mem" -> Mem
      | "insert" -> Insert
      | "remove" -> Remove
      | "equal" -> Equal
      | "subset" -> Subset
      | "disjoint" -> Disjoint
      | "union" -> Union
      | "inter" -> Inter
      | "diff" -> Diff
      | "fold" -> Fold
      | "filter" -> Filter
      | "partition" -> Partition
      | _ -> halt "Invalid op"

    let to_string = function
      | Mem -> "mem"
      | Insert -> "insert"
      | Remove -> "remove"
      | Equal -> "equal"
      | Subset -> "subset"
      | Disjoint -> "disjoint"
      | Union -> "union"
      | Inter -> "inter"
      | Diff -> "diff"
      | Fold -> "fold"
      | Filter -> "filter"
      | Partition -> "partition"

    let hash_fold t state =
      Usize.hash_fold (to_usize t) state

    let cmp t0 t1 =
      Usize.cmp (to_usize t0) (to_usize t1)

    let pp ppf t =
      String.pp ppf (to_string t)
  end
  include T
  include Cmper.Make_mono(T)
end

module Conf = struct
  type t = {
    impl: (module Bench.Intset_intf.S);
    seed: usize;

    a_n: usize;
    a_min: usize;
    a_max: usize;

    b_n: usize;
    b_min: usize;
    b_max: usize;

    b_rel_a: b_rel_a;

    set_reps: usize;
    op_reps: usize;

    insert_order: order;
    pivot: usize;
    filter: Cmp.t;

    ops: (Op.t, Op.cmper_witness) Set.t;
  }

  (* Maximum value supported by Random.int . *)
  let x_max = 0x3fff_fffe

  let defaults = {
    impl=(module Bench.Ordset);
    seed=U128.to_usize (Entropy.seed);

    a_n=0;
    a_min=0;
    a_max=x_max;

    b_n=0;
    b_min=0;
    b_max=x_max;

    b_rel_a=Independent;

    set_reps=1;
    op_reps=1;

    insert_order=Rand;
    pivot=x_max / 2;
    filter=Cmp.Lt;

    ops=Set.empty (module Op);
  }

  let pp_usage ppf =
    let open Format in
    fprintf ppf "@[<v>Usage: bench_set <options>*@,@,";
    fprintf ppf "Options: @[<v>";
    let options = [
      ("-h", "Print usage.");
      ("-impl {stdlib,base,set,ordset}", "Implementation (*ordset)");
      ("-seed <seed>", "Set PRNG seed");
      ("-a_n <usize>", "Size of set A (*0)");
      ("-a_min <usize>", "Minimum key in A (*0)");
      ("-a_max <usize>", "Maximum key in A (*2^30 - 2)");
      ("-b_n <usize>", "Size of set B (*0)");
      ("-b_min <usize>", "Minimum key in B (*0)");
      ("-b_max <usize>", "Maximum key in B (*2^30 - 2)");
      ("-b_rel_a {independent,equal,", "B relation to A (*independent)");
      ("          subset,disjoint}", "");
      ("-set_reps <usize>", "Number of set rep (*1)");
      ("-op_reps <usize>", "Per set number of operation reps (*1)");
      ("-insert_order {incr,decr,rand}", "Insertion order (*rand)");
      ("-pivot <usize>", "Filter pivot (*2^29 - 1)");
      ("-filter {lt,gt}", "Filter (key {lt,gt} pivot) (lt*)");
      ("-ops <op>[,<op>]*", "Operation(s) in {mem,insert,remove,");
      (                 "", "  equal,subset,disjoint,");
      (                 "", "  union,inter,diff,");
      (                 "", "  fold,filter,partition}");
    ] in
    let option_width = List.fold options ~init:0 ~f:(fun w (option, _) ->
      max w (String.clength option)
    ) in
    List.iteri options ~f:(fun i (option, descr) ->
      if i > 0 then fprintf ppf "@,";
      fprintf ppf "%-*s | %s" option_width option descr
    );
    fprintf ppf "@]@,@]"

  let b_ops =
    Set.of_list (module Op) [Equal; Subset; Disjoint; Union; Inter; Diff]

  let need_b t =
    Set.(length (inter t.ops b_ops)) > 0

  let validate t =
    let invalid msg = begin
      Format.eprintf "@[<h>Invalid configuration: %s@\n@]" msg;
      pp_usage Format.err_formatter;
      exit 1
    end in
    if t.a_max > (Usize.of_isize Isize.max_value) then
      invalid "a_max > Isize.max_value";
    if t.b_max > (Usize.of_isize Isize.max_value) then
      invalid "b_max > Isize.max_value";
    if t.a_min > t.a_max then invalid "a_min > a_max";
    if t.b_min > t.b_max then invalid "b_min > b_max";
    if t.a_n > 0 && t.a_n - 1 > t.a_max - t.a_min then
      invalid "a_n > 0 && a_n - 1 > a_max - a_min";
    if t.b_n > 0 && t.b_n - 1 > t.b_max - t.b_min then
      invalid "b_n > 0 && b_n - 1 > b_max - b_min";
    if need_b t && b_rel_a_subset t.b_rel_a && t.b_n > t.a_n then
      invalid "need_b && && b_rel_a = Subset && b_n > a_n";
    t

  let of_argv argv =
    let rec fn t = function
      | [] -> t
      | "-h" :: _ -> pp_usage Format.std_formatter; exit 0

      | "-impl" :: "stdlib" :: tl ->
        fn {t with impl=(module Bench.Stdlibset)} tl
      | "-impl" :: "base" :: tl -> fn {t with impl=(module Bench.Baseset)} tl
      | "-impl" :: "set" :: tl -> fn {t with impl=(module Bench.Set)} tl
      | "-impl" :: "ordset" :: tl -> fn {t with impl=(module Bench.Ordset)} tl

      | "-seed" :: s :: tl -> fn {t with seed=Usize.of_string s} tl

      | "-a_n" :: a_n :: tl -> fn {t with a_n=Usize.of_string a_n} tl
      | "-a_min" :: a_min :: tl -> fn {t with a_min=Usize.of_string a_min} tl
      | "-a_max" :: a_max :: tl -> fn {t with a_max=Usize.of_string a_max} tl

      | "-b_n" :: b_n :: tl -> fn {t with b_n=Usize.of_string b_n} tl
      | "-b_min" :: b_min :: tl -> fn {t with b_min=Usize.of_string b_min} tl
      | "-b_max" :: b_max :: tl -> fn {t with b_max=Usize.of_string b_max} tl

      | "-b_rel_a" :: "independent" :: tl -> fn {t with b_rel_a=Independent} tl
      | "-b_rel_a" :: "equal" :: tl -> fn {t with b_rel_a=Equal} tl
      | "-b_rel_a" :: "subset" :: tl -> fn {t with b_rel_a=Subset} tl
      | "-b_rel_a" :: "disjoint" :: tl -> fn {t with b_rel_a=Disjoint} tl

      | "-set_reps" :: set_reps :: tl ->
        fn {t with set_reps=Usize.of_string set_reps} tl
      | "-op_reps" :: op_reps :: tl ->
        fn {t with op_reps=Usize.of_string op_reps} tl

      | "-insert_order" :: "incr" :: tl -> fn {t with insert_order=Incr} tl
      | "-insert_order" :: "decr" :: tl -> fn {t with insert_order=Decr} tl
      | "-insert_order" :: "rand" :: tl -> fn {t with insert_order=Rand} tl

      | "-pivot" :: pivot :: tl -> fn {t with pivot=Usize.of_string pivot} tl

      | "-filter" :: "lt" :: tl -> fn {t with filter=Cmp.Lt} tl
      | "-filter" :: "gt" :: tl -> fn {t with filter=Cmp.Gt} tl

      | "-ops" :: ops :: tl -> begin
          let rec fn_ops t = function
            | [] -> t
            | op_str :: ops' -> begin
                let op = Op.of_string op_str in
                fn_ops {t with ops=Set.insert op t.ops} ops'
              end
          in
          let ops_strs =
            String.split ops ~f:(fun c -> Codepoint.(c = of_char ',')) in
          fn (fn_ops t ops_strs) tl
        end
      | args -> begin
          Format.eprintf "@[<h>Unhandled argument%s: %a@\n@]"
            (if List.length args <> 1 then "s" else "")
            (List.pp String.pp) args;
          pp_usage Format.err_formatter;
          exit 1
        end
    in
    let args = List.tl (Array.to_list argv) in (* Drop program name. *)
    validate (fn defaults args)
end

let init_random seed =
  Random.init seed

let range_of_min_max x_min x_max =
  let x_range = x_max - x_min in
  assert (x_range <= x_max + 1);
  x_range

let choose_base_range base range =
  match range with
  | 0 -> base
  | _ -> base + (Random.int range)

let str_of_ns ns =
  let str = Usize.to_string ns in
  let len = String.clength str in
  String.of_list_rev (String.foldi
    str ~init:[] ~f:(fun i accum cp ->
      match i <> 0 && ((len - i) % 3 = 0) with
      | true -> cp :: (Codepoint.of_char '_') :: accum
      | false -> cp :: accum
    )
  )

let bench conf =
  let open Conf in
  let module Intset = (val conf.impl: Bench.Intset_intf.S) in

  let bench_insert order n x_min x_max = begin
    let rec fn_incr n i x_min set = begin
      match i < n with
      | false -> set
      | true -> begin
          let set' = Intset.insert (x_min + i) set in
          fn_incr n (succ i) x_min set'
        end
    end in
    let rec fn_decr n i x_max set = begin
      match i < n with
      | false -> set
      | true -> begin
          let set' = Intset.insert (x_max - i) set in
          fn_decr n (succ i) x_max set'
        end
    end in
    let rec fn_rand n x_min x_range set = begin
      match n with
      | 0 -> set
      | _ -> begin
          let x = choose_base_range x_min x_range in
          let set', n' = match Intset.mem x set with
            | true -> set, n
            | false -> (Intset.insert x set), (pred n)
          in
          fn_rand n' x_min x_range set'
        end
    end in
    match order with
    | Incr -> fn_incr n 0 x_min Intset.empty
    | Decr -> fn_decr n 0 x_max Intset.empty
    | Rand -> begin
        let x_range = range_of_min_max x_min x_max in
        match x_range with
        | 0 -> Intset.singleton x_min
        | _ -> fn_rand n x_min (succ x_range) Intset.empty
      end
  end in

  let b_equal a = begin
    Intset.fold a ~init:Intset.empty ~f:(fun b m -> Intset.insert m b)
  end in

  let b_subset b_n a a_n = begin
    (* This method for sampling has the side effect that its insertion order
       is stable, whereas shuffling-based sampling would allow random insertion
       order. *)
    let b, need, _ = Intset.fold a ~init:(Intset.empty, b_n, a_n)
      ~f:(fun (b, need, have) m ->
        let flt_need = Float.of_isize (Usize.to_isize need) in
        let flt_have = Float.of_isize (Usize.to_isize have) in
        let p = Float.(flt_need / flt_have) in
        let probe = Random.float 1. in
        match Float.(probe <= p) with
        | true -> (Intset.insert m b), (pred need), (pred have)
        | false -> b, need, (pred have)
      )
    in
    assert (need = 0);
    b
  end in

  let b_disjoint b_n b_min b_max a = begin
    let rec fn need b_min b_range a b = begin
      match need > 0 with
      | false -> b
      | true -> begin
          let x = choose_base_range b_min b_range in
          let b', need' = match (Intset.mem x a) || (Intset.mem x b) with
            | true -> b, need
            | false -> (Intset.insert x b), (pred need)
          in
          fn need' b_min b_range a b'
        end
    end in
    let b_range = range_of_min_max b_min b_max in
    fn b_n b_min b_range a Intset.empty
  end in

  let bench_remove set = begin
    let rec fn set = begin
      match Intset.is_empty set with
      | true -> ()
      | false -> begin
          let m = Intset.choose set in
          let set' = Intset.remove m set in
          fn set'
        end
    end in
    Sys.opaque_identity (fn set)
  end in

  let bench_mem op_reps x_min x_max set = begin
    let x_range = range_of_min_max x_min x_max in
    for _op_rep = 1 to op_reps do
      let x = choose_base_range x_min x_range in
      ignore (Sys.opaque_identity (Intset.mem x set))
    done
  end in

  let bench_equal op_reps a b = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.equal a b))
    done
  end in

  let bench_subset op_reps a b = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.subset a b))
    done
  end in

  let bench_disjoint op_reps a b = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.disjoint a b))
    done
  end in

  let bench_union op_reps a b = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.union a b))
    done
  end in

  let bench_inter op_reps a b = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.inter a b))
    done
  end in

  let bench_diff op_reps a b = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.diff a b))
    done
  end in

  let bench_fold op_reps set = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.fold set ~init:() ~f:(fun _ _ -> ())))
    done
  end in

  let bench_filter op_reps pivot filter set = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.filter set ~f:(fun x ->
        let open Cmp in
        match filter with
        | Lt -> x < pivot
        | Gt -> x > pivot
        | Eq -> not_reached ()
      )))
    done
  end in

  let bench_partition op_reps pivot filter set = begin
    for _op_rep = 1 to op_reps do
      ignore (Sys.opaque_identity (Intset.partition_tf set ~f:(fun x ->
        let open Cmp in
        match filter with
        | Lt -> x < pivot
        | Gt -> x > pivot
        | Eq -> not_reached ()
      )))
    done
  end in

  let rec rep conf i reps_tsum = begin
    match i < conf.set_reps with
    | false -> reps_tsum
    | true -> begin
        (* Insert. *)
        let rep_t0 = Unix.gettimeofday () in
        let a = bench_insert conf.insert_order conf.a_n conf.a_min conf.a_max in
        let b = match Conf.need_b conf with
          | true -> begin
              match conf.b_rel_a with
              | Independent ->
                bench_insert conf.insert_order conf.b_n conf.b_min conf.b_max
              | Equal -> b_equal a
              | Subset -> b_subset conf.b_n a conf.a_n
              | Disjoint -> b_disjoint conf.b_n conf.b_min conf.b_max a
            end
          | false -> Intset.empty
        in
        let rep_t0 = match Set.mem Op.Insert conf.ops with
          | true -> rep_t0
          | false -> Unix.gettimeofday ()
        in

        (* Mem. *)
        if Set.mem Op.Mem conf.ops then
          bench_mem conf.op_reps conf.a_min conf.a_max a;

        (* Equal. *)
        if Set.mem Op.Equal conf.ops then
          bench_equal conf.op_reps a b;

        (* Subset. *)
        if Set.mem Op.Subset conf.ops then
          bench_subset conf.op_reps a b;

        (* Disjoint. *)
        if Set.mem Op.Disjoint conf.ops then
          bench_disjoint conf.op_reps a b;

        (* Union. *)
        if Set.mem Op.Union conf.ops then
          bench_union conf.op_reps a b;

        (* Inter. *)
        if Set.mem Op.Inter conf.ops then
          bench_inter conf.op_reps a b;

        (* Diff. *)
        if Set.mem Op.Diff conf.ops then
          bench_diff conf.op_reps a b;

        (* Fold. *)
        if Set.mem Op.Fold conf.ops then
          bench_fold conf.op_reps a;

        (* Filter. *)
        if Set.mem Op.Filter conf.ops then
          bench_filter conf.op_reps conf.pivot conf.filter a;

        (* Partition. *)
        if Set.mem Op.Partition conf.ops then
          bench_partition conf.op_reps conf.pivot conf.filter a;

        (* Remove. *)
        if Set.mem Op.Remove conf.ops then
          bench_remove a;

        let rep_t1 = Unix.gettimeofday () in
        let rep_tdelta = Float.(rep_t1 - rep_t0) in
        let reps_tsum' = Float.(reps_tsum + rep_tdelta) in
        rep conf (succ i) reps_tsum'
      end
  end in
  let tsum = rep conf 0 0. in

  let flt_set_reps = Float.of_isize (Usize.to_isize conf.set_reps) in
  let s_per_set_rep = Float.(tsum / flt_set_reps) in
  let flt_op_reps = Float.of_isize (Usize.to_isize conf.op_reps) in
  let s_per_op_rep = Float.(s_per_set_rep / flt_op_reps) in
  let ns_per_op_rep = Usize.of_isize
    Float.(to_isize (s_per_op_rep * 1_000_000_000.)) in
  Printf.printf "ns/op_rep: %s %u\n" (str_of_ns ns_per_op_rep) ns_per_op_rep;
  ()

let main () =
  try
    Printexc.record_backtrace true;
    let conf = Conf.of_argv Sys.argv in
    init_random conf.seed;
    bench conf;
    0
  with
    e -> begin
      Format.eprintf "@[==> Error: %s@\n%s@\n@]"
        (Printexc.to_string e) (Printexc.get_backtrace ());
      exit 1
    end

let () = exit (main ())
