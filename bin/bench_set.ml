open Basis.Rudiments
open Basis

type b_rel_a =
  | Independent
  | Equal
  | Subset
  | Disjoint

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
      (                 "", "  fold,filter,remove}");
    ] in
    let option_width = List.fold options ~init:0 ~f:(fun w (option, _) ->
      max w (String.clength option)
    ) in
    List.iteri options ~f:(fun i (option, descr) ->
      if i > 0 then fprintf ppf "@,";
      fprintf ppf "%-*s | %s" option_width option descr
    );
    fprintf ppf "@]@,@]"

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

let b_ops =
  Set.of_list (module Op) [Equal; Subset; Disjoint; Union; Inter; Diff]

let need_b conf =
  let open Conf in
  Set.(length (inter conf.ops b_ops)) > 0

let range_of_min_max x_min x_max =
  let x_range = x_max - x_min in
  assert (x_range <= x_max + 1);
  x_range

let choose_base_range base range =
  match range with
  | 0 -> base
  | _ -> base + (Random.int range)

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
    fn set
  end in

  let bench_mem op_reps x_min x_max set = begin
    let x_range = range_of_min_max x_min x_max in
    for op_rep = 1 to op_reps do
      let x = choose_base_range x_min x_range in
      let _ = Intset.mem x set in
      ()
    done
  end in

  let rec rep conf i reps_tdelta = begin
    match i < conf.set_reps with
    | false -> reps_tdelta
    | true -> begin
        (* Insert. *)
        let rep_t0 = Unix.gettimeofday () in
        let a = bench_insert conf.insert_order conf.a_n conf.a_min conf.a_max in
        let _b = match need_b conf with
          | true ->
            bench_insert conf.insert_order conf.b_n conf.b_min conf.b_max
          | false -> Intset.empty
        in
        let rep_t0 = match Set.mem Op.Insert conf.ops with
          | true -> rep_t0
          | false -> Unix.gettimeofday ()
        in

        (* Mem. *)
        if Set.mem Op.Mem conf.ops then
          bench_mem conf.op_reps conf.a_min conf.a_max a;

        (* XXX Add ops: Equal, Subset, Disjoint, Union, Inter, Diff,
           Fold, Filter. *)

        (* Remove. *)
        if Set.mem Op.Remove conf.ops then
          bench_remove a;

        let rep_t1 = Unix.gettimeofday () in
        let rep_tdelta = Float.(rep_t1 - rep_t0) in
        let reps_tdelta' = Float.(reps_tdelta + rep_tdelta) in
        rep conf (succ i) reps_tdelta'
      end
  end in
  let tdelta = rep conf 0 0. in

  let flt_set_reps = Float.of_isize (Usize.to_isize conf.set_reps) in
  let s_per_set_rep = Float.(tdelta / flt_set_reps) in
  let ns_per_set_rep = Usize.of_isize
    Float.(to_isize (s_per_set_rep * 1_000_000_000.)) in
  Printf.printf "ns/set_rep: %u\n" ns_per_set_rep;

  let flt_op_reps = Float.of_isize (Usize.to_isize conf.op_reps) in
  let s_per_op_rep = Float.(s_per_set_rep / flt_op_reps) in
  let ns_per_op_rep = Usize.of_isize
    Float.(to_isize (s_per_op_rep * 1_000_000_000.)) in
  Printf.printf "ns/op_rep: %u\n" ns_per_op_rep;

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
