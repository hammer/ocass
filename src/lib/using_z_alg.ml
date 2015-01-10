module String = struct include Sosa.Native_string end

(*********************)
(* Utility functions *)
(*********************)
let split_hd s =
  String.split_at s 1

let all_indices_of e l =
  List.rev (BatArray.fold_lefti (fun acc i el -> if el = e then i :: acc else acc) [] l)

(***************)
(* Z algorithm *)
(***************)
let prefix_match_length text k =
  let pattern = String.drop ~index:k text in
  let rec loop p t n =
    match split_hd p, split_hd t with
    | (hd1, p), (hd2, t) when hd1 = hd2 -> loop p t (n + 1)
    | _ -> n
  in
  loop pattern text 0

(* zs.(i) := max k s.t. String.sub s i (i + k) = String.sub s 0 k *)
let z_alg s =
  let s_length = String.length s in
  let zs = Array.make s_length 0 in
  let l = ref 0 in
  let r = ref 0 in
  for k = 1 to s_length - 1 do
    if k > !r then
      let z_k = prefix_match_length s k in
      if z_k > 0 then
        zs.(k) <- z_k;
        l := k;
        r := k + z_k - 1
    else
      (* alpha := String.sub s l r *)
      (* beta  := String.sub s k r *)
      let k' = k - !l in
      let z_k' = zs.(k') in
      let beta_length = (!r - k) + 1 in
      if z_k' <= beta_length then
        zs.(k) <- z_k'
      else
        let z_k =
          if k + beta_length < s_length then
            let alpha_length = (!r - !l) + 1 in
            let s_dropped_alpha = String.drop s alpha_length in
            let new_ix = (k + beta_length) - alpha_length in
            let post_beta_length = prefix_match_length s_dropped_alpha new_ix in
            l := k;
            r := k + beta_length + post_beta_length - 1;
            beta_length + post_beta_length
          else
            beta_length
        in
        zs.(k) <- z_k
  done;
  zs

(***********************************)
(* Simple exact matching algorithm *)
(***********************************)
(* Assumes "$" does not occur in pattern or text *)
let simple_exact_match pattern text =
  let n = String.length pattern in
  let s = pattern ^ "$" ^ text in
  let zs = z_alg s in
  let matches_in_s = all_indices_of n zs in
  List.map (fun x -> x - (n + 1)) matches_in_s

(*************************)
(* Boyer-Moore algorithm *)
(*************************)
let compute_big_ns s =
  let s_rev = String.rev s in
  let zs_s_rev = z_alg s_rev in
  BatArray.rev zs_s_rev

let compute_big_l's big_ns =
  let n = Array.length big_ns in
  let big_l's = Array.make n 0 in
  for j = 0 to n - 1 do
    let i = n - big_ns.(j) in
    if i < n then big_l's.(i) <- j
  done;
  big_l's

let compute_big_ls big_l's =
  let n = Array.length big_l's in
  let big_ls = Array.make n 0 in
  big_ls.(1) <- big_l's.(1);
  for i = 2 to n - 1 do
    big_ls.(i) <- max big_ls.(i-1) big_l's.(i)
  done;
  big_ls

let compute_l's big_ns =
  let n = Array.length big_ns in
  let next_l' acc i el  = (if el = i + 1 then i + 1 else (List.hd acc)) :: acc in
  Array.of_list (BatList.take n (BatArray.fold_lefti next_l' [0] big_ns))

(* TODO(hammer): implement extended bad character rule *)
let compute_rs s =
  let initial_hashtbl_size = 10 in
  let rs = Hashtbl.create initial_hashtbl_size in
  String.iteri ~f:(fun i c -> Hashtbl.replace rs c i) s;
  rs

let bm_exact_match pattern text =
  let big_ns = compute_big_ns pattern in
  let l's = compute_l's big_ns in
  let big_l's = compute_big_l's big_ns in
  let rs = compute_rs pattern in
  let n = String.length pattern in
  let m = String.length text in
  let match_indices = Res.Array.empty () in
  let k = ref (n - 1) in
  while !k <  m do
    let i = ref (n - 1) in
    let h = ref !k in
    while !i >= 0 && String.get ~index:!i pattern = String.get ~index:!h text do
      i := !i - 1;
      h := !h - 1;
    done;
    if !i = (-1) then begin
      Res.Array.add_one match_indices (!k - n + 1);
      k := !k + n - l's.(1);
    end else begin
      let good_suffix_shift =
        match () with
        | () when !i = n - 1 -> 1
        | () when big_l's.(!i + 1) > 0 -> n - big_l's.(!i + 1)
        | () -> n - l's.(!i + 1)
      in
      let mismatched_char = String.get_exn ~index:!h text in
      (* TODO(hammer): implement extended bad character rule *)
      let bad_character_shift =
        if Hashtbl.mem rs mismatched_char then
          n - 1 - Hashtbl.find rs mismatched_char
        else 0
      in
      k := !k + max good_suffix_shift bad_character_shift;
    end;
  done;
  Res.Array.to_list match_indices
