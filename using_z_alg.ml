(*********************)
(* Utility functions *)
(*********************)
let all_indices_of e l =
  List.rev (BatArray.fold_lefti (fun acc i el -> if el = e then i :: acc else acc) [] l)

(***************)
(* Z algorithm *)
(***************)

(* k < |text| *)
let prefix_match_length text k =
  let pattern = BatList.drop k text in
  let text_prefix = BatList.take (List.length pattern) text in
  let paired_chars = List.combine text_prefix pattern in
  let matched_prefix = BatList.take_while (fun (x, y) -> x = y) paired_chars in
  List.length matched_prefix

(* so imperative it hurts *)
let z_alg s =
  let s_list = BatString.to_list s in
  let s_length = List.length s_list in
  let zs = Array.create s_length 0 in
  let l = ref 0 in
  let r = ref 0 in
  for k = 1 to s_length - 1 do
    if k > !r then
      let z_k = prefix_match_length s_list k in
      if z_k > 0 then
        zs.(k) <- z_k;
        l := k;
        r := k + z_k - 1
    else
      (* alpha := S[l..r] *)
      (* beta  := S[k..r] *)
      let k' = k - !l in
      let z_k' = zs.(k') in
      let beta_length = (!r - k) + 1 in
      if z_k' <= beta_length then
        zs.(k) <- z_k'
      else
        let z_k = if k + beta_length < s_length then
                    let alpha_length = (!r - !l) + 1 in
                    let s_dropped_alpha = BatList.drop alpha_length s_list in
                    let new_ix = (k + beta_length) - alpha_length in
                    let post_beta_length = prefix_match_length s_dropped_alpha new_ix in
                    beta_length + post_beta_length (* TODO(hammer): update r? *)
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
  let s_rev = BatString.rev s in
  let zs_s_rev = z_alg s_rev in
  BatArray.rev zs_s_rev

let compute_big_l's s =
  let n = String.length s in
  let big_l's = Array.create n 0 in
  let big_ns = compute_big_ns s in
  for j = 0 to n - 1 do
    let i = n - big_ns.(j) in
    if i < n then big_l's.(i) <- j
  done;
  big_l's

let compute_big_ls s =
  let n = String.length s in
  let big_l's = compute_big_l's s in
  let big_ls = Array.create n 0 in
  big_ls.(1) <- big_l's.(1);
  for i = 2 to n - 1 do
    big_ls.(i) <- max big_ls.(i-1) big_l's.(i)
  done;
  big_ls
