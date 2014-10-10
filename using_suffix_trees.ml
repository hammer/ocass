(* http://www.sanger.ac.uk/resources/downloads/bacteria/clostridium-difficile.html *)
let embl_file = "CD630_updated_Jan2014.embl"

type ix_info =
  { z : int;
    l : int;
    r : int;
  }

let get_sq embl_file =
  let embl_file_enum = BatFile.lines_of "CD630_updated_Jan2014.embl" in
  let sq_guard = fun line -> not (BatString.starts_with line "SQ") in
  let sq_lines_enum = BatEnum.skip 1 (BatEnum.drop_while sq_guard embl_file_enum) in
  let process_sq_line sq_line = BatString.filter BatChar.is_letter sq_line in
  let fold_sq_line = fun acc sq_line -> (process_sq_line sq_line) :: acc in
  let sq_lines_list = List.rev (BatEnum.fold fold_sq_line [] sq_lines_enum) in
  String.concat "" sq_lines_list

let search_sq pattern embl_file =
  let text = get_sq embl_file in
  let tree_to_search = Suffix_tree.Ukkonen.create text in
  let position_found = Suffix_tree.Ukkonen.substring tree_to_search pattern in
  Printf.printf "Found pattern %s at position %d in text %s\n" pattern position_found text

(* |t| <= |s| *)
let prefix_match_length s t =
  let st = List.combine (BatList.take (List.length t) s) t in
  let matched_prefix = BatList.take_while (fun (x, y) -> x = y) st in
  List.length matched_prefix

let z_alg_init s =
  let s_to_match = BatList.drop 1 s in
  let z_2 = prefix_match_length s s_to_match in
  [{ z = z_2; l = 1; r = 1 + z_2 - (min z_2 1) }]

let rec z_alg_rec s ix_infos =
  let s_length = List.length s in
  let ix = List.length ix_infos + 1 in
  match ix with
  | 1 -> z_alg_rec s (z_alg_init s)
  | k when ix = s_length -> ix_infos
  | k when ix > (List.hd ix_infos).r ->
     let z_k = prefix_match_length s (BatList.drop k s) in
     if z_k > 0 then
       let r_k = k + z_k - 1 in
       let l_k = z_k in
       z_alg_rec s ({ z = z_k; l = l_k; r = r_k } :: ix_infos)
     else
       let last_ix_info = List.hd ix_infos in
       z_alg_rec s ({ z = z_k; l = last_ix_info.l; r = last_ix_info.r } :: ix_infos)
  | k -> [] (* TODO(hammer): implement *)

let print_ix_info ix ix_info =
  Printf.printf "(z_%d:%d, l_%d:%d, r_%d:%d)\n" ix ix_info.z ix ix_info.l ix ix_info.r

let () =
  let pattern = "ccc" in
  let ix_infos = z_alg_rec (BatString.to_list pattern) [] in
  List.iteri print_ix_info ix_infos


