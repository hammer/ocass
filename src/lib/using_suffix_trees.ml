(* http://www.sanger.ac.uk/resources/downloads/bacteria/clostridium-difficile.html *)
let pattern = "caat"
let embl_file = "CD630_updated_Jan2014.embl"

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

let () =
  (* TODO(hammer): pick these up from program arguments *)
  search_sq pattern embl_file
