(* http://www.sanger.ac.uk/resources/downloads/bacteria/clostridium-difficile.html *)
let embl_file = "CD630_updated_Jan2014.embl"

let process_sq_line sq_line =
  BatString.filter BatChar.is_letter sq_line

let get_sq embl_file =
  let embl_file_enum = BatFile.lines_of "CD630_updated_Jan2014.embl" in
  let sq_lines_enum = BatEnum.drop_while (fun line -> not (BatString.starts_with line "SQ")) embl_file_enum in
  BatEnum.drop 1 sq_lines_enum;
  let sq_lines_list = List.rev (BatEnum.fold (fun acc sq_line -> (process_sq_line sq_line) :: acc) [] sq_lines_enum) in
  String.concat "" sq_lines_list

let () =
  let string_to_find = "caat" in
  let string_to_search = get_sq embl_file in
  let tree_to_search = Suffix_tree.Ukkonen.create string_to_search in
  let position_found = Suffix_tree.Ukkonen.substring tree_to_search string_to_find in
  Printf.printf "Found string %s at position %d in string %s\n" string_to_find position_found string_to_search
