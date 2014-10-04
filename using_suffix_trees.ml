let () =
  let string_to_find = "caat" in
  let string_to_search = "atggatatagtttctttatgggacaaaaccctacaattaataaaaggtgacttaacttca" in
  let tree_to_search = Suffix_tree.Ukkonen.create string_to_search in
  let position_found = Suffix_tree.Ukkonen.substring tree_to_search string_to_find in
  Printf.printf "Found string %s at position %d in string %s\n" string_to_find position_found string_to_search
