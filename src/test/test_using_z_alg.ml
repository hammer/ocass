open OUnit2

let string1 = "aabcaabxaaz" (* Section 1.3 *)
let string2 = "aa"
let string3 = "cabdabdab"
let string4 = "aabaabcaxaabaabcy"

let test_split_hd test_ctxt =
  assert_equal ("c", "abdabdab") (Using_z_alg.split_hd string3)

let test_all_indices_of test_ctxt =
  assert_equal [1; 3] (Using_z_alg.all_indices_of 1 [|0; 1; 0; 1|])

let test_prefix_match_length test_ctxt =
  let text = string1 in
  let pos = 4 in
  assert_equal 3 (Using_z_alg.prefix_match_length text pos)

let test_z_alg test_ctxt =
  let pattern = string1 in
  let expected_zs = [| 0; 1; 0; 0; 3; 1; 0; 0; 2; 1; 0 |] in
  let zs = Using_z_alg.z_alg pattern in
  assert_equal expected_zs zs

let test_simple_exact_match test_ctxt =
  let pattern = string2 in
  let text = string1 in
  assert_equal [0; 4; 8] (Using_z_alg.simple_exact_match pattern text)

let test_compute_big_ns test_ctxt =
  let big_ns = Using_z_alg.compute_big_ns string3 in
  let expected_big_ns = [| 0;0;2;0;0;5;0;0;0; |] in
  assert_equal expected_big_ns big_ns

let test_compute_big_l's test_ctxt =
  let big_l's = Using_z_alg.compute_big_l's string3 in
  let expected_big_l's = [| 0;0;0;0;5;0;0;2;0; |] in
  assert_equal expected_big_l's big_l's

let test_compute_big_ls test_ctxt =
  let big_ls = Using_z_alg.compute_big_ls string3 in
  let expected_big_ls = [| 0;0;0;0;5;5;5;5;5; |] in
  assert_equal expected_big_ls big_ls

let test_compute_rs text_ctxt =
  let rs = Using_z_alg.compute_rs string1 in
  let expected_rs_alist = [('a', 9); ('b', 6); ('c', 3); ('x', 7); ('z', 10)] in
  let expected_rs = Hashtbl.create (List.length expected_rs_alist) in
  List.iter (fun a -> Hashtbl.add expected_rs (fst a) (snd a)) expected_rs_alist;
  assert_equal (Hashtbl.length expected_rs) (Hashtbl.length rs);
  Hashtbl.iter (fun k v -> assert_equal (Hashtbl.find rs k) v) expected_rs

let suite =
  "suite" >:::
    [ "test_split_hd" >:: test_split_hd;
      "test_all_indices_of" >:: test_all_indices_of;
      "test_prefix_match_length" >:: test_prefix_match_length;
      "test_z_alg" >:: test_z_alg;
      "test_simple_exact_match" >:: test_simple_exact_match;
      "test_compute_big_ns" >:: test_compute_big_ns;
      "test_compute_big_l's" >:: test_compute_big_l's;
      "test_compute_big_ls" >:: test_compute_big_ls;
      "test_compute_rs" >:: test_compute_rs
    ]

let () =
  run_test_tt_main suite
