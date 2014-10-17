open OUnit2

let string1 = "aabcaabxaaz"
let string2 = "aa"
let string3 = "cabdabdab"

let test_prefix_match_length test_ctxt =
  let text = BatString.to_list string1 in
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

let suite =
  "suite" >:::
    [ "test_z_alg" >:: test_z_alg;
      "test_prefix_match_length" >:: test_prefix_match_length;
      "test_simple_exact_match" >:: test_simple_exact_match;
      "test_compute_big_ns" >:: test_compute_big_ns
    ]

let () =
  run_test_tt_main suite
