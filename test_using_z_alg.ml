open OUnit2

let string1 = "aabcaabxaaz"

let test_z_alg test_ctxt =
  let pattern = string1 in
  let expected_zs = [| 0; 1; 0; 0; 3; 1; 0; 0; 2; 1; 0 |] in
  let zs = Using_z_alg.z_alg pattern in
  assert_equal expected_zs zs

let suite =
  "suite" >:::
    [ "test_z_alg" >:: test_z_alg ]

let () =
  run_test_tt_main suite
