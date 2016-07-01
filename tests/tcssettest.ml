open OUnit

open Tcsset



let test_fixture = "Tcs Set" >::: [
	"TreeSet: empty set is empty" >:: (fun () ->
		assert_equal true (TreeSet.is_empty TreeSet.empty_def)
	);
	"TreeSet: singleton set is not empty" >:: (fun () ->
		assert_equal false (TreeSet.is_empty (TreeSet.singleton_def 42))
	)
]



let _ = run_test_tt ~verbose:true test_fixture