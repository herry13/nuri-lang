open OUnit2
open Domain

let test_fixture = "Domain" >:::
[
  "!-" >:: ( fun _ ->
    assert_equal ["a"; "b"] !-["a"; "b"; "c"];
    assert_equal [] !-["a"];
    assert_equal [] !-[]
  );

  "@+" >:: ( fun _ ->
    assert_equal ["a"] ([] @+ ["a"]);
    assert_equal ["a"] (["a"] @+ []);
    assert_equal ["a"; "b"] (["a"] @+ ["b"])
  );

  "@+." >:: ( fun _ ->
    assert_equal ["a"] ([] @+. "a");
    assert_equal ["a"; "b"] (["a"] @+. "b")
  );

  "@-" >:: ( fun _ ->
    assert_equal ["a"; "b"] (["a"; "b"] @- []);
    assert_equal ["b"] (["a"; "b"] @- ["a"]);
    assert_equal ["a"; "b"] (["a"; "b"] @- ["c"]);
    assert_equal ["b"] (["a"; "b"] @- ["a"; "c"]);
    assert_equal [] ([] @- ["a"])
  );

  "@<=" >:: ( fun _ ->
    assert_equal true (["a"] @<= ["a"]);
    assert_equal false (["a"; "b"] @<= ["a"]);
    assert_equal true (["a"] @<= ["a"; "b"]);
    assert_equal true (["a"; "b"] @<= ["a"; "b"]);
    assert_equal true ([] @<= []);
    assert_equal true ([] @<= ["a"; "b"]);
    assert_equal false (["a"; "b"] @<= []);
  );

  "@<" >:: ( fun _ ->
    assert_equal false (["a"] @< ["a"]);
    assert_equal false (["a"; "b"] @< ["a"]);
    assert_equal true (["a"] @< ["a"; "b"]);
    assert_equal false (["a"; "b"] @< ["a"; "b"]);
    assert_equal false ([] @< []);
    assert_equal true ([] @< ["a"; "b"]);
    assert_equal false (["a"; "b"] @< []);
  );
]

let _ = run_test_tt_main test_fixture
