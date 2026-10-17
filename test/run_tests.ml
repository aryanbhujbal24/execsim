(* Runs the property suite and the unit suites, failing if any does. *)

let () =
  let qcheck_failures = QCheck_base_runner.run_tests ~colors:true Props.tests in
  if qcheck_failures <> 0
  then (
    prerr_endline "property tests failed";
    Stdlib.exit qcheck_failures);
  Alcotest.run
    ~argv:[| "run_tests" |]
    "execsim"
    [ "order book", Units.tests; "schedules and impact", Schedule_tests.tests ]
;;
