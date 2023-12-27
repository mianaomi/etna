val main :
  (string * 'a Runner.property) list ->
  (string * 'a QCheck.arbitrary) list ->
  (string * 'a Crowbar.gen) list ->
  unit
