val main :
  (string * 'a Runner.property) list ->
  (string * 'a QCheck.arbitrary) list ->
  (string * 'a Crowbar.gen) list ->
  (string * 'a Runner.basegen) list ->
  unit

<<<<<<< HEAD
val etna :
  (string * 'a Runner.property) list ->
  (string * 'a QCheck.arbitrary) list ->
  (string * 'a Crowbar.gen) list ->
  (string * 'a Runner.basegen) list ->
=======
val etna_fuzz :
  (string * Runner.fuzz_property) list ->
  (string * string QCheck.arbitrary) list ->
  (string * string Crowbar.gen) list ->
  (string * string Runner.basegen) list ->
>>>>>>> upstream/afl-persistent
  unit

val timeout : int ref
