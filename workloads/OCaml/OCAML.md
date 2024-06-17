# ETNA for OCaml

Here's a step-by-step guide on how to run and design your own workloads for OCaml. For this example, I'll re-create the existing `BST` workload.

##### Prerequisites

You'll need to use `dune` to build your workloads. You'll also need to install the `etna` package to your `opam switch`:

```bash
cd workloads/OCaml/util
opam install util .
```

Also install the PBT frameworks that ETNA uses:
```bash
opam install crowbar
opam install base_quickcheck
opam install qcheck
```

If you want to evaluate AFL fuzzing, make sure to use an `opam switch` that supports `afl`. Look [here](https://opam.ocaml.org/packages/ocaml-variants/) to see how to install the OCaml variant that supports that.

##### Creating a Dune Project

First, create a new `dune` project for your workload. You can do this by:

```bash
cd workloads/OCaml/
dune init proj BST
cd BST
```

#### Necessary Files

Each workload contains four necessary files:

- `lib/impl.ml` which contains the code to test, and the mutants
- `lib/spec.ml` which contains the properties themselves
- `lib/test.ml` (which can be named anything) that describes how the generators compose, more on this later
- `bin/main.ml` which is the driver

#### `lib/impl.ml`

This contains the code you're testing, as well as the mutants to test against.

Your code probably has some central `type` that it defines. If you'd like, you can use derivers for type-based generators for QuickCheck. It'll look something like this at the simplest:


```ocaml
(* impl.ml *)
type tree =
| E
| T of tree * int * int * tree
```

Then just write the code with mutants. The syntax for this uses the standard OCaml comment with an `!` after the opening. Make sure you tag each mutant using `(*!! [name] *)` before the definition. It'll look something like this:

```ocaml
(* impl.ml *)
let rec insert (k: int) (v: int) (t: tree) =
  match t with
  | E -> T (E, k, v, E)
  | T (l, k', v', r) ->
    (*! *)
      if k < k' then T ((insert k v l), k', v', r)
      else if k' < k then T (l, k', v', (insert k v r))
      else T (l, k', v, r)
    (*!! insert_1 *)
      (*!
      T (E, k, v, E)
      *)
```
You can include any other functions you'd like in `impl.ml` if you want as well.

#### `lib/spec.ml`

This contains the properties that you'll be testing. At the top, you'll want to import your implemented functions as well as part of the `util` library:

```ocaml
(* spec.ml *)
open Impl
open Util.Runner
```

Once again, you can define any new functions or types you want.

Properties are written in `ETNA` syntax so that we can run them with any framework. Properties take in a tuple of arguments and return a `test` type. You can define a property using the `->>` operator:

```ocaml
(* spec.ml *)
let prop_InsertValid (t, k, v) =
    isBST t ->> isBST (insert k v t)
```

You can chain multiple preconditions together with the `=>>` operator:
```ocaml
(* spec.ml *)
let prop_UnionValid (t, t') =
    isBST t =>> isBST t' ->> isBST (union t t')
```

If your property doesn't require preconditions, you can just set them `true`:

```ocaml
(* spec.ml *)
let prop_TreeIdentity t =
    true ->> t = t
```

#### `lib/test.ml`

You can name this file anything. It's basically all boilerplate, so I will probably write a preprocessor that does this for you, but its kinda necessary when working with so many different frameworks.

This describes how the generators are composed when a property is actually run.

Let's say we are evaluating a `tree` generator. But our property takes in a `tree` as well as an `int`. What generator should we use for the `int`? Here's where you can decide.

You'll want to import the necessary packages, as well as your properties and implementation:

```ocaml
(* test.ml *)
open Impl
open Spec
open Util
open Runner
open QCheck
open Crowbar
```

For each test from `spec.ml`, turn it into a `tree property`. The `'a property` record has 4 fields:
```ocaml
(* Pre-defined type from ETNA code *)
type 'a property = {
  name : string;
  q : 'a QCheck.arbitrary -> string -> qtest;
  c : 'a Crowbar.gen -> string -> ctest;
  b : 'a basegen -> string -> btest;
}
```

Here's how to define each one:
- `name` can be whatever you want
- `q`
    - Write an inline function that takes one argument, lets say, **`(fun q_gen -> ...)`**. Think of this argument as the generator you're trying to evaluate.
    - Use `qbuild` to compose the other generators for your property. Let's say my property takes three arguments: a `tree` and two `int`s. Remember that the `tree` generator is the one I'm evaluating, so it's `q_gen`. The other two are `int`s, so I can use `QCheck.small_int` for example. The first argument for `qbuild` would then be **`(triple q_gen small_int small_int)`**
    - The second argument to `qbuild` is just the property we defined in `spec.ml` with `qmake` composed, so **`qmake << prop_InsertValid`**
    - We end up with **`q = (fun q_gen -> qbuild (triple q_gen QCheck.small_int QCheck.small_int) (qmake << prop_InsertValid));`**
- `c`
  - This goes the same way. Write an inline function, say **`(fun c_gen -> ...)`**
  - The first argument is the composition, except this time with `Crowbar` syntax, like **`[ c_gen; Crowbar.int8; Crowbar.int8 ]`**
  - The second argument is a little different since Crowbar doesn't use tuples. Deconstruct the tuple before calling `cmake`: **`(fun t k v -> prop_InsertValid (t, k, v) |> cmake)`**
  - Combine using `cmake` this time, you end up with **`c = (fun g -> cbuild [ g; Crowbar.int8; Crowbar.int8 ] (fun t k v -> prop_InsertValid (t, k, v) |> cmake));`**
- `b`
  - This is the same thing but for `Base_quickcheck`. Since they use modules rather than tuples, the `Core_plus` module `ETNA` defines should be handy. Compose the modules similar to how you did for `QCheck` and you'll get something like: **`b =
      (fun b_gen ->
        bbuild
          (Core_plus.triple b_gen (module Core.Int) (module Core.Int))
          (bmake << prop_InsertValid));`**

Do that for all the properties. yes this is very annoying but it shouldn't require much thinking and I will eventually (maybe) write a preprocessor that does this lol

#### Generators

Now, just write the generators you'd like to evaluate! You can put these in any files you want, though I have them all in a `Strategies` folder. You can read the documentation of whichever generator framework you're using to see how to do this.

#### **`bin/main.ml`**
Now just put it all together. Import everything:

```ocaml
(* main.ml *)
open QCheck
open Crowbar
open Util.Runner
open Util.Io
open BST.Impl
open BST.Test
open BST.QcheckType
open BST.QcheckBespoke
open BST.CrowbarType
open BST.CrowbarBespoke
open BST.BaseType
open BST.BaseBespoke
```

Write an association list of string-property pairs. The strings will be how they are referred to when we later run the `python` scripts. The properties are what were defined in `test.ml`:

```ocaml
let properties =
  [
    ("prop_InsertValid", test_prop_InsertValid);
    ("prop_DeleteValid", test_prop_DeleteValid);
    ("prop_UnionValid", test_prop_UnionValid);
  ]
```

Write an association list of your QCheck strategies:

```ocaml
let qstrategies =
  [ ("type", qcheck_type); ("bespoke", qcheck_bespoke) ]
```

As well as your Crowbar and QuickCheck ones:

```ocaml
let cstrategies =
  [ ("type", crowbar_type); ("bespoke", crowbar_bespoke) ]

let bstrategies =
  [ ("type", (module BaseType)); ("bespoke", (module BaseBespoke)) ]
```

Then just call `etna` (or `main`):
```ocaml
let () = main properties qstrategies cstrategies bstrategies
```

#### Dune Building & Dune files

Once you have your files set up, just run

```bash
dune build
```

Check out the `BST/lib/dune` file to see what the build rules there are. You should also suppress unused variable warnings since when mutants are applied, sometimes variables will go unused.

**Make sure that you can run any test with any generator from the command line now!**
Try things like:
```bash
dune exec BST -- qcheck prop_InsertInsert bespoke out
dune exec BST -- qcheck prop_InsertInsert type out
dune exec BST -- crowbar prop_InsertInsert bespoke out
dune exec BST -- crowbar prop_InsertInsert type out
dune exec BST -- afl prop_InsertInsert bespoke out
dune exec BST -- afl prop_InsertInsert type out
dune exec BST -- base prop_InsertInsert bespoke out
dune exec BST -- base prop_InsertInsert type out
```

# Running ETNA

Now that the OCaml part is done, you can evaluate the generators' performances. I use `conda` to manage my `python` packages. ETNA's `benchtool` should be installed locally:

```bash
# in the ETNA head directory
cd tool/
setuptools-conda build .
```

Now just go to the `ocaml-experiments` folder and make some small modifications to `Main.py`:

```bash
# in the ETNA head directory
cd experiments/ocaml-experiments/
vim Main.py
```


Change `WORKLOADS` to be a list with just the name of the dune project you're testing.
Change `STRATEGIES` to be a list with just the framework-generator pairs you have.

Then just run:

```bash
python Main.py
```

 You should see the `raw` folder having some files created inside of it. Once all the tasks are run, it will run the parser to convert this into a `.json` file, and finally generate the graphs for you automatically.


