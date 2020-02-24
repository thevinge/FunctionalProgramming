open QCheck
(*
let dictionary = Dict.empty

let dictionary = Dict.add dictionary "foo" 2

let test = Dict.find dictionary "foo";;

print_int test;;
*)


let myshr i = Iter.return (i/2);;
  let t = Test.make (set_shrink myshr int) (fun i -> false);;
  QCheck_runner.run_tests [t];;
