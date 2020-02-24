open QCheck
(* Exercise 2 *)
let myshr i = match i with
    | 0 -> Iter.empty
    | _ -> Iter.append (Iter.append (Iter.return (i/10)) (Iter.return (i/2))) (Iter.return (i-1))
    
 
  let t = Test.make (set_shrink myshr int) (fun i -> false);;
  let t2 = Test.make (set_shrink myshr int) (fun i -> i < 432);;
  QCheck_runner.run_tests [t;t2];;


(* Exercise 3 *)

let t = Test.make (pair small_nat small_nat) (fun (i,j) -> i+j = 0);;
  QCheck_runner.run_tests ~verbose:true [t];;

 
