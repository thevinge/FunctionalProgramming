open QCheck

let first tuple = match tuple with
    | (x,y) -> x;;

let second tuple = match tuple with
    | (x,y) -> y;;


let rec sum l = match l with
    | [] -> 0
    | elem::elems -> elem + sum(elems) 

let rec member stack element = match stack with
    | [] -> false
    | elem::elems -> if elem = element
                     then true
                     else member elems element

let sum_concat_test =
  Test.make ~name:"Split and Sum" ~count: 1000
    (pair (list int) (list int))
    (fun (xs,ys) ->  sum(xs @ ys) = (sum ys) + (sum xs));;

let _ = QCheck_runner.run_tests ~verbose:true  [sum_concat_test;]


let rec merge_sort lst1 lst2 = match lst1, lst2 with
    | [],list -> lst2
    | list,[] -> lst1
    | elem1::elems1, elem2::elems2 -> if elem1 <= elem2 
                                then elem1:: merge_sort elems1 (elem2::elems2)
                                else elem2:: merge_sort elems2 (elem1::elems1)


let sort_concat_test =
  Test.make ~name:"Split and sort" ~count: 1000
    (pair (list int) (list int))
    (fun (xs,ys) ->  merge_sort (List.sort compare xs) (List.sort compare ys) = List.sort compare (xs@ys))


let a = QCheck_runner.run_tests ~verbose:true  [sort_concat_test;]
