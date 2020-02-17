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
    | [],_ -> lst2
    | _,[] -> lst1
    | elem1::elems1, elem2::elems2 -> if elem1 <= elem2 
                                then elem1:: merge_sort elems1 lst2
                                else elem2:: merge_sort elems2 lst1


let sort_concat_test =
  Test.make ~name:"Split and sort" ~count: 1000
    (pair (list int) (list int))
    (fun (xs,ys) ->  merge_sort (List.sort compare xs) (List.sort compare ys) = List.sort compare (xs@ys))


let _ = QCheck_runner.run_tests ~verbose:true  [sort_concat_test;]


let int64_Back =
  Test.make ~name:"int64 to int and back" ~count: 1000
    (int)
    (fun (i) -> i = (Int64.to_int (Int64.of_int i)) )

let int32_Back =
  Test.make ~name:"int32 to int and back" ~count: 1000
    (int)
    (fun (i) -> i = (Int32.to_int (Int32.of_int i)) )

let intString_Back =
  Test.make ~name:"String to int and back" ~count: 1000
    (int)
    (fun (i) -> i = (int_of_string (string_of_int i)) )


let _ = QCheck_runner.run_tests ~verbose:true  [int64_Back; int32_Back; intString_Back;]

(** Euclid **)
let rec eu_gcd = fun n m ->
    if (m = 0)
    then m
    else 
    if n > m
    then eu_gcd (n-m) m
    else eu_gcd (m-n) n;;


let rec gcd a b = 
    let r = a mod b in
    if r = 0 then b else gcd b r;;



let rec fib n = match n with
    | 0 -> 1
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2);;


(**  Fibonacci
if n < 2 then 1 else
if n = accum then num1 + num2 else
fib2 n (num1 + num2) num1 (accum+1)
**)

(** Dictionary **)

let add_dict d k v = fun k' -> if k = k' then v else d k';;
let empty_dict k = 0;;
let find_dict d k = d k
