
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