

let dictionary = Dict.empty

let dictionary = Dict.add dictionary "foo" 2

let test = Dict.find dictionary "foo";;

print_int test;;