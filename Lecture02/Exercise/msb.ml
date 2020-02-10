let rec msb number = 
    if number = 0
    then 0
    else 1 + msb(number lsr 1);; (* Every iteration returns either 1 or 0, therefore the result is 1 + 0 + 1 + 1 (This is just an example)*)