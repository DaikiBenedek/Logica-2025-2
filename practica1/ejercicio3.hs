-- función que calcula las combinaciones de n en k usando la función predefinida product
comb :: Int -> Int -> Int
comb n k = div (product [1 .. n]) (product [1 .. k] * product [1 .. n-k])
