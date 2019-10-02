mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

makeTuples xs ys = [(x, y) | x <- xs, y <- ys]

makeTuples50 xs ys = [(x, y) | x <- xs, y <- ys, x < 50, y < 50]

numTuples xs = length xs
