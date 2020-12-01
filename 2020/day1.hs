main = do
    input <- readFile "day1-input.txt"
    let numbers :: [Int]
        numbers = map read (lines input)
    let part1 = head [ x * y
            | x <- numbers
            , y <- numbers
            , x + y == 2020
              ]
    let part2 = head [ x * y * z
            | x <- numbers
            , y <- numbers
            , z <- numbers
            , x + y + z == 2020
            ]
    print (part1, part2)
