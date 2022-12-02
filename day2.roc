app "day2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.File, pf.Path, pf.Task.{ await }]
    provides [main] to pf

parseTheirs : Str -> Result Shape [BadFormat]
parseTheirs = \val ->
    when val is
        "A" -> Ok Rock
        "B" -> Ok Paper
        "C" -> Ok Scissors
        _ -> Err BadFormat

expect parseTheirs "A" == Ok Rock
expect parseTheirs "B" == Ok Paper
expect parseTheirs "C" == Ok Scissors
expect parseTheirs "D" == Err BadFormat

parseOurs : Str -> Result Shape [BadFormat]
parseOurs = \val ->
    when val is
        "X" -> Ok Rock
        "Y" -> Ok Paper
        "Z" -> Ok Scissors
        _ -> Err BadFormat

expect parseOurs "X" == Ok Rock
expect parseOurs "Y" == Ok Paper
expect parseOurs "Z" == Ok Scissors
expect parseOurs "D" == Err BadFormat

Shape : [Rock, Paper, Scissors]

compose : Result Shape [BadFormat], Result Shape [BadFormat] -> Result { them: Shape, us: Shape } [BadFormat]
compose = \theirs, ours ->
    when [theirs, ours] is
        [Ok them, Ok us] -> Ok { them, us }
        _ -> Err BadFormat

parseRow : Str -> Result { them: Shape, us: Shape } [BadFormat]
parseRow = \line ->
    when (line |> Str.trim |> Str.split " ") is 
        [theirs, ours] -> compose (parseTheirs theirs) (parseOurs ours)
        _ -> Err BadFormat

expect parseRow "A X" == Ok { them: Rock, us: Rock }
expect parseRow "B Y" == Ok { them: Paper, us: Paper }
expect parseRow "X A" == Err BadFormat 

pickWinner : { them: Shape, us: Shape } -> { them: Shape, us: Shape, winner: [Us, Them, Draw] }
pickWinner = \{ them, us } ->
    winner = when [them, us] is
        [Rock, Rock] -> Draw
        [Rock, Paper] -> Us
        [Rock, Scissors] -> Them
        [Paper, Rock] -> Them
        [Paper, Paper] -> Draw
        [Paper, Scissors] -> Us
        [Scissors, Rock] -> Us
        [Scissors, Paper] -> Them
        [Scissors, Scissors] -> Draw
        _ -> crash "Unhandled match"
    { them, us, winner}

expect (pickWinner { them: Rock, us: Paper }).winner == Us
expect (pickWinner { them: Rock, us: Rock }).winner == Draw
expect (pickWinner { them: Rock, us: Scissors }).winner == Them

scoreRound : { us: Shape, winner: [Us, Them, Draw] }* -> U32
scoreRound = \{ us, winner } ->
    winnerScore =
        when winner is
            Us -> 6
            Draw -> 3
            Them -> 0
    shapeScore =
        when us is
            Rock -> 1
            Paper -> 2
            Scissors -> 3
    winnerScore + shapeScore

expect scoreRound { us: Paper, winner: Us } == 8
expect scoreRound { us: Rock, winner: Them } == 1
expect scoreRound { us: Scissors, winner: Draw } == 6
    
part1 : Str -> Result U32 [BadFormat]
part1 = \file ->
    file
    |> Str.split "\n"
    |> List.mapTry (\row -> row |> parseRow |> Result.map pickWinner |> Result.map scoreRound)
    |> Result.map \list -> List.sum list

exampleInput = "A Y\nB X\nC Z"

expect part1 exampleInput == Ok 15

losingMove : Shape -> Shape
losingMove = \them ->
    when them is
        Rock -> Scissors
        Paper -> Rock
        Scissors -> Paper
winningMove : Shape -> Shape 
winningMove = \shape -> losingMove (losingMove shape)

parseOursCheat : Shape, Str -> Result Shape [BadFormat]
parseOursCheat = \theirs, ourInput ->
    when ourInput is
        "X" -> Ok (losingMove theirs)
        "Y" -> Ok theirs
        "Z" -> Ok (winningMove theirs)
        _ -> Err BadFormat

expect parseOursCheat Rock "X" == Ok Scissors
expect parseOursCheat Rock "Y" == Ok Rock
expect parseOursCheat Rock "Z" == Ok Paper
expect parseOursCheat Rock "L" == Err BadFormat

parseRowCheating : Str -> Result { them: Shape, us: Shape } [BadFormat]
parseRowCheating = \line ->
    when (line |> Str.trim |> Str.split " ") is 
        [theirs, ours] -> 
            theirShape = parseTheirs theirs
            ourShape = Result.try theirShape \x -> (parseOursCheat x ours)
            compose (parseTheirs theirs) ourShape
        _ -> Err BadFormat
expect parseRowCheating "A X" == Ok { them: Rock, us: Scissors }
expect parseRowCheating "A Y" == Ok { them: Rock, us: Rock }
expect parseRowCheating "A Z" == Ok { them: Rock, us: Paper }
expect parseRowCheating "A D" == Err BadFormat

part2 : Str -> Result U32 [BadFormat]
part2 = \file ->
    file
    |> Str.split "\n"
    |> List.mapTry (\row -> row |> parseRowCheating |> Result.map pickWinner |> Result.map scoreRound)
    |> Result.map \list -> List.sum list

expect part2 exampleInput == Ok 12

printResult = \result ->
    when result is
        Ok num -> num |> Num.toStr |> Stdout.line 
        Err BadFormat -> Stdout.line "Invalid line"

main =
    result <- Task.attempt (File.readUtf8 (Path.fromStr "./day2.txt"))
    count = Result.map result \file -> { first: part1 file, second: part2 file }
    when count is
        Ok { first, second } -> 
            _ <- await (printResult first)
            printResult second
        Err e -> when e is
            FileReadUtf8Err _path _ -> Stdout.line "Bad path"
            FileReadErr _path readError -> readError |> File.readErrToStr |> Stdout.line