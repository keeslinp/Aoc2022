app "day1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.File, pf.Path, pf.Task.{ await }]
    provides [main] to pf

updateFirst : List U32, U32 -> List U32
updateFirst = \list, delta ->
    list |> List.set 0 ((List.first list |> Result.withDefault 0) + delta)

expect updateFirst [1, 2, 3] 10  == [11, 2, 3]

walker : List U32, [Delim, Value U32] -> List U32 
walker = \state, row ->
    when row is
        Delim -> state |> List.prepend 0
        Value val -> state |> updateFirst val

expect walker [] Delim == [0]
expect walker [10] (Value 10) == [20]
expect walker [10] Delim == [0, 10]


parseRow : Str -> Result [Delim, Value U32] [InvalidNumStr]
parseRow = \row ->
    trimmed = row |> Str.trim
    when trimmed is
        "" -> Ok Delim
        num -> num |> Str.toU32 |> Result.map Value

expect parseRow "1234  " == Ok (Value 1234)
expect parseRow "  " == Ok Delim


getSums : Str -> Result (List U32) [InvalidNumStr]
getSums = \file ->
    file |> Str.split "\n" |> List.mapTry parseRow |> Result.map \rows -> List.walk rows [] walker

part1 : Str -> Result U32 [InvalidNumStr]
part1 = \file ->
    file |> getSums |> Result.map \x -> (List.max x |> Result.withDefault 0)

exampleInput : Str
exampleInput = "1000\n 2000\n 3000\n \n 4000\n \n 5000\n 6000\n \n 7000\n 8000\n 9000\n \n 10000    \n"

expect part1 exampleInput == Ok 24000

part2 : Str -> Result U32 [InvalidNumStr]
part2 = \file ->
    sums <- file |> getSums |> Result.map
    sums |> List.sortDesc |> List.takeFirst 3 |> List.sum

expect part2 exampleInput == Ok 45000

printResult = \result ->
    when result is
        Ok num -> num |> Num.toStr |> Stdout.line 
        Err InvalidNumStr -> Stdout.line "Invalid line"

main =
    result <- Task.attempt (File.readUtf8 (Path.fromStr "./day1.txt"))
    count = Result.map result \file -> { first: part1 file, second: part2 file }
    when count is
        Ok { first, second } -> 
            _ <- await (printResult first)
            printResult second
        Err e -> when e is
            FileReadUtf8Err _path _ -> Stdout.line "Bad path"
            FileReadErr _path readError -> readError |> File.readErrToStr |> Stdout.line 
